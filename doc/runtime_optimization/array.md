# Array の実装と最適化

Array は Ruby プログラムの「ただの並び」であると同時に、`each` / `map` /
`[]` / `<<` が最内ループに現れる型でもある。optcarrot では `Array#<<` と
`Array#[]=` のスライス形が最上位のホットスポットだった
（[`../optcarrot_opt_profile.md`](../optcarrot_opt_profile.md) §3）。

この文書は、Array がメモリ上でどう置かれ、VM と JIT が `[]` / `[]=` / `<<` /
リテラル生成 / 多重代入をどう処理し、どの組み込みメソッドが Ruby で書かれて
いてそれがなぜかをまとめ、最後に CRuby との実装差異を並べる。共通の前提
（RValue の 64 バイト、write barrier、インライン生成器の仕組み）は
[README.md](README.md) にある。

---

## 1. 表現

### 1.1 セルの中身

`RValue` は 64 バイトの `#[repr(C)]` で、8 バイトのヘッダ、8 バイトの
`var_table`（あふれた ivar）、48 バイトのペイロード共用体 `ObjKind` から
なる（`rvalue.rs`）。Array のペイロードは

```rust
pub const ARRAY_INLINE_CAPA: usize = 5;
#[repr(transparent)]
pub struct ArrayInner(SmallVec<[Value; ARRAY_INLINE_CAPA]>);
```

で、**5 要素までは 48 バイトの中に直接置かれ、それを超えるとヒープに
あふれる**（`value/rvalue/array.rs`）。`smallvec` は fork
（`sisshiki1969/rust-smallvec`、`const_generics` 有効）で、fork している理由は
JIT / VM のアセンブリがペイロードを直接アドレッシングできるように
`OFFSET_CAPA` / `OFFSET_INLINE` / `OFFSET_HEAP_PTR` / `OFFSET_HEAP_LEN` を
公開するためである。`rvalue.rs` はこれを `RVALUE_OFFSET_ARY_CAPA` などとして
セル先頭からのオフセットに焼き直す。

**すべての高速経路の前提になる規約**: inline のあいだは `capacity` が
**長さを兼ねる**。`capacity > 5` ならヒープ変種が生きていて、そのときは
`capacity` が確保サイズ、実際の長さはポインタの隣にある。したがって
`cmpq capa, 5` の 1 命令で「inline かヒープか」と「inline なら長さは capa」
の両方が決まる。Struct（`StructInner`）は同じレイアウトを共有していて、
スロットアクセスに同じ定数を使う。

### 1.2 フラグ

frozen はヘッダ `flag` のビット 1（`0b10`）。ビット 0 が live、ビット 2 が
chilled（String 専用）、ビット 3 / 4 / 6 が世代別 GC の OLD / WB_UNPROTECTED /
WB_ARMED、ビット 8..15 が GC の年齢。Array は Hash と違って型別メタデータ
バイト `ty_flags` を使わない。

`NEWBORN_FLAG_MASK` は JIT と共有していて、リテラル複製の機械語がテンプレート
のヘッダ語をこのマスクで落とすので、`dup` / `clone` / リテラル評価はすべて
若く・バリア未装填の状態で始まる。

### 1.3 GC との関係

- mark は要素を順に mark するだけ（`rvalue.rs` の `ObjTy::ARRAY` アーム）。
- **Array は OLD 世代へ昇格できる**（`is_promotable`）。条件は「要素の
  格納が全部バリアされていること」で、インタプリタ側は `Array` ラッパ、
  JIT 側は `array_index_assign` と emit されたバリアがそれを保証する。
- ラッパ `Array`（`#[monoruby_object(write_barrier)]`）の固有メソッドは
  `Deref` 先の `ArrayInner` の同名メソッドを**隠す**ので、`ary.push(v)` /
  `set_index` / `insert` / `fill` / `resize` / `extend` / `replace` /
  `set_index2` は必ず `write_barrier` / `write_barrier_bulk` を通る。削除系
  （`pop` / `remove` / `truncate` / `clear` / `drain`）にバリアは要らない。
- `as_array_mut(&Store)` は frozen 検査を**取得時に 1 回だけ**行い、返った
  ラッパの mutator は再検査しない。

詳細は [`../gc.md`](../gc.md)。

---

## 2. VM の経路

### 2.1 バイトコード

| 命令 | 用途 |
|---|---|
| `Array(dst, callsite)` | リテラル生成。splat 位置は call site が持つ |
| `ArrayConcat { dst, src }` | 長いリテラルの分割結合 |
| `ExpandArray(src, dst, len, rest_pos)` | 多重代入・ブロック引数の分解 |
| `ArrayTEq` / `ArrayAny` | `when *ary`（`===` あり / 真偽のみ） |
| `Index` / `IndexAssign` | 単一・非 splat の添字 |
| `Literal` / `FrozenLiteral` | 定数リテラル（Array は常に `Literal`、§3.6） |

リテラルは `LITERAL_CHUNK_LEN` まで 1 命令、それより長ければ**有界の
チャンク**に分けて `ArrayConcat` で繋ぐ（`bytecodegen/expression.rs::gen_array`）。
一時レジスタの数がリテラル長に比例せず、`ret` が作りかけの配列を持たない。
定数リテラルは `Value::from_const_ast` でテンプレートを 1 個作り
`emit_literal` に渡すが、`FrozenLiteral` になるのは `is_always_frozen` な
クラスだけなので Array は `Literal`、つまり**評価ごとに deep copy**（Ruby の
意味論どおり新しい可変オブジェクト）になる。

実行時の `Array` は `runtime::gen_array`: splat が無ければスタックスロットの
範囲を `Value::array_from_iter` で一気に取り込む。

### 2.2 `[]` / `[]=`

VM スタブ `vm_index` / `vm_index_assign` は二項演算と同じ簿記をしてから
`runtime::get_index` / `set_index` を呼ぶ:

1. `(base_class, idx_class)` をサイトのインラインキャッシュに記録し、多相
   ビットを立てる（`vm_save_binary_class`）。
2. `is_func_call = (base slot == 0)`、すなわちレシーバ無しの `self[i]` を
   区別する。CRuby と同じく **private な `#[]` に届く**ようにするため。

`runtime::get_index` はまず `basic_op_redefined_for(base_class, :[])` を見る。
ディスパッチテーブル型の演算子と違ってこのヘルパ自体が実装なので、
`_no_opt` 版と差し替えるのではなくフラグを直接読む。再定義されていれば
`invoke_method`。`ARRAY_CLASS` アームは、Fixnum でも Range でもない添字が
`Enumerator::ArithmeticSequence` なら `aseq.[](self)` に委ね（スライスの
規則は AS 側にある）、それ以外は `to_int` して `ArrayInner::get_elem1`。
続いて `HASH_CLASS` / `METHOD_CLASS` のアームがあり、他は dispatch。

このヘルパの `is_func_call` が VM のフラグ語ではなく `bool` なのは、型を
`BinaryOpFn` に揃えて JIT が多相サイトの残余アームとして再利用できるように
するためである（§3.2）。

`runtime::set_index` は `Array` + Fixnum 添字 + `[]=` 未再定義なら明示的な
frozen 検査（`FrozenError`）の後 `Array::set_index` を呼び、メソッド探索を
しない。`Hash#[]=` の並行アームは #1245 で追加された
（[hash.md](hash.md) §2）。

### 2.3 添字計算

`value/rvalue/array.rs`:

- `get_array_index(i64) -> Option<usize>`: 負なら折り返し、それでも負なら
  `None`。
- `get_elem1`: Range 添字（beginless / endless / exclusive、負の折り返し、
  `len == start → []` と `len < start → nil` の区別）とスカラ。**Range 形は
  常に新しい配列を確保する**（共有しない、§6）。
- `get_elem2`: `ary[start, len]` 形。
- `set_index`: 末尾より先の正の添字は nil で埋めて伸ばす、範囲外の負は
  `IndexError`。
- `set_index2`: スライス代入の splice。自己代入は先にスナップショットを取り
  （`is_self`）、伸縮方向に応じて `copy_within`。

### 2.4 `ExpandArray` と `case/when`

`runtime::expand_array` はユーザーが上書きした `respond_to?` を尊重してから
`#to_ary` を探し（CRuby 準拠）、素の `BasicObject` はスカラのままにし、
エラーは `None` で返す。`array_teq` は splat 配列を `===` で回し、rescue 版は
「class or module required for rescue clause」を出す。`array_any` は CRuby の
`checkmatch(VM_CHECKMATCH_TYPE_WHEN | ARRAY)` と同じく**真偽だけ**で、`===` も
ユーザーから見える呼び出しも無い。

---

## 3. JIT の経路

### 3.1 登録されているインライン生成器

`builtins/array.rs::init` で `inline_gen2!`（中身は `Box::new`）により登録:

| メソッド | 生成器 | 出るもの |
|---|---|---|
| `size` / `length` | `array_size` | 型付き LIR `ArrayLenFixnum`（アーキ固有クロージャ無し） |
| `clone` / `dup` | `array_clone` / `array_dup_inline` | `array_clone_extern` / `array_dup_extern` への直接 call |
| `<<` | `array_shl` | `guard_frozen` + `emit_array_shl` |
| `[]` / `slice` | `array_index` | `AsmInst::ArrayIndex` |
| `[]=` | `array_index_assign` / `array_slice_assign` | `AsmInst::ArrayIndexAssign` / `emit_array_slice_assign` |
| `rotate!` | `array_rotate_` | `guard_frozen` + `ary_rotate_` への call |

生成器は `compile_method_call` から、class-version ガードとレシーバクラス
ガードを**出した後に**呼ばれる。`proven: Option<ClassId>`（クラス集合ガードや
dispatch アームの陰では `None`）と `arg_class` を受け取り、`false` を返せば
状態と IR が巻き戻される（trial inlining、[`../inline.md`](../inline.md)）。

### 3.2 `[]` / `[]=` のコンパイル経路

`codegen/jitgen/compile/index.rs`。`index()` は次の順で決める:

1. 多相サイトでレシーバクラスが抽象状態に無い → `index_dispatch`。
2. レシーバクラスが証明できない → `Recompile(NotCached)`。
3. `fire_index_inline` — 前置きを省いたインライン。
4. それ以外は通常の `call_binary_method`。

**`fire_index_inline`** は数値演算の直接発火と違って**レシーバクラスガードを
残す**。理由は添字の生成器が自分でレシーバを守るとは限らないからで、
`array_index` の `load_array_ty` は**オブジェクト型**（`ObjTy::ARRAY`）を
証明するだけなので、独自の `#[]` を持つ `Array` サブクラスがすり抜ける。
省くのは **class-version ガード**と `compile_method_call` の残りの前置きで、
再定義は記録した BOP 依存で捕まえる（§3.7）。メソッドが無い・生成器が
無い・`basic_op_assumable` でない・可視性で弾かれる・ブロック無しで
キャプチャしうる・ブロックが渡される、のどれかなら断る。ガードと生成器は
巻き戻し可能な 1 単位として出る。

**`index_dispatch`** は多相サイトへの 2 腕の答え:

```
br_class_ne rdi, C -> slow
<C#[] をインライン>          ; BOP 依存を記録、class-version ガード無し
br merge
slow: <runtime::get_index>   ; どんなレシーバでも正しい。BOP の許可も自分で再確認
merge:
```

要点は **deopt という第 3 の選択肢が無い**こと。optcarrot の
`@fetch[addr][addr]` は Array と Method を交互に取り、ホットループ最大の
deopt 源だった。`index_inline_class` はインラインキャッシュが持つクラス
ではなく「観測されたうち生成器を持つ最頻クラス」を選ぶ — キャッシュには
たまたま `Method` が入っていて、それをインライン化すると RAM 読みの全部が
C 呼び出しに残ってしまうからである。両腕とも `dst` に素の `Value` を残す
ので join 機構は要らない。`index_assign` に dispatch 腕は無い。

### 3.3 `[]` / `[]=` の機械語（x86-64）

`codegen/arch/x86_64/compile/index.rs`。

- **`array_index`**（非負添字の読み）: `movq rax,[rdi+ARY_CAPA]; cmpq rax,5;
  jgt heap`。inline 腕は `capa`（= 長さ）と添字を比べ、`jle out_range` の後
  `movq rax,[rdi + rsi*8 + INLINE]`。heap 腕は `HEAP_LEN` で境界検査してから
  `HEAP_PTR` 経由。`out_range` は **`NIL_VALUE` を返す**（範囲外の `[]` は
  Ruby でも nil なので deopt しない）。cold ブロックはページ 1 に置く。
- **`array_index_assign`**: 同じ常駐分岐だが、write barrier は `rdi` を
  ヒープバッファに付け替える**前**に出す。範囲外の添字はインラインで
  raise せず `generic` → `set_array_integer_index`（正なら伸長、負すぎれば
  `IndexError`）へ落ち、エラーは `handle_error` に戻る。
- **`gen_array_index` / `gen_array_index_assign`**（`ArrayIndexKind` の駆動）:
  `U16` なら添字は `movl` の即値。`Fixnum` なら `sarq rsi,1` で untag し
  `js negative`。negative 腕（ページ 1）は `get_array_length` を呼んで
  `addq rsi,rax; jns checked` — **一度折り返して検査済み経路に再入する**。
  まだ負なら generic / out_range。
- **`get_array_length`** は分岐無し: `cmpq rax,5; cmovgtq rax,[rdi+HEAP_LEN]`。

抽象状態側（`jitgen/state/index.rs`）: 添字がコンパイル時の `u16` リテラル
なら `ArrayIndexKind::U16`、そうでなければ `load_fixnum`（Fixnum ガードを
出す）して `Fixnum`。`load_array_ty` は抽象状態がまだ配列型を証明していない
ときだけ `GuardArrayTy` を出す（`ClassInfo::is_array_ty_instance` なので
サブクラスも通る — これが §3.2 でクラスガードを残す理由）。

aarch64 は `cmov` を `csel` で置き換えた同型で、cold ブロックは同じページに
インライン配置する（`b` / `b.cond` が monoasm の第 2 ページに届かないため）。
`a[-100000]` / `a[100000]` / 添字 100000 への `[]=` 伸長 / `a[-3]=` のエラーは
同ファイルのテストが固定している。

### 3.4 `<<`

`array_shl` は `guard_frozen` を明示的に出す。`emit_array_shl` のインライン
格納は `Array::push` を迂回し、`push` 自体は frozen を見ないので、frozen な
レシーバは JIT を抜けてインタプリタに `FrozenError` を出させる必要がある —
これはインライン化と同時に直した実バグである
（[`../optcarrot_opt_profile.md`](../optcarrot_opt_profile.md) §3.2）。

`emit_array_shl`（`x86_64/compile/builtin.rs`）: `capa <= 5` ⇔ 未あふれ
なので、どちらの常駐でも「2 ロード + 1 ストア」の高速経路になる。
`cmpq rax, 5` が**inline バッファ満杯の判定のフラグも同時に立てている**ので
`jeq grow` はただで付く。満杯のとき — `capacity` 回に 1 回、償却 — だけ
`ary_shl` を呼んで再確保させる（そこでバリアも走る）。最後に
`emit_write_barrier_rdi(Rsi)` と `rax = rdi`（`<<` は self を返す）。

### 3.5 `rotate!` / スライス代入 / `size` / `clone` / `dup`

- **`rotate!`**: `pos_num <= 1`、引数があるときは `arg_class == INTEGER_CLASS`
  のときだけ。`guard_frozen` → 回転数を untag → `ary_rotate_`。
- **`ary[start, len] = other`**（`array_slice_assign`）: 動機は optcarrot の
  `@bg_pixels[@scroll_xfine, 8] = ...` で、支配的な形は「配列の内側の
  run を同じ長さの run で置き換える」、つまりサイズが変わらない素のコピー。
  ゲートは `may_be_fixnum(start)`（意図的に弱い — `ary[ivar, 8]` の start は
  抽象状態が絞っていない ivar から来るので、`load_fixnum` のガードに任せる。
  Float / Range と**証明された**スロットだけ断る）と、`len` が
  `0..=MAX_INLINE_SLICE`（8）の**コンパイル時 Fixnum リテラル**であること
  （コピーの展開回数になる）。`emit_array_slice_assign` は start の untag →
  `js slow`、自己代入（`cmpq rdi, rdx`）→ slow、`rdx` がちょうど `len`
  要素の Array で、レシーバの run がその内側に収まるときだけ `len` 個の
  `mov` 対を展開し、`emit_write_barrier_bulk_rdi`、`rax = rdx`。それ以外は
  `set_array_slice` が builtin と同じ規則（添字正規化、`#to_ary`）で処理。
- **`size` / `length`**: `ir.array_len_fixnum(Rax, Rdi)` の純 LIR。x86 は
  `cmov` + `salq/orq`、aarch64 は `csel`（[`../lir.md`](../lir.md) の
  「container length」族）。
- **`clone` / `dup`**: 証明済みレシーバのときだけ、`fpr_save` + 直接 call。
  `clone` は frozen を伝播し、`dup` は本来のクラスに付け替える意味論を保つ。

### 3.6 リテラルと生成

- **リテラル確保のインライン化**（`TraceIr::Array`）: splat 無しで長さが
  `ARRAY_INLINE_CAPA` 以下（`[]` を含む）なら GC free list からセルを直接
  取る（`new_array_inline` → `emit_alloc_cell`）。ヘッダは
  `class<<32 | ObjTy::ARRAY<<16 | 1` の即値、`var_table = 0`、`ARY_CAPA = len`、
  続けて `len` 個のスロット→フィールド移送。free list が空／ページ境界なら
  `runtime::gen_array` に落ちる。
- **リテラル複製のインライン化**（`DeepCopyLit`）: テンプレートが ivar
  無し・`len <= 5`・**全要素が immediate** なら
  `RValue::inline_copyable_array`。immediate の deep copy は恒等なので語
  コピーで済み、要素は GC ポインタでなくテンプレートも不変なので機械語に
  焼き込める。`emit_deep_copy_lit` は `CellHeader::NewbornOf(template)` で
  ヘッダを `NEWBORN_FLAG_MASK` で落として写す（`Header::newborn` と同じ
  マスクなので乖離しない）。それ以外は `value_deep_copy` 呼び出し。
- **`[a, b].min` / `.max` の融合**（CRuby の `opt_newarray_send`）:
  `try_fuse_array_minmax`。消費する呼び出しが同一基本ブロックの**直後の
  命令**で、レシーバがそのリテラルで、リテラルが**一時スロット**に落ち
  （ローカルは再読される）、splat / 引数 / ブロック無し、`Array#min` /
  `#max` が builtin に解決する（`FuncKind::Builtin { abs_address }` を
  `builtins::array::min/max` と比較）ときだけ。class-version ガードの後
  `ir.array_min_max` を出し、call 命令を `fused_skip` で飛ばす。実行時の
  `opt_array_minmax` は要素を**スタックスロット上で**比較し、Array を
  作らない。同点は先の要素、比較不能は raise、と builtin と同じ。
- **`ExpandArray` の高速経路**: `src` が既に `len` 要素以上の Array なら
  `len` 回の移送で済ませる（`respond_to?` / `#to_ary` の dispatch 無し、
  nil 埋め無し、raise 無し）。`rest_pos` あり・`len == 0`・
  `len > MAX_INLINE_EXPAND`（8）は断る。成功時 `rax = 1`
  （`expand_array` は null 返しでエラーを伝えるため）。呼び出し元が続けて
  出す実行時呼び出しがそのまま遅い経路になる。

### 3.7 BOP（基本演算）の再定義

`globals/store/basic_op.rs` の Array エントリは 3 つだけ:
`(ARRAY_CLASS, "[]")`, `(HASH_CLASS, "[]")`, `(ARRAY_CLASS, "[]=")`。
`runtime::get_index` / `set_index` は素の Rust ヘルパなので、差し替えられる
のではなく `BasicOpTable::redefined` を直接見る。

**`Array#size` / `#length` は BOP ではない**（CRuby には `BOP_SIZE` /
`BOP_LENGTH` がある）。そのインライン生成器は `compile_method_call` から
発火し、そこで既に class-version ガードが出ているので再定義はそちらで
捕まる。JIT 側の許可は `basic_op_assumable`（追跡対象の組・グローバルに
未再定義・コンパイル中スコープで refine されていない）で、`record_bop_dep`
が `(class, op)` を記録し、`set_bop_redefine` がそれを読んで依存本体だけを
evict する。**直接発火経路には class-version ガードが無く、健全性は記録した
依存だけに拠る**。`Hash#[]=` が意図的に表に無いのはこのためで、通常の
class-version ガード付き経路に残す（`index_hash_assign_redefinition` テスト）。
効果は [`../bop_redefinition.md`](../bop_redefinition.md)（`Array#[]` 再定義
時 0.30 → 0.030 → 0.009 s）。

### 3.8 `Array.new` と `Class#new`

`Array.new` は native では定義されていない。Ruby の `Class#new` トランポリン
（`o = __builtin_allocate__; o.__builtin_initialize__(...)`）を継いで、Ruby
で書かれた `Array#initialize`（`builtins/array.rb`、位置引数 0〜2、rest /
キーワード無し）にインライン経路で届く。以前の `__send__` による override は
rest + kwrest 登録だったため JIT の転送高速経路を全部外し、汎用の引数再解析
と rest Array の即時実体化を毎回払っていた（約 8 倍遅い）。さらに public な
`allocate` を dispatch していたのでユーザーの上書きを尊重してしまい、CRuby の
`Array.new` とは違っていた。

`Array#initialize` の重い脚は native の `__init_fill` / `__init_from` /
`__size_to_int`。`#to_ary` の探索は引数が Integer でないときに限る（CRuby の
`!FIXNUM_P`）。ブロック形は要素を逐次 push して `break` 時の部分内容を
CRuby と一致させ、`yield` はユーザーの `Array.new { }` サイトのリテラル
ブロックに対して特殊化される（`resolve_given_block` が `Class#new` の転送
連鎖を辿る）。

アロケータは `array_alloc_func` で `default_alloc_func` ではないため、
`emit_class_allocate` の `InlineAlloc::Object` 列にはならず、`array_alloc_func`
への直接 call になる。クラス記録により後続の `__builtin_initialize__` は
単相になる。

rest 引数 `Array` の遅延実体化（D1）は
[`../arg_forwarding_jit.md`](../arg_forwarding_jit.md) §3.4 にある。
`WriteBack::forward_rest` が `(dst, src, len)` を持ち、deopt 時に
`runtime::create_array` で作る。リテラルの write back が `forward_rest` より
先なのは、実体化が確保を伴い、その時点でフレームが GC 整合でなければならない
からである（[`../jit_invariants.md`](../jit_invariants.md)）。

---

## 4. Ruby で書かれているメソッド

`builtins/array.rb` に `initialize` / `each` / `reverse_each` /
`each_with_index` / `each_index` / `map` / `map!` / `bsearch` /
`bsearch_index` / `dig` / `tally` / `filter_map` / `cycle` / `combination` /
`permutation` / `repeated_*` / `at` / `to_ary` / `deconstruct` / `drop_while` /
`fetch_values` / `rindex` / `assoc` / `rassoc` / `values_at` / `__zip_pull` が
ある。

理由はファイル自身が書いている: ホットな `Array.new` サイトが JIT の
特殊化された引数束縛を得られ、ブロック形では要素ごとの `yield` がインライン
展開される（native 版は要素ごとに native→Ruby のブロック呼び出しを払って
いた）。`each` / `map` の Rust 版は `builtins/array.rs` に残っているが**登録が
コメントアウト**されていて、`while i < self.size` を `Array#size` / `Array#[]`
のインラインで回す Ruby 版が勝つ。

逆に **`Array#sum` は意図的に Ruby で開き直さない**（Rust の Fixnum 高速
経路を迂回してしまうため）。`sample` / `shuffle` / `shuffle!` は `random:`
の解決と `RAND_UPTO` を共有するため Rust のまま。

このトレードオフは [`../bop_redefinition.md`](../bop_redefinition.md) に記録
されている: `Integer#<` を再定義すると monoruby では `Array#map`（の
`while i < size`）が壊れるが、C 実装の CRuby は影響を受けない。
`Array#size` を再定義すると `Kernel#p` の内部が `ArgumentError` になるのも
同種で、BOP フックの欠落ではなく「組み込みが Ruby で書かれていることの露出」
である。

---

## 5. 個別メソッドの注記

- **`Array#sum`**: ブロック無し・整数の初期値・全要素 Fixnum なら
  `checked_add` の高速経路（約 30 倍）、外れたら元の `sum` で汎用ループへ。
  Float が続くあいだは **Kahan–Neumaier 補償加算**（CRuby と同じ精度）。
  補償項は全 Float 経路を抜けるときと最後に畳み、非有限になったら更新
  しない（`Inf - Inf` の偽 NaN を避ける）。配列はイテレータではなく毎回
  添字で読み直すので、ブロックがレシーバを伸ばしても CRuby と同じく耐える。
- **`sort` / `sort!` / `min` / `max` / `minmax`**（`executor/op/sort.rs`）:
  20 要素以下は挿入ソート（マージバッファ無し）。マージ中のバッファは
  一方の run の**唯一の参照**を持ち、比較子は任意の Ruby を走らせるので、
  Rust の `Vec<Value>` では GC に見えない — 実際にあったバグで、呼び出し側
  は nil 埋めの `Array` を temp スタックに置いてバッファにする。全 Fixnum /
  全 String / 全（非 NaN）Float のスライスは `homogeneous_ord` で
  **Rust の中だけで**ソートし、クラスごとに別の `sort_unstable_by*` を呼ぶ
  （キー抽出が単相化して inline される）。Ruby の sort は安定でないので
  unstable で良い。許可は `BASIC_OP_DEFS` + `cmp_redefined`。`cmpint` は
  `rb_cmpint` に従い、Bignum は符号を直接読み、非 Integer は `<=> 0` でなく
  `> 0` と `< 0` を尋ねる。`min(n)` / `max(n)` は部分選択ではなく rooted な
  コピーの 1 回ソートを使う。
- **`hash` / `eql?`**: 外側再帰は `exec_recursive_outer_unregistered` で
  畳み、循環構造はすべて同じ番兵にハッシュする（`rec.hash == [{x: rec}].hash`
  が `eql?` と整合する）。`Store::has_builtin_container_hash` が「この
  Array / Hash はまだ builtin の `:hash` に解決するか」を class version で
  キャッシュしていて、真なら `Value::ruby_hash` が構造ダイジェストを
  **native で**計算する。singleton / mock / monkey-patch があれば dispatch。
- **`include?` / `index` / `count` / `delete`**（`EqSearch`、`executor/op.rs`）:
  needle を 1 回準備してから走査する。Integer / Symbol / nil / true / false
  の needle なら同クラスでビットが違う要素は `==` 無しで不一致、String は
  内容で決まる。`basic_op_redefined_for(class, :==)` でゲート。dispatch に
  達する要素には `(ClassId, class_version)` キーの**単相キャッシュ**
  `cached_eq` があり、独自 `==` を持たないクラスは `Identity` アームで
  呼び出し自体を省く。以前は**引数**に `==` を尋ねて hoist していて、
  レシーバが違うバグだった。
- **`-` / `&` / `intersection` / `uniq`**: `RubySet` を作る。`insert` が
  Ruby を走らせうるので、格納ポインタを temp スタックで root する。
- **`flatten`**: object id の `seen` で循環検出。要素の変換探索
  `try_convert_to_array` は `rb_check_funcall` の 4 段（ユーザーの
  `respond_to?` → `to_ary` → `respond_to_missing?` → `method_missing`）を
  ruby/spec どおりに踏む。
- **`flat_map`**: 蓄積先は Rust の `Vec` ではなく **rooted な Ruby Array**
  （`vm.temp_array_*`）。`invoke_block` はすべて safepoint だからで、
  gc-stress の optcarrot が落ちた事例が [`../gc.md`](../gc.md) にある。
- **`pack`**: `string/pack.rs` の CRuby `pack.c` 忠実移植で、実行時の
  高速経路は特に無い。
- **`shuffle` / `sample`**: CRuby の `RAND_UPTO`（`rb_random_ulong_limited`）
  をそのまま使うので、`srand(n)` 後の列が**1 回の draw 単位で CRuby と
  一致**する。

---

## 6. CRuby との実装差異

| 項目 | CRuby | monoruby |
|---|---|---|
| 埋め込み／共有 | `RARRAY_EMBED`（埋め込み）と `ary_make_shared` による共有配列 + copy-on-write の 2 段 | **共有配列は無い**。`SmallVec<[Value; 5]>` の 1 機構: 5 要素まで 48 バイトのペイロードに直置き、超えたらヒープ。`get_elem1` / `get_elem2` の Range 形は毎回新しい配列を確保する。String には隠れ frozen root による共有部分文字列があるので（[string.md](string.md)）、Array に無いのは意図的な選択 |
| 基本演算の追跡 | `BOP_SIZE` / `BOP_LENGTH` / `BOP_AREF` / `BOP_ASET` … | `Array#[]` / `Array#[]=` / `Hash#[]` のみ。`size` は class-version ガードで守る。`Hash#[]=` は意図的に外す |
| 組み込みの実装言語 | C（`Integer#<` を再定義しても `Array#map` は無傷） | `each` / `map` / `initialize` などが Ruby。`Integer#<` や `Array#size` の再定義が組み込みの内部に及ぶ（意図したトレードオフ、§4） |
| `Array.new` と `allocate` | ユーザー定義の `self.allocate` を迂回 | 同じ。private な `__builtin_allocate__` 経由で実現 |
| 範囲外の `[]` | nil | JIT コード内で nil（deopt 無し）。負添字も生成コードで折り返す。範囲外の**書き込み**は必ず高速経路を抜ける（伸長・`IndexError` のため） |
| `ArithmeticSequence` 添字 | `rb_ary_aref1` 内で処理 | `aseq.[](self)` に委譲（3 箇所とも）。特異クラス付きの AS（`Range#%` 由来）も `ObjTy` で判定するので届く |
| `[]=` の Range 開始 | 折り返しても負なら `RangeError` | 同じ。素の負 Integer 添字は `IndexError` |
| `Array#<=>` | 最初に `0` でなかった要素比較の結果をそのまま返す | 同じ（nil / ±1 / String もそのまま） |
| リテラル | `duparray` / `newarray`、`opt_newarray_send` | `Literal`（テンプレート deep copy）。要素が全部 immediate で 5 以下なら複製を機械語に焼く。`[a,b].min/max` 融合あり |
| `Array.new` の `#to_ary` 探索 | `!FIXNUM_P` で判定。Bignum は探索する | Integer クラスが 1 つなので Bignum も探索を飛ばし、`__init_fill` のサイズ検査が CRuby の `NUM2LONG` + 「array size too big」の順で拒む。`MAX_ARRAY_SIZE = 1 << 30` |
| `zip` の引き出し | `take_items`（`each` + `break`） | 同じ。`Enumerator#next` の fiber は使わないので、引数の `each` からの `StopIteration` は呼び出し元に届く |
| 要素格納のバリア | `RARRAY_ASET` が `RB_OBJ_WRITE` | `Array` ラッパが `ArrayInner` の mutator を隠して全経路をバリア。JIT は `emit_write_barrier_rdi` / `_bulk` |

---

## 7. テストと関連文書

| 場所 | 内容 |
|---|---|
| `codegen/arch/x86_64/compile/index.rs` のテスト | 添字境界（±100000、伸長、`a[-3]=` のエラー） |
| `codegen/jitgen/compile/index.rs` のテスト | `index_redefinition_evicts` / `index_polymorphic_array_and_method` / `index_array_subclass_override` / `index_private_receiver` / `index_hash_assign_redefinition` |
| `builtins/array.rs` の `array_literal_minmax_fusion` | `[a, b].min` 融合 |
| [`../optcarrot_opt_profile.md`](../optcarrot_opt_profile.md) | `<<` / `rotate!` / `expand_array` / スライス代入の計測と経緯 |
| [`../bop_redefinition.md`](../bop_redefinition.md) | BOP の差分掃討（48 ケース）、Ruby 実装組み込みの露出 |
| [`../lir.md`](../lir.md) | `GuardArrayTy` / `GuardFrozen` / `ArrayLenFixnum` の LIR |
| [`../arg_forwarding_jit.md`](../arg_forwarding_jit.md) | rest Array の遅延実体化（D1） |
| [`../gc.md`](../gc.md) | バリア、temp スタックの root、`flat_map` の事例 |
| [`../progress_2025-2026.md`](../progress_2025-2026.md) | `Array#[]` / `#[]=` のインライン化（#269）などの変更履歴 |
