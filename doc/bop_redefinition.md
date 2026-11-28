# 基本演算の再定義（BOP redefinition）

`1 + 2` のような演算子式を、メソッド探索を挟まずインライン展開してよいのは
「`Integer#+` が再定義されていない」という**仮定**が成り立つ間だけである。Ruby
はその仮定をいつでも壊せる（`class Integer; def +(o); …; end; end`）ので、
処理系は「仮定を置く」「壊れたことを検知する」「壊れたあと辻褄を合わせる」の
3 点を用意しなければならない。

本書は monoruby の現行の仕組みをコードに即して記述し、CRuby の戦略と比べる。
粒度はどちらも **(演算子, クラス) の組**で、再定義の影響はその組を仮定した
コードだけに及ぶ。

関連: `doc/refinements.md` §6.7（refinements と基本演算）、
`doc/chain_deopt.md`（実行中フレームの変換）、`doc/jit_invariants.md`、
`doc/inline.md`

---

## 1. 基本演算の表

`globals/store/basic_op.rs` の静的表 `BASIC_OP_DEFS` が、fast path が
仮定してよい `(クラス, メソッド名)` の組を列挙する。CRuby の
`vm_opt_method_defs` と同型である。`BasicOpTable` はこれを intern したもので、
次を持つ。

| フィールド | 意味 |
| --- | --- |
| `set` / `names` | 表の組と、その名前だけの集合（「この定義は演算子に触れるか」の安い一次判定） |
| `armed` | ブートストラップ用ラッチ。ビルトイン自身の定義がこの表そのものなので、`builtins/*.rb` と gem のロードが終わるまで報告しない（`Store::arm_basic_ops`） |
| `redefined` | どれか 1 組でも置き換わったか。Rust 側の fast path はまずこれを読むので、何も再定義しないプログラムのコストはヘルパ呼び出しごとに bool 1 個 |
| `redefined_set` | 置き換わった組（下の 2 つの和集合）。実行時ガードが知りたいのはこれ |
| `globally_redefined_set` / `refined_set` | 内訳。グローバルな再定義か、refinement だけによる無効化か（§5） |
| `armed_funcs` | arm 時点での各組の解決先 `FuncId`（mixin 検知と JIT の解決に使う） |
| `pending_warnings` | `:performance` 警告待ちの組 |
| `case_dispatch_off` | `case` が値比較で分岐してよいクラスの `===` が置き換わったか（CRuby の `opt_case_dispatch` と同じく、立ったら戻らない） |

判定キーが「上書きされたエントリ」ではなく「(クラス, メソッド) の組」で
あることが要点である。`Integer#!` / `#+@` / `#~`、`NilClass#==` などは
そのクラスにエントリを持たない（`Object` / `BasicObject` からの継承）。
`class Integer; def !; …` は上書きではなく**挿入**なので、エントリに印を
付ける方式では原理的に検知できない。

## 2. 検知

メソッド表を変更する経路すべてに検知点がある。

- **`insert_method` / `remove_method`** — `def` / `define_method` /
  `alias` / `import_methods` / `undef_method`（`add_empty_method` 経由）を
  1 箇所で覆う。refinement のメソッドは refinement モジュール側の
  `ClassId` に入るので、`refined_class()` を引いて `(refined_class, name)`
  で照会する（§5）。
- **mixin** — モジュールに後からメソッドが入った場合は `insert_method` →
  `check_mixed_in_basic_op(module_id, name)`、すでにメソッドを持つモジュール
  が後から差し込まれた場合は `append_features` / `prepend_features` →
  `check_mixin_basic_ops(module)`。

mixin で問うのは「モジュールがその名前を定義しているか」ではなく
「**そのクラスは今もビルトインに解決するか**」である。`Comparable` は
`<` `<=` `>` `>=` `==` を定義し、`Integer` はブートストラップ以来それを
include しているので、前者は `include Comparable` のたびに真になる。
mixin 検知は `armed_funcs` と現在の解決先を比べるので、`Integer.prepend M`
（M が `+` を持つ）なら立ち、`class Foo; include Comparable; end` なら
立たない。回帰テストは `basic_op.rs` の
`a_mixin_that_displaces_nothing_keeps_the_basic_ops` /
`a_prepend_that_displaces_the_builtin_retires_its_pair` で、値ではなく表の
状態を直接確かめる（fast path が消えても答えは正しいので、値の比較では
検出できない）。

置き換わった組は `Store::set_bop_redefine` が記録し、`pending_warnings` に
積む。警告（`Redefining 'Integer#+' disables interpreter and JIT
optimizations`）は `Store` の中では出せないので、`Executor::invoke_method_added`
で出す。

## 3. VM 側の反応

VM の asm fast path はすべて fixnum 限定である。`vm_binops_opt` /
`vm_cmp_opt!` / `vm_neg` … はどれも fixnum ガードで始まり、fixnum でない
オペランドは Rust ヘルパ（`add_values`、`cmp_lt_values`、`eq_values_vis`、
`not_value`、`get_index` …）に落ちる。そこで反応は 2 段になっている。

- **asm 側（Integer のみ）** — `Codegen::bop_flags` は JIT データ領域の 1 語で、
  `VmBop` ごとに 1 ビットを持つ。asm の inline 計算を持つ 11 個
  （`+ - == != < <= > >= === +@ -@`）の VM ハンドラは、自分の fixnum ガードの
  直後で自分のビットを読み（x86-64 `testq [rip+f], (1<<bit)`、aarch64
  `ldr x9` / `tbnz #bit`）、立っていれば Rust ヘルパへ抜ける。CRuby
  インタプリタの `BASIC_OP_UNREDEFINED_P` と同じ配置である。`~` と `!` は
  inline 計算を持たず、残りの二項演算（`* / % ** << >> & | ^`）は最初から
  Rust ヘルパへ直行する。通常の実行ではこの検査のコストは測定にかからない。
- **Rust ヘルパ側（全クラス）** — ネイティブに答えるアームに入る前に
  `basic_op_redefined_for(受け手のクラス, 演算子名)` を確かめ、置き換わって
  いれば本物のディスパッチに戻る（`Executor::dispatch_redefined_op`）。
  ディスパッチ表を持たない Rust 側の fast path（`runtime::{get_index,
  set_index}` の `Array#[]` / `Hash#[]` / `Array#[]=`）も同じ表を読む。

ガードは「探索を飛ばした呼び出し元のための入口」`*_values` にだけ置き、
実装本体は無防備な `*_values_raw` に分けてある。ビルトインの `Integer#+` の
本体（`numeric.rs` の `binop!` / `unop!`、`integer.rs` の `cmpop!` と
`% ** << >>`、`float.rs` の比較と `==`）は `_raw` を呼ぶ。ガードを実装側に
置くと、次のコードで `__orig`（= 元のビルトイン）に入った時点でガードが再び
発火し、名前 `+` で再ディスパッチして無限再帰になる。CRuby が
`BASIC_OP_UNREDEFINED_P` を命令側（`opt_plus`）に置き `rb_int_plus` には
置かないのと同じ理由である。

```ruby
class Integer; alias __orig :+; def +(o); __orig(o); end; end
1 + 2
```

回帰テストは `aliasing_an_operator_before_redefining_it_does_not_recurse`。
`Array#sum` や `Numeric#angle` のように探索せずに演算子を使う内部呼び出しは
ガード付きの入口を使う（CRuby も同じ位置で BOP を見る）。

## 4. JIT 側の反応

JIT がガード無しで基本演算を仮定するのは、整数・浮動小数の算術／比較／
定数畳み込みと単項演算だけである（`state/binop.rs` の `binop_integer` /
`binop_float` / `gen_cmp_*` / `unop_integer_*` / `unop_float`）。メソッド
呼び出しに落ちるものはすべて class-version ガードで守られている
（再定義は必ず class version を進める）。

1. **仮定と記録** — inline 生成器の入口（`fire_binary_inline` /
   `fire_unary_inline`、融合 compare-and-branch も同じ）で
   `JitContext::basic_op_assumable(class, op)` を問い、生成器が成功した
   ときだけ `record_bop_dep` で `(class, op)` を記録する。解決には生の
   メソッド表ではなく arm 時のスナップショット（`Store::basic_op_armed_func`、
   `JitContext::resolve_basic_op`）を使う（§6）。置き換わった組は仮定せず、
   その演算だけ通常のメソッド呼び出しになるので、再定義後の再コンパイルは
   健全で、他の演算子の inline は保たれる。
2. **依存の保存** — 記録は `jit_compile` から `ISeqInfo::bop_deps` に
   入る（self class をまたいで union）。
3. **無効化** — `set_bop_redefine` は `Store::evict_jit_code_for_bop(class,
   name)` で、その組に依存した iseq（`depends_on_bop`）**だけ**のコードを
   捨てる（`evict_jit_code`）。捨てたメソッドは `Codegen::rearm_jit_stub` が
   wrapper のウォームアップ stub を戻してカウンタを `COUNT_START_COMPILE` に
   再装填するので、次のウォームアップで再コンパイルされる。ループ本体は
   `clear_loop_jit_entries` がバイトコード内 `LoopStart` の `[pc+8]` を
   クリアする。このオペランドは三値（`0` = 未コンパイル、`1` = コンパイルが
   bail した恒久センチネル、それ以外が実エントリ）で、クリアするのは実エントリ
   だけである。恒久的に bail した本体は `invalidate_jit_code` で表し、
   回復可能な `evict_jit_code` とは区別する。
4. **実行中のフレーム** — 捨てるだけでは、いまスタックで動いている JIT
   フレームが古いコードを実行し続ける。`set_bop_redefine` が
   `bop_eviction_pending` を立て、直後の `Codegen::check_bop_redefine` が
   それを消費して（エッジトリガ）、`chain_deopt_into` で全スレッドの
   スタック上の JIT フレームをインタプリタフレームへ変換する（各フレームの
   write-back を再生し、返りアドレススロットを共有 VM continuation stub に
   書き換える。`doc/chain_deopt.md` §10）。他スレッドのフレームと、
   セーフポイント poll で止まっているフレームの扱いは
   `doc/chain_deopt.md` §10.1。
5. **`def` 地点** — `AsmInst::CheckBOP` は JIT データ語のカウンタを
   コンパイル時の値と比べる（class-version ガードと同じ世代比較）。
6. **Ruby を走らせうるヘルパの直後** — 汎用演算子ヘルパ（多相な `==` など）、
   文字列・正規表現の式展開（`#to_s`）、Hash リテラル（`#hash`）、Range
   リテラル（`#<=>`）、splat（`#to_a`）、多重代入（`#to_ary`）、`when *ary`、
   再定義された `Symbol#to_proc` の `&:sym` は、フレームを積まないヘルパの中で
   Ruby を実行する。そこで再定義が起きても、ヘルパを呼んだフレームには
   返りアドレスを保持する callee が無いので、4. の walk では変換されない。
   これらの直後にも `CheckBOP` を置き（`JitContext::check_bop_after_helper`）、
   出口は結果を書き戻して次の命令から VM で再開する。カウンタは無関係な
   再定義でも進むので、出口は数回で再コンパイルする
   （`RecompileReason::BopRedefined`）。依存していない本体は新しいカウンタで
   作り直され、以後は抜けない。splat と多重代入は Array をインラインで
   扱う速い経路を持つので、比較はランタイム呼び出しの側にだけ置く
   （`AsmInst::ToA` / `ExpandArray` の `bop`）。回帰テストは
   `tests/helper_bop_redefinition.rs`。

`fib(29)`（release、x86-64）で測ると、`fib` が使わない演算子
（`Float#+`、`Integer#~` / `#|` / `#*`、`Array#[]`）を再定義しても実行時間は
再定義なしと同じ（0.009 s）で、CRuby（0.051 s）でも同様に影響が無い。
`fib` 自身が使う演算子を再定義した場合は、その演算だけがメソッド呼び出しになる。

## 5. refinements

refinement による無効化は `refined_set` に入り、`globally_redefined_set` とは
分けて記録する。JIT は 2 段で問う（`JitContext::assume_basic_op`）。

1. グローバルな再定義なら、どのスコープも逃れられないので inline しない。
2. refinement 由来なら、**コンパイル中のスコープの refinement set が実際に
   その組を解決し直すか**だけを問う（`basic_op_refined_in_scope`）。refine
   していないスコープ（refine しないプログラムの全スコープを含む）は inline
   を維持する。

判定は「set のエントリのうち当該クラスを refine するものを辿り、その
refinement モジュール（と ancestors）が当該名を持つか」で行い、クラス表しか
触らない（コンパイル中は `CODEGEN` を `borrow_mut` しているので、メソッド
キャッシュを経由する解決は使えない）。判定は意図的に広めに倒してある。迷ったら
実呼び出しにするのは常に健全である。VM の asm ガードは呼び出し地点の文脈を
持たないグローバル語なので粗いままで、正しさはディスパッチが担保する。

呼び出しサイトの解決は「いま実行中のフレームが書いたスコープ」
（`Executor::site_refinements`）で行い、ネイティブフレームと Ruby 製コアの
フレームは常に refinement なしになる（`doc/refinements.md` §6.10）。

## 6. Ruby で書いたビルトインと再定義

monoruby はコアの一部を Ruby で書いている（`builtins/*.rb`）。CRuby では
C なので再定義の届かないコードが、ここでは届きうる。

```ruby
class Integer; def <(o); :OV; end; end
[1, 2, 3].map { |x| x * 2 }
#   CRuby => [2, 4, 6]  (Array#map は C)
```

`builtins/*.rb` 由来のフレームには `Meta::set_internal_builtin` の 1 ビットが
立っており（`$~` / `$_` を持たせないためと、refinements を届かせないため）、
再定義もそこに届かせない。読む場所は 3 つである。

| 場所 | 役割 |
| --- | --- |
| `Executor::dispatch_redefined_op` | VM ヘルパのガード。内部ビルトインのフレームからなら `_raw` へ落とす |
| `JitContext::resolve_basic_op` | JIT の fast path 解決。生のメソッド表ではなく arm 時のスナップショット（`Store::basic_op_armed_func`）を引く |
| `JitContext::basic_op_assumable` | 上で解決したものをガード無しで inline してよい、という licence |

CRuby 自身にも Ruby で書かれたコアメソッドがあり、`Integer#times` は
`<internal:numeric>` のループで `succ` と `<` を呼ぶ。そのため CRuby では
`Integer#<` を再定義すると `5.times` が `[]` を返すが、monoruby の
`Integer#times` はこの再定義の影響を受けない。CRuby のこの瑕疵は再現しない。

フレーム単位の束縛は「このファイルの演算子は全部ビルトイン束縛」としか
言えず、ライブラリ自身の帳簿（`i += 1`）と呼び出し側のための演算（`sort` の
`<=>`）を区別できない。区別は手作業で保っている（`EqSearch::new` と
`op/sort.rs` は意図的に束縛の外）。より細かい手段として、`__add` / `__lt` の
ような再定義されない内部呼び出しを bytecodegen が通常の `BinOp` 命令に落とす
方式（intrinsic）がありうるが、`builtins/*.rb` の演算子約 1,000 箇所の多くは
呼び出し側の値を比較しており、intrinsic にしてはいけない。フレーム単位の
束縛を既定の安全側とし、実害が出た箇所から漸進的に intrinsic にする方針を
とる（#1362 のレビューで決定）。

## 7. `!=`

CRuby にはどのクラスにも固有の `!=` がなく、あるのは `!(a == b)` を計算する
`BasicObject#!=`（`rb_obj_not_equal`）だけである。monoruby も同じで、
高速パスは次の形で持つ。

- 表には `(Integer|Float|String, "!=")` を**継承ペア**として置く
  （`Integer#!` と同じく、クラス自身にエントリは無い）。このペアが許可する
  のは「そのクラスに対する `BasicObject#!=` のインライン形」、つまり VM の
  fixnum 比較と JIT の `object::object_ne` である。
- インライン形はビルトインの `==` を前提にするので、そのクラスの `==` の
  グローバルな再定義は `!=` のペアも一緒に落とす（`Store::set_bop_redefine`）。
  refinement による `==` の差し替えは連動させない。`rb_obj_not_equal` は
  `==` を `rb_funcall` で呼ぶので、refinement された `==` は見えないからである
  （実行時は `Executor::eq_values_funcall`、JIT は `JitContext::funcall_eq`）。
- `object_ne` は受け手のクラスで分岐する。`==` が `BasicObject#==` /
  `Symbol#==` ならワード比較、`Integer#==` / `Float#==` なら数値比較、
  `String#==` なら定数との比較（`string_ne_gen`）。クラスバージョンガード
  付きの経路から入った場合は `==` の解決先を salvage 用に記録する
  （`JitContext::assume_funcall_eq`）。megamorphic なサイトの即値ビット比較
  （`opt_eq_assumable`）は、`!=` のペアを持たない `Symbol` / `NilClass` /
  `TrueClass` / `FalseClass` について `!=` が `BasicObject#!=` に解決する
  ことを確かめて記録する。
- `:performance` 警告は `!=` のペアには出さない（CRuby に `!=` の basic op は
  無く、`opt_neq` は `BOP_EQ` に乗る）。

インライン形を持たない `==`（`Struct` / `Array` / `Hash` / `Rational` …）は
`BasicObject#!=` の本体が呼ぶ。クラスごとのメモ（`ClassInfo::eq_method_at`、
`Store::funcall_eq_entry`）が `==` の解決先を持ち、それが引数 1 個の
ネイティブメソッドなら、`==` を `!=` 自身のフレームで実行する
（`Executor::default_ne_in_frame`、`Lfp::become_frame_of`）。実行中フレームの
`Meta` は `==` のものに替え、戻ったら `!=` のものに戻すので、`==` の中から
出た例外のバックトレースは CRuby と同じ `Struct#==` → `BasicObject#!=` の
2 行になる。

## 8. `hash` / `eql?` は基本演算にしない

`Hash` / `Set` のルックアップは `hash` → `eql?` と連鎖するが、これらを表に
入れても速くならない。表は「この仮定を置いてよい」という許可と検知であって、
速度は仮定を使う fast path が出す。そして基本クラスのキーについては、
`Value::hash_value` / `Value::eql`（`value.rs`）が `ObjTy` で分岐して Rust
側で直接計算する fast path が既にあり、無条件に効いている。表に入れると
`set_bop_redefine` が JIT まで巻き込むトリップワイヤが増えるだけである。

`hash` を定義していない素のオブジェクトについては、レシーバのクラスにおける
`hash` の解決先がまだビルトインの `Kernel#hash` かを `FuncId` で比べ
（`Store::check_method`、class-version キーのキャッシュ付き）、そうなら同じ
digest を直接計算する。表は静的な `(クラス, 名前)` なので、ユーザ定義クラスが
自前の `hash` を持つ場合を表では捕まえられない。CRuby も `rb_any_hash` で
同じ特別扱いをしている。

## 9. CRuby の戦略との比較

- **`ruby_vm_redefined_flag[BOP_xxx]`** — BOP ごとに 1 ワード、その中のビットが
  クラス（`INTEGER_REDEFINED_OP_FLAG`、`FLOAT_…` …）。`vm_opt_method_defs`
  （`(クラス, メソッド名) → BOP`）を `rb_vm_check_redefinition_opt_method` が
  メソッドの追加・削除のたびに引く。
- **インタプリタ** — `opt_plus` 等の中で、オペランドの型チェックの後に
  `BASIC_OP_UNREDEFINED_P(BOP_PLUS, INTEGER_REDEFINED_OP_FLAG)` を毎回検査する。
- **YJIT / ZJIT** — `assume_bop_not_redefined()` で「その (class, bop) は
  未再定義」という invariant にコードブロックを登録し、再定義時はその
  invariant に依存するブロックだけを無効化する。
- **通知** — `-W:performance` で警告を出す。

| 軸 | CRuby | monoruby |
| --- | --- | --- |
| 表 | `vm_opt_method_defs` | `BASIC_OP_DEFS` |
| VM の検査 | fast path 内のフラグ読み（BOP ごとの語 × クラスビット） | asm は Integer の演算子ごとのビット、Rust ヘルパは組の集合 |
| JIT | invariant + 依存ブロックの無効化 | `bop_deps` + 依存 iseq の evict、実行中フレームは chain deopt |
| refinements | — | スコープ単位（§5） |
| 通知 | `-W:performance` | 同じ警告 |

JIT に CRuby インタプリタ式の実行時チェックを入れる方式は採らない。いま
効いている定数畳み込みとレジスタ常駐が成立しなくなるためである。

## 10. 変更するときの確認

1. **コード生成の同一性** — BOP 再定義を含まないワークロードで
   `--features emit-asm` の出力が変更前とバイト単位で一致すること。
2. **差分テスト** — 演算子の再定義スイープ（再定義 × プローブ）と alias
   スイープの結果が CRuby と一致すること。
3. **性能** — `fib` / optcarrot が baseline を維持し、無関係な演算子の
   再定義で `fib` が遅くならないこと。
