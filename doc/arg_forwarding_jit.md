# Argument Forwarding (`def f(a, ...) g(...) end`) の最適化

本書は argument forwarding（`...`）の最適化について、設計方針と実装済みの
機構を現行コードに紐づけて記述する設計記録である。原則・段階分け・deopt
安全性を中心にまとめる。

行番号は変動しやすいため、参照は原則としてファイル名＋関数名で示す。

## 1. 素朴に実装した場合のコスト構造

`...` は prism から `ParamKind::Forwarding`（`ast/node.rs`）として取り込まれ、
`globals/store.rs` の `ParamKind::Forwarding` アームで

- 合成 **rest** スロット
- 合成 **kw_rest** スロット（`SlotId(1 + args_names.len())`）
- 匿名 **block** パラメータ

へ脱糖される（`ParamsInfo::forwarding = true`、`globals/store/iseq.rs`）。

`g(...)` 呼び出しは `bytecodegen/method_call/arguments.rs` の `handle_forward`
が、

- `splat_pos` に mother の rest を指す位置（純転送は `(pos_start, 1, vec![0])`、
  先頭引数つきは `splat_pos.push(len)` で末尾）、
- `hash_splat_pos = [kw_rest]`、
- `BlockArgProxy`（`bytecodegen/inst.rs`、エンコードは `bytecodegen/encode.rs`）

を持つ `CallSite { forwarding: true }` を生成する。

素朴な実装の実行時コストは 2 箇所:

1. **caller → `f`**: `set_callee_frame_arguments`（`codegen/runtime/args.rs`）。
   `a` を超える位置引数を rest **Array** に確保し、余剰 keyword を kw_rest
   **Hash** に確保する（`fill_positional_args`）。
2. **`f` → `g(...)`**: `is_simple_call`（`globals/store/function.rs`）が
   `has_splat()` により偽 → JIT は specialize 不可で `AsmInst::SetArguments`
   → `jit_generic_set_arguments` の汎用パス（`CallSiteInfo` の実行時再解釈）
   へ落ちる。

## 2. 核心的観察 — forwarding は不透明パイプ

Ruby では `...` は名前を持てず、`f` のコードから rest/kw_rest/block を観測
する手段が一切ない。唯一の読み手は `handle_forward` が生成した転送先
callsite と `BlockArgProxy` だけ。したがって `f` が確保する rest Array /
kw Hash は次を**除き** Ruby から決して観測されない:

- `f` がインタプリタへ **deopt**（呼出規約上 rest/kw_rest スロットに実体を期待）
- フレームが capture される（`binding`、`eval`、外側 proc、zsuper 等）

これは JIT が float を XMM に保持し deopt 時のみ stack へ書き戻す
**WriteBack**（`codegen/jitgen.rs::gen_write_back_for_deopt`、`doc/jit.md`）と
同型の「遅延実体化（lazy materialization）」問題である。実装はこの観察を
両ティアで別々に具体化している:

- **JIT ティア** — D1/K1 転送遅延（§3.4）。specialize されたトランポリンの
  rest/kwrest 確保を省き、呼び出し元フレームのスロット窓から直接読む。
- **VM/汎用ティア** — lazy `(...)` 呼出規約（§3.6）。rest スロットに
  `Fixnum(callid)` マーカーを置き、転送時に元 caller のスロットを直読みする。

## 3. 実装状況

| # | 内容 | 状態 |
|---|---|---|
| 1 | `f→g` の specialize（eager: 実 Array に対する長さガード + インライン充填） | 実装済み |
| 1.5 | opt/post/rest 持ち callee 向け専用 runtime ヘルパ | 実装済み |
| 2 | mixed 経路の `Vec` 排除（`SmallVec` 化） | 実装済み |
| 3 | D1/K1: `f` 側 rest Array / kw Hash の確保省略（deopt 実体化つき） | 実装済み |
| 4 | `super` 暗黙転送（単一 splat 任意位置） | 実装済み |
| 5 | VM/汎用ティアの lazy `(...)` 呼出規約 | 実装済み |
| 6 | 転送を跨いだ trivial fold / frameless 展開 | 実装済み |

`f → g` の引数設定は `codegen/jitgen/compile/method_call.rs::set_arguments`
の 4 段階層に集約されている:

| 段 | 条件 | 生成 AsmInst | コスト |
|---|---|---|---|
| A | D1 source-routed（§3.4 が発火） | `SetArgumentsForwarded { deferred_src: Some(..) }` | ガード・フォールバック無し、確保ゼロ |
| B | eager（`g` は required(+opt) のみ、末尾単一 splat） | `SetArgumentsForwarded { deferred_src: None }` | 長さガード + miss 時フォールバック |
| C | `g` が opt/post/rest 持ち、単一 splat が任意位置（`super` 含む） | `SetArgumentsForwardedHelper` | 専用ヘルパ（汎用再解釈をスキップ） |
| D | 上記外（named kw を持つ callee など） | `SetArguments` | 従来の汎用パス |

### 3.0 前提: forwarding callee は常に specialize

`compile_method_call`（`compile/method_call.rs`）は `params().forwarding()` な
callee を `is_C_immediate` ヒューリスティックから除外し、**無条件に
specialize** する。トランポリン本体を同一コンパイル単位に取り込まない限り
§3.4 以降の跨ぎ最適化が成立しないため。

### 3.1 Increment 1 — `f→g` 呼び出しの specialize（Array は温存）

対象: forwarding `g(x.., ...)`（`callsite.forwarding` かつ**末尾単一 splat**
`splat_pos == [pos_num-1]`、先頭 `lead_num = pos_num-1` 個の通常引数 +
`...` rest）で、`g` の positional が required(+optional) のみの場合。純転送
`g(...)` は `lead_num == 0` の特殊形として同経路に内包。

`AsmInst::SetArgumentsForwarded` の lowering は
`arch/x86_64/compile/method_call.rs::jit_set_arguments_forwarded` /
`arch/aarch64/compile/…`。asm は**書込み前ガード → ミスは無ロールバックで
フォールバック**という形:

self を `LFP_SELF` へ → `lead_num` 個の先頭引数を frame slot `args+i` から
callee slot `i` へ unroll コピー → `args+lead_num` の `...` Array を読み
tag / `RVALUE_OFFSET_TY == ARRAY` 検査 → `RVALUE_OFFSET_ARY_CAPA` /
`HEAP_LEN` / `INLINE` / `HEAP_PTR` で len と要素基底取得（inline/heap 両対応）
→ **長さガード**（`expected_len` は即値）→ 転送 kw_rest が非 nil なら脱出 →
callee slot `lead_num..` へ src 昇順 / dst 降順の 2 ポインタコピー → 成功
sentinel `rax = NIL_VALUE`。ガードミスは page1 の `fallback:` で既存
`jit_set_arguments`（`jit_generic_set_arguments`）へバイト一致委譲。

Array を温存するため deopt は自明に安全（インタプリタは実 Array を普通に
使うだけ）。

なお非 forwarding の素の末尾 splat（`g(x.., *ary)`）にも同じ lowering を
適用するアームがある（`item_check(*node)` 形の再帰が該当）。

### 3.2 Increment 1.5 — opt/post/rest を持つ `g`（runtime ヘルパ方式）

rest 付き `g`（`def g(a,*r)` 等）は `*rest` 配列の新規確保が CRuby セマン
ティクス上不可避で、手書きアロケーション asm は GC / ライトバリア絡みで
破壊リスクが高い。よって**専用 runtime ヘルパ**方式:

- `runtime::jit_forwarded_set_arguments`（`jit_generic_set_arguments` と同
  シグネチャ）。forwarding 形状（単一 splat、`lead = sp`）が静的に既知なの
  で、**転送 kw が空**の常套ケースは汎用 `set_callee_frame_arguments` の
  `splat_pos` 走査・余剰 kw 機構をスキップして positional buffer を直接構築
  し `fill_positional_args1`（req/opt/rest/post を正しく処理）へ渡す。kw が
  実際に転送される稀ケースは実証済み汎用関数へ委譲し、微妙な kw→rest セマン
  ティクスをバイト一致で保つ。
- `AsmInst::SetArgumentsForwardedHelper` の lowering は `jit_set_arguments`
  と同一の asm 形状（レジスタ設定・rsp 調整・エラー処理）で**call 先のみ
  差し替え**。手書き asm ループ・アロケーションは追加しない。

### 3.3 Increment 2 — mixed 経路の `Vec` 排除

`set_callee_frame_arguments` の汎用 splat 分岐は `g(x, ...)` / `super(x, ...)`
等で呼び出し毎に `Vec<Value>` をヒープ確保していた。これを
`smallvec::SmallVec<[Value; 8]>` に置換し、引数列が短い通常ケースでヒープ
確保を消去（巨大引数列のみ heap へスピル）。分配ロジック
（`fill_positional_args1`）は不変で共有。

### 3.4 Increment 3 — D1/K1: `f` 側 rest Array / kw Hash の確保省略

当初計画で「最大の利得かつ最大のリスク」としていた遅延実体化。現行実装は
新しい `LinkMode` を導入せず、**抽象状態の注釈（`DeferredForward`）+ 生成側
の拒否権**という形に落ちている。

#### ゲート

**構造ゲート** — `Store::forwarding_trampoline_rest`（`store/function.rs`）:
`f` が*純転送トランポリン*（`def f(...) = g(...)`）であること。すなわち
req/opt/post が 0、rest あり、`params().forwarding()`、かつ**本体が単一基本
ブロック**（join が遅延スロットを観測し得ない）。転送呼び出しの個数は無制限
— 各 consume が source-route するか拒否権を行使するかのどちらかなので、
任意個数・混在でも安全。

**呼び出し側ゲート** — `JitContext::forward_rest_deferral`（`jitgen/context.rs`）:
自フレームが specialize 済みであること、mother の callsite が
`is_simple_call`、`hash_splat_pos` と `block_arg` が無いこと。リテラル
keyword があれば K1 として rest と**一括で**遅延する。結果は

```rust
DeferredForward { rest_local, src, len, kw }
```

で、`src`/`len` は**呼び出し元フレームの引数スロット窓**（caller の
レジスタ番号）。`f` は自前の rbp を確立している（`init_func` の
`pushq rbp; movq rbp, rsp`）ので、直接の物理呼び出し元の rbp は `f` が
`[rbp]` に退避した値であり、source は `[caller_rbp - rbp_local(src + i)]`
に居る。トランポリンの入れ子で「遅延済み source をさらに遅延する」ことは
起きない — 親自身の転送 callsite は splat を持つため `is_simple_call` に
落ちるからである。

#### 注釈 — LinkMode は変えない

`AbstractState`（`jitgen/state/slot.rs`）はフレーム入口で `deferred_forward`
を置くだけで、rest スロットの `LinkMode` は baseline の `S` のまま残す。
遅延が発火すれば caller 側の `set_arguments` が実 `nil` を物理的に書き
（GC 安全）、発火しなければ caller が普通に Array を作ってそのスロットに
入る。`C(nil)` にしないので、後者で書き戻しが実 Array を壊すことがない。

#### consumer（`f` 内の各転送）

`set_arguments` の A 段が `AbstractState::deferred_rest_src()` を引き、
`req <= lead+len`、post 無し、余剰は明示 `*rest` が吸収、転送 `**kwrest` は
nil、という条件で**充填レイアウトがコンパイル時定数**と示せたときだけ
`ir.set_deferred_rest()` を立てて caller 窓から直接読む。示せない場合は
`ir.set_needs_rest_array()` で**拒否権**を行使する（配列パス／ヘルパ／
汎用／native callee のすべてが拒否権を行使する）。

callee フレームの静的レイアウトは
`FuncInfo::forwarded_deferred_layout` → `ForwardedLayout { from_src,
none_fill, rest, kw_rest }`。先頭引数は必ず req/opt に消費される
（`reqopt >= lead_num`）ので、`*rest` に入る値は caller の source 窓の
**連続した末尾**になり、`create_array` 一発で呼び出し元フレームから直接
構築できる（中間バッファ無し）。埋まらない optional スロットには `None`
（0）が入り、callee プロローグの `CheckLocal` が既定値式を走らせる。

#### producer（caller 側）

`send_specialized` が `defer_rest = deferred_rest && !needs_rest_array` を
計算する。すなわち**1 つ以上の consume が source-route され、かつどの
consume も実 Array を要求しない**ときだけ `set_arguments` の rest 充填が
`create_array` を省略し、代わりに

- rest スロットへ実 `nil` を格納（GC 安全）、
- `write_back_range` で **source 窓をスピルしてメモリ常駐化**
  （routed read と deopt 実体化の両方がメモリを読むため）

を行う。`**kwrest` 側は `TraceIr::CheckKwRest` の空 Hash 生成
（`jitgen/compile.rs`）も省略され nil のまま。nil の hash-splat は全ての
consume 経路で「keyword なし」として扱われるので普遍的に安全。

#### K1 — リテラル keyword の同時遅延

`X.new(a, k: 1)` のように mother が**リテラル keyword** を渡す場合、
`DeferredForward::kw = (kwrest_local, kw_pos, names)` として kw も遅延する。
`kw_forward_route`（`compile/method_call.rs`）が callee の宣言 keyword と
静的に突き合わせ、`route[i]` = callee kw パラメータ `i` を満たす caller
スロット（省略可能 keyword が無ければ `None` → 0 埋めで既定値が走る）を返す。
必須 keyword が埋まらない、`**kwrest` を持つ callee、名前が合わない、と
いった場合は route 不成立で汎用へ。rest と kw は**一括で**遅延するか一括で
諦めるかのどちらかである（caller 側のスキップは 1 つのフラグが両方を覆う
ため）。

#### deopt 安全性

- **(D1) `f` 内 deopt**: `WriteBack` に `forward_rest` / `forward_kwrest`
  エントリが載り、`gen_write_back_for_deopt` がリテラル書き戻しの**後**に
  `gen_forward_rest_materialize`（`create_array`）と
  `gen_forward_kwrest_materialize`（`runtime::correct_rest_kw`）を走らせて
  実体をスロットへ書く。呼び出し元 rbp は `[rbp]` から復元。順序を後ろに
  置くことで、確保を伴う呼び出しの最中もフレームが GC 整合を保つ
  （未書き込みの遅延スロットは caller が入れた `nil` を保持している）。
  aarch64 版は `arch/aarch64/compile/mod.rs::a64_gen_forward_rest_materialize`。
- **(D2) 呼出ガード失敗**: A 段は「レイアウトが定数」というゲートを通って
  いるので長さガードもフォールバックも持たない（失敗し得ない）。B 段の
  ガードはすべて callee フレーム書込み前にあり、ミスは無ロールバックで
  汎用へ。
- **(D3) 多重転送**: 注釈は最初の転送で消さない。すべての consume と
  side exit が同じ注釈を参照する（拒否権も注釈が生きている間だけ意味を
  持つ）。

`*rest` の意味論（毎回新しい Array）とも整合する — 実体化は常に新規確保。

### 3.5 Increment 4 — `super` 暗黙転送（単一 splat 任意位置）

`jit_check_super` が super 先 FuncId をコンパイル時解決し、
`handle_super_forward`（`bytecodegen/method_call/arguments.rs`）は
`forwarding=true` の CallSite を生成するため、super も同じ `set_arguments`
経路に乗る。

- `def m(a,b); super; end`（splat なし）→ 既に `is_simple` 特化済み。
- `def m(a,*r); super; end`（rest 末尾、`sp == pn-1`）→ Increment 1 系。
- `def m(a,*r,z); super; end`（**rest の後ろに post**、`sp != pn-1`）→
  ヘルパゲートを `splat_pos.len() == 1`（任意位置の単一 splat）へ一般化し、
  `jit_forwarded_set_arguments` の fast path が
  **lead[0..sp] ++ splat配列 ++ post[sp+1..]**（汎用 splat 分岐とバイト一致の
  順序）を直接構築する。

zero-alloc の inline 路は trailing + required-only のまま据え置き（post を
跨ぐ asm は複雑化＝リスクのため安全なヘルパへ誘導）。

### 3.6 VM/汎用ティア — lazy `(...)` 呼出規約

JIT が specialize しない経路（インタプリタ実行、汎用 `set_arguments`）でも
rest Array を作らずに済ませる規約。`codegen/runtime/args.rs`。

**ゲート** — `Store::lazy_forwarding_rest`: 構造ゲート
（`forwarding_trampoline_rest`）に加え、`ISeqInfo::forwarding_no_escape` が
真であること。後者は `bytecodegen/encode.rs::forwarding_no_escape` が
バイトコード列から前計算する述語で、

- `super`（zsuper はメソッドフレームの**全**パラメータスロットを読む）
- `yield`
- ブロックリテラル（`callsite.block_fid`。ブロック本体に zsuper があると
  外側チェーン経由で親のスロットを読む）
- 保守的に `defined?(super)` / `defined?(yield)`

のいずれかを含む本体を失格にする。結果は `ISeqInfo::lazy_forwarding_rest`
にキャッシュされ、実行時は単なるフィールドロードになる。

**エントリ** — `set_frame_arguments`: 呼び出し側が平坦（splat / kw /
hash_splat なし）なら、rest Array を作らず callee の rest スロットへ
`Fixnum(callid)` **マーカー**を、`**kwrest` へ nil を書いて終わり。

**解決** — `resolve_lazy_forwarding`: 転送 callsite の splat スロットが
マーカーなら、`lazy_marker_source` が cfp チェーンを辿って元 caller の
フレームと callsite を特定し、その引数スロットを直読みして
`lazy_forward_fill` で callee フレームを直接埋める（`lead ++ 元の引数列 ++
post` の順は実体化パスとバイト一致）。fast gate を外れる形
（keyword を取る callee、block 形 callee、`g(*a, ...)` のような追加 splat）は
`materialize_lazy_at_callsite` が**その場で実 Array を作り**、トランポリンの
rest スロットと splat 引数スロットの両方に書いてから実証済みの汎用機構へ
渡す。

**マーカーの誤認防止**: splat スロットの `Fixnum` がマーカーであるのは
「呼び出しフレーム自身が lazy 資格を持つ」場合に限る。`def m(*r); r = 7;
super; end` のような再代入済み名前付き rest も splat スロットに Fixnum を
置くが、そのフレームは lazy 資格を持たないので通常のスカラ包み込みに落ちる。
加えてマーカーは**末尾** splat にしか居ない（`...` は末尾必須）ので
`trailing` 判定も課している。

**エスケープ**: 文字列 `eval`（`globals.rs`）と `Kernel#binding`
（`builtins/kernel.rs`）は、コンパイル対象フレームから cfp チェーンを辿って
`materialize_lazy_forwarding` で全マーカーを実体化してから進む。lazy な
フレームはブロックリテラルを含み得ない（＝Proc として生き延びない）ので、
cfp チェーンの走査で必要な範囲を尽くせる。

### 3.7 転送を跨いだ上位最適化

D1 注釈があると転送後の位置引数の個数が静的に確定する
（`forwarded_trivial_pos_num`）。これにより `is_simple_call` が門前払いする
forwarding callsite に対しても、

- **trivial method fold**（`ISeqHint::ConstReturn` / `SelfReturn`）— 呼び出し
  自体が消える。call を消すこと自体が転送の consume なので、`ir.set_deferred_rest()`
  を立てて caller 側スキップを維持する（さもないと誰も見ない Array を作る）。
- **frameless な ivar ストア展開**（`compile/frameless.rs::ivar_store_body`）—
  `ArgSlot::Caller` / `AsmInst::LoadCallerSlot` で呼び出し元スロットを直読み
  して `@a = a` 相当のストアを caller の命令として展開する。

が効く。Ruby レベルの `Class#new` は `o.__builtin_initialize__(...)`
（`bypass_visibility` 付きの forwarding call）という*まさにこの形*なので、
`X.new(a, b)` が **allocate + ivar ストア 2 本**にまで落ちる。これが実利上
最大の効果である。

## 4. フォールバック条件（汎用パス据置）

- **named keyword パラメータを持つ callee** への転送 / `super`
  （K1 の静的ルーティングが成立する場合を除く）
- `f` が `binding` / `eval` / フレーム capture を含む（lazy 規約は
  `forwarding_no_escape` で、JIT は capture ガードで排除）
- 複数 splat（`g(*a, ...)`）
- **ruby2_keywords**: 呼び出し側が keyword 構文を持たない転送
  （`ruby2_keywords def t(*args); super; end` や、委譲ブロックの
  `target(*args, **kwargs)`）。フラグ付き末尾 Hash の keyword 昇格
  （`r2k_promote`）を fast path が実装していないため、汎用パス必須。
- `single_arg_expand`（block 形 callee）対象の転送
- `g` が単相に未解決（megamorphic / 未キャッシュ）

## 5. 主な実装箇所

| 箇所 | 内容 |
|---|---|
| `globals/store.rs`（`ParamKind::Forwarding`） | `...` の脱糖 |
| `bytecodegen/method_call/arguments.rs` | `handle_forward` / `handle_super_forward` |
| `bytecodegen/encode.rs::forwarding_no_escape` | lazy 規約の本体側ゲート（前計算） |
| `globals/store/function.rs` | `is_simple_call` / `forwarding_trampoline_rest` / `lazy_forwarding_rest` / `forwarded_deferred_layout` |
| `codegen/jitgen/context.rs::forward_rest_deferral` | D1/K1 の呼び出し側ゲート、`DeferredForward` |
| `codegen/jitgen/state/slot.rs` | 遅延注釈と `deferred_rest_src` |
| `codegen/jitgen/compile/method_call.rs::set_arguments` | 4 段の分岐、`kw_forward_route`、拒否権 |
| `codegen/jitgen/compile/method_call.rs::send_specialized` | `defer_rest` の確定（producer） |
| `codegen/jitgen/compile.rs`（`CheckKwRest`） | 空 Hash 生成の省略 |
| `codegen/jitgen/asmir.rs` | `SetArgumentsForwarded` / `…Helper` / `LoadCallerSlot` |
| `arch/{x86_64,aarch64}/compile/…` | 上記の lowering、deopt 実体化 |
| `codegen/jitgen.rs::gen_write_back_for_deopt` | `forward_rest` / `forward_kwrest` の実体化 |
| `codegen/runtime/args.rs` | 専用ヘルパ、lazy 規約（マーカー / 解決 / 実体化）、`SmallVec` 化 |
| `globals.rs`（eval）・`builtins/kernel.rs`（binding） | マーカーの強制実体化 |

## 6. 検証

- `monoruby/tests/method_call.rs` — `forwarding1..3` / `forwarding_super` /
  `anonymous_block_forwarding1..4` / `anonymous_rest_forwarding` /
  `forwarding_specialized_*`（inline / heap / zero-arity / arity 不一致
  フォールバック / kwargs フォールバック / block 透過）/ `forwarding_leading_*`。
- `monoruby/tests/kwargs_forward.rs` — K1（`X.new(...)` 形のリテラル keyword）。
- `monoruby/tests/forwarded_block_yield.rs` — block 透過・`break`・非局所
  `return`・二段ホップ。
- `monoruby/tests/ruby2_keywords.rs` — 汎用パス据置の担保。
- `codegen/jitgen/compile/method_call.rs` のインラインテスト —
  `forwarded_opt_callee` / `forwarded_rest_callee` /
  `forwarded_struct_rest_native` / `trivial_forwarded_fold_and_redefine` /
  `deferred_construction_deopt`（遅延中の deopt で実体化を踏む）。
- `codegen/runtime/args.rs` のインラインテスト — `lazy_forwarding` /
  `lazy_forwarding_escape`（`binding`/`eval` によるマーカー実体化）/
  `lazy_forwarding_class_new`。
- 経路の発火確認は `--features jit-log,emit-asm`、確保ゼロの確認は
  `--features gc-log`、遅延実体化の踏み込みは `--features deopt` で行う。
- GC 絡みの回帰は `GC_STRESS=1`（手動 `gc-stress` ワークフロー）で確認する。
  遅延スロットは常に物理的な `nil` を保持し、source 窓は cfp チェーン上の
  caller フレームにあるので、両者ともスキャン対象である。

### 参照 Ruby について

単一コードのテストヘルパは `monoruby/tests/ruby_oracle.tsv` のスナップショット
オラクルを再生し、ミス時のみ実 `ruby` を起動する（`CLAUDE.md` 参照）。keyword を
印字する比較は vendored pin（現在 4.0.2）に一致する CRuby が必要で、
`MONORUBY_TEST_ORACLE=ruby` で全件を実 Ruby に対して取り直せる。positional
転送のみのケースは古い Ruby でも検証できる。
