# JIT 最適化方針: Argument Forwarding (`def f(a, ...) g(...) end`)

本書は argument forwarding（`...`）を JIT で最適化するための実装方針を、
現行コードの該当箇所に紐づけて記述する。原則・段階分け・deopt 安全性
（呼び出し元が特に注意を要求した点）を中心にまとめる。

## 1. 現状のコスト構造

`...` は `ParamKind::Forwarding`（`ruruby-parse/src/node.rs:263`）として
パースされ、`monoruby/src/globals/store.rs:943-952` で

- 合成 **rest** スロット
- 合成 **kw_rest** スロット（`SlotId(1 + args_names.len())`）
- 匿名 **block** パラメータ

へ脱糖される（`ParamsInfo::forwarding = true`、`globals/store/iseq.rs:608`）。

`g(...)` 呼び出しは `bytecodegen/method_call/arguments.rs:70-140` の
`handle_forward` が、

- `splat_pos` に mother の rest を指す位置（純転送は `(pos_start, 1, vec![0])`、
  先頭引数つきは `splat_pos.push(len)` で末尾）、
- `hash_splat_pos = [kw_rest]`、
- `BlockArgProxy`（`inst.rs:120`、エンコード `encode.rs:374-378`）

を持つ `CallSite { forwarding: true }` を生成する。

実行時コストは 2 箇所:

1. **caller → `f`**: `set_callee_frame_arguments`
   (`codegen/runtime/args.rs:134-216`)。`a` を超える位置引数を rest
   **Array** に確保し、余剰 keyword を kw_rest **Hash** に確保する
   (`fill_positional_args` `args.rs:354-372`)。
2. **`f` → `g(...)`**: `is_simple_call`（`globals/store/function.rs:1241`）が
   `has_splat()` により偽 → JIT は specialize 不可で
   `AsmInst::SetArguments`（`codegen/jitgen/compile/method_call.rs:992`、
   lowering `asmir/compile.rs:332`）→ `jit_generic_set_arguments` の
   汎用パスへ落ちる。

純 `g(...)`（`args.rs:183-188`、`pos_args==1 && splat_pos==[0]`）は
rest Array をそのまま `fill_positional_args1` に渡すため中間 `Vec` は
出ない。先頭引数つき `g(x, ...)` は `args.rs:189-207` の汎用 splat 分岐で
呼び出し毎に `Vec<Value>` を確保する。

## 2. 核心的観察 — forwarding は不透明パイプ

Ruby では `...` は名前を持てず、`f` のコードから rest/kw_rest/block を
観測する手段が一切ない。唯一の読み手は `handle_forward` 生成の転送先
callsite と `BlockArgProxy` だけ。したがって `f` が確保する rest Array /
kw Hash は次を**除き** Ruby から決して観測されない:

- `f` がインタプリタへ **deopt**（呼出規約上 rest/kw_rest スロットに実体を期待）
- フレームが capture される（`binding`、外側 proc 等。
  `possibly_capture_without_block` / `branch_if_captured` が既存ガード）

これは JIT が float を XMM に保持し deopt 時のみ stack へ書き戻す
**WriteBack**（`doc/jit_architecture.md:188-197`）と同型の
「遅延実体化（lazy materialization）」問題である。

## 3. 段階的実装計画

### Increment 1 — f→g 呼び出しの specialize（Array は温存）

最も低リスク。`f` の caller が確保した rest Array を**温存**したまま、
`g(...)` 呼び出しのみを最適化する。Array が実在するため deopt は
インタプリタが実 Array を普通に使うだけで安全（新規の heap 実体化不要）。

- 述語追加（`globals/store/function.rs` 付近）: `callsite.forwarding`
  かつ callee が iseq、転送束ねが「末尾単一 splat（= rest Array）+
  forwarded kw_rest + proxy block」の形であることを判定。
- `compile/method_call.rs::set_arguments`（871-995）の非 simple 分岐
  （989 の `else`）に、上記形のときだけ通る専用 lowering を追加。
  既存 simple/汎用パスはバイト一致で不変に保つ（blast radius 限定）。
- 専用 lowering: rest Array 長を実行時に読み、観測値 `N` に対する
  **長さガード**（`GuardArrayTy` 系 `asmir.rs:932,367` を範とする新ガード）
  を張り、一致時は simple 充填路（`fetch_for_callee` /
  `fetch_rest_for_callee`、`state/read_slot.rs:195-244`）で
  Array 要素を `g` フレームへ直接 mov。不一致は **deopt**
  （実 Array が在るのでインタプリタ復帰は自明に正しい）。
- これにより `g` の specialize / inline（`specialized_iseq`、
  `method_call.rs:254-272`）が forwarding 越しに可能になる。
- 単相 forwarding（`def log(...); real(...); end` 等、常に同 arity）で
  長さガードはほぼ当たり、deopt スラッシュは起きない。

検証: 純/先頭引数つきの positional forwarding（本環境で検証可能）、
長さ不一致を強制する deopt テスト、`--features deopt`。

### Increment 2 — mixed 経路の `Vec` 排除【実装済み】

`set_callee_frame_arguments` の汎用 splat 分岐（`args.rs:189-` 付近）は
forwarding（`g(x, ...)` / `super(x, ...)`）等で呼び出し毎に
`Vec<Value>` をヒープ確保していた。これを `smallvec::SmallVec<[Value; 8]>`
に置換し、引数列が短い通常ケースでヒープ確保を消去（巨大引数列のみ
heap へスピル）。分配ロジック（`fill_positional_args1`）は不変で共有、
x86 非依存・低 blast radius。Ruby 4.0.4 比較ハーネスで
forwarding スイート全 8 件＋`method_call` 全 64 件グリーンを確認。

### Increment 3 — f 側 rest Array / kw Hash の確保省略（要 deopt 実体化）

最大の利得かつ最大のリスク。`f` が forwarding-transparent
（`forwarding() && 非capture`）な JIT パスで rest Array / kw Hash の
確保を発行せず、抽象状態に新 `LinkMode`（`ForwardRest{src_base,src_len}` /
`ForwardKw{..}`、`jitgen/state.rs`・`context.rs`）として記録。転送元
（caller 引数領域）を pin し、`f` 内の全転送 callsite を跨いで維持する。

**Deopt 安全性（呼出元が注意を要求した点）**:

- (D1) `f` 内 deopt: 各側方退出で pin 済み転送元から rest Array /
  kw Hash を**新規確保して rest/kw_rest スロットへ書き戻し**、block を
  復元する遅延実体化を `WriteBack`（`doc/jit_architecture.md:191`）へ
  追加。float spill-on-deopt と同枠組み、生成物が heap obj になる差のみ。
  `*rest` 意味通り「毎回新 Array」で意味論も整合。
- (D2) `g(...)` 直前/呼出ガード失敗: `set_arguments` は既に
  `reg_sub Rsp` → 充填 → `reg_add Rsp` 順。pin 転送元を呼出確定まで
  上書きしない順序を守り、ガード失敗時も束ね再構築可能に保つ。
- (D3) 多重転送 `def f(...); g(...); h(...); end`: pin を最初の転送で
  解放せず最終転送 or deopt まで維持。

### Increment 4 — `super` 暗黙転送・block 透過

`handle_super_forward`（`arguments.rs:142-213`）。block 転送が
`move_frame_to_heap` を誘発する specialize 拒否（`method_call.rs:72-86`、
`has_block_arg()`）と最も込み入って干渉するため最後に。block は
`LFP_BLOCK` に既存、透過専用パスを用意して解決する。

## 4. フォールバック条件（現行 eager パス据置）

- `f` が `binding` / フレーム capture / `possibly_capture_without_block`
- `g` が単相に未解決（megamorphic / 未キャッシュ）
- `single_arg_expand`（block-style callee）対象の転送
- 本書が扱わない束ね形（複数 splat、`ex` あり 等）

## 5. 変更ファイル一覧

| 箇所 | 内容 | Increment |
|---|---|---|
| `globals/store/function.rs` | forwarding-shape 述語 | 1 |
| `codegen/jitgen/compile/method_call.rs::set_arguments` | forwarding 専用 lowering | 1 |
| `codegen/jitgen/asmir.rs` / `asmir/compile.rs` | 長さガード asm 命令 | 1 |
| `codegen/runtime/args.rs` | mixed `Vec` 排除 | 2 |
| `codegen/jitgen/state.rs` / `context.rs` | `LinkMode::ForwardRest/ForwardKw`、pin 管理 | 3 |
| `codegen/jitgen/asmir/compile/init_method.rs` ＋ prologue | 確保抑止・束ね記録 | 3 |
| `codegen/jitgen/deoptimize.rs`（`WriteBack`）| 遅延実体化 | 3 |
| `bytecodegen/method_call/arguments.rs` | `super` 透過調整 | 4 |

## 6. 検証戦略

- `run_test` で forwarding 形状マトリクス + deopt 強制版（型変化ガード /
  BOP 再定義 / block 経由）を回し遅延実体化を踏ませる。
- `--features deopt` と `*rest` 同一性（呼出毎 fresh）テスト。
- `--features gc-log` でホットパスの Array/Hash 確保ゼロをベンチ確認。
- 注: Ruby 3.4↔3.3 の Hash inspect 差（`build.rs` `MIN_RUBY_VERSION=(4,0)`）
  のため keyword を印字する比較は Ruby 4.0 環境で行うこと。positional
  転送は Ruby 3.3 環境でも検証可能。

## 7. 検証環境の構築（ネットワーク制限下）

CRuby 比較ハーネスは Ruby ≥4.0 を要求するが、apt/cache.ruby-lang.org は
遮断される一方 github.com は到達可能。再現手順:

1. `git clone --depth 1 --branch ruby_4_0 https://github.com/ruby/ruby.git`
2. `./autogen.sh && ./configure --prefix=/usr/local --disable-install-doc`
3. `make -j$(nproc)` → `make install-local`
   （`make install` は bundled gems 取得で失敗するため `install-local`）
4. bundled/default gems 未取得のため `export RUBYOPT=--disable-gems`
5. `ruby -e 'puts RUBY_VERSION' > ~/.monoruby/ruby_version`、
   `ruby -e 'puts($:)' > ~/.monoruby/library_path`、`touch monoruby/build.rs`

これで `cargo test` の CRuby 比較が Ruby 4.0.4 で機能する。
