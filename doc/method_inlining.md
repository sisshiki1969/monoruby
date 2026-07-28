# メソッドインライン化(フレーム生成の省略)

specialization をさらに進め、呼び出し先メソッドのフレーム生成そのものを省略する
最適化の設計文書。第1段階として「例外を発生させないことが保証されているメソッド」
のみを対象とする。

## 1. 背景

### 1.1 既存の specialization が省くもの / 省かないもの

`JitType::Specialized`(`codegen/jitgen/compile/method_call.rs` の
`specialized_iseq` / `compile_specialized_func`)は、呼び出し先を呼び出し元の
情報(self クラス・引数の `LinkMode`・定数)付きで専用クローンコンパイルし、
`SpecializedCall` で直接 call する機構である。これにより以下は既に省けている。

- 動的ディスパッチ / wrapper / class-guard stub
- 引数の再パース(`set_arguments` のインライン展開)
- `LinkMode::C` の伝播によるメソッド境界を越えた定数畳み込み
  (`ReturnValue::Const` なら戻り値ストア自体が消える)

一方で **フレーム構築は丸ごと残っている**。monomorphic な JIT→JIT 呼び出し
1回あたりの固定コストはおよそ:

| 呼び出し元 | 呼び出し先 |
|---|---|
| `check_stack` | `pushq rbp; movq rbp,rsp; subq rsp,N` |
| FPR 退避 + continuation frame 確保 | 非引数スロットの nil 埋め |
| `ContFramePc` | `Preparation`(ivar テーブル容量チェック) |
| 引数ストア(全てメモリ経由) | GC/プリエンプトポール |
| `SetupMethodFrame`(outer/meta/svar/block) | |
| `set_lfp` + `push_frame` + `call` | |
| 復帰後: `pop_frame` + FPR 復元 + `handle_error` + 結果ストア | |

さらに `send` は復帰後に `unset_class_version_guard` /
`unset_const_version_guard` / `unset_side_effect_guard` を行うため、呼び出しを
またぐたびに下流でガードが再発行される。

### 1.2 既にフレームレスの経路

フレームを作らないインライン化は既に3系統存在する。

1. builtin の `InlineGen`(`executor/inline.rs`, `doc/inline.md`)
2. `AttrReader` / `AttrWriter` / `StructReader` / `StructWriter`
   (`compile/method_call.rs` の `attr_reader` / `attr_writer` など —
   `LoadIVarInline` / `LoadIVarHeap` / `StoreIVarHeap` に展開)
3. `ISeqHint::ConstReturn` / `SelfReturn` による自明メソッドの畳み込み

本最適化はこれらの **厳密な一般化** である:「ivar 1個の read/write」→
「移動系命令の任意列」。実装も `attr_reader` / `attr_writer` の
AsmInst・ガード構成をそのまま流用する。

## 2. 対象となるメソッド(第1段階)

以下を全て満たす ISeq をインライン化候補とする。

- パラメータが required のみ(`ParamsInfo::is_simple()` かつ
  `optional_num == 0` 相当。opt/rest/post/kw/kwrest/block param なし)
- 例外テーブルなし(`has_exception_handler()` が false)
- 本体が以下のバイトコードのみで構成される(単一 basic block は命令集合から自明):
  - `InitMethod`(172)— スキップ
  - `FrozenLiteral`(6)— nil / true / false / Integer / Symbol / frozen 定数
  - `Mov`(178)— ローカル変数の代入・参照は全てこれに落ちる
  - `LoadIvar`(16)/ `StoreIvar`(17)— バイトコード上 ivar アクセスは常に
    self(= インライン化ではレシーバ)基準
  - `Ret`(80)
- 命令数上限(`MAX_INLINE_BYTECODE_LEN`)以下

呼び出しサイト側の条件:

- `is_simple_call` が成立(splat / hash-splat / kw なし、arity 一致)
- ブロックを渡さない(`block_fid.is_none() && block_arg.is_none()`)
- `pos_num == callee.req_num()`
- ivar アクセスを含む場合、レシーバクラスが即値クラス
  (`is_always_frozen` / 非ヒープ)でない

除外(将来拡張): 可変 `Literal`(String リテラル等。raise しないが
`value_deep_copy` の C 呼び出しを伴う)、分岐、`MethodRet`。

## 3. 核心: 排除すべき「割り込み」は例外だけではない

フレームを作らないということは、**本体の途中でインタプリタに制御を渡す手段が
一切なくなる**(インタプリタが再開すべき callee フレームが存在しない)ことを
意味する。排除すべきは:

1. **例外(raise)** — 命令セット制限で排除。
2. **deopt(side exit)** — 通常の `StoreIvar` lowering は frozen ガードで
   deopt を発行する。callee 内部の pc への deopt はフレームがないと不可能。
3. **GC セーフポイント** — GC は CFP チェーン経由でフレームを走査するため、
   callee 相当の値は GC から見えない。本体中にポールがあってはならない。
4. **フレームのヒープ化 / 非局所脱出** — `Proc.new` / `binding` /
   `MethodRet` は不可(命令セットに呼び出しがないので自明)。

これらは **ガードの巻き上げ(hoisting)** で解決する:

- **frozen ガード**: レシーバは本体中に変化しない(呼び出しがない)ので、
  呼び出しサイトで1回だけ発行すれば本体内の全ストアで省略できる。
- **ivar テーブル容量**: heap ivar への load は境界チェック付きの
  `LoadIVarHeap { self_: false }`(miss は nil)、store は cold path で
  `set_ivar` を呼ぶ `StoreIVarHeap` を使う(`attr_reader` / `attr_writer` と
  同一)。`set_ivar` はテーブル拡張のためアロケートし得るが、raise せず、
  アロケーションは alloc_flag を立てるだけで実収集はポールまで遅延される
  (`alloc.rs`)。よって本体中に GC は走らない。
- GC ポール / スタックチェック / continuation frame: 呼び出しではないので
  一切発行しない。

結果、構造は
**「呼び出しサイトで全ガード → deopt・raise・GC・capture が一切ない直線コード
→ 戻り値定義」** となり、deopt 先は常に **呼び出し元の `MethodCall` の pc**
でよい。

### 3.1 再実行安全性

deopt 先を `MethodCall` の pc にする(= 呼び出し全体をインタプリタで再実行
する)ことが正しいためには、**deopt 時点で観測可能な副作用がまだ起きていない**
必要がある。ガードを巻き上げない素朴な実装だと壊れる反例:

```ruby
def swap!
  tmp = @a      # 再実行時、@a は既に @b で上書き済み
  @a = @b
  @b = tmp      # ここで(仮に)deopt すると tmp が壊れる
end
```

全 deopt をストアより前に巻き上げる本設計では、本体は中断不能な直線列になる
ため、この問題はクラスごと消滅する。実装ではこれを不変条件として扱う
(インライン本体のコード生成中に deopt を新規作成しない)。

### 3.2 FrozenError のスタックトレース

frozen ガードの失敗は「例外を投げる」のではなく「deopt する」。

1. 呼び出しサイト先頭の frozen ガードが失敗
2. deopt → 呼び出し元の `MethodCall` の pc にインタプリタで復帰
   (この時点で例外は未発生)
3. インタプリタが `vm_send` で呼び出しを普通に再実行 — 本物の callee
   フレームが作られる
4. callee 内の `StoreIvar`(`set_instance_var_with_cache`)が `FrozenError`
   を発生

例外の発生元・バックトレースは正しく callee のメソッドになる。これは既存の
非インライン版 `StoreIvar` の frozen ガード(deopt であって error exit では
ない)と同じ構造であり、deopt 先が「callee 内の store の pc」から「caller の
call の pc」に変わるだけである。逆に JIT コードから直接例外を発生させる設計
(error side exit)にすると、`handle_error` は現在のフレームの Meta と pc
から位置を組み立てるため、caller が発生元として記録されてしまう。
`IvarIdNotFound` の recompile-deopt も同じ理屈で正しい。

## 4. 実装

実装本体は `monoruby/src/codegen/jitgen/compile/inline_iseq.rs`
(`try_inline_iseq` / `analyze_inline_iseq` / `compile_inline_iseq`)。
テストも同ファイルにある。呼び出しサイトのフック(および `simple_fold`
ゲート)は `compile/method_call.rs` の `FuncKind::ISeq` アーム内。

注意: 呼び出しサイトがインライン化されるのは**呼び出し元**が JIT
コンパイルされたときである(トップレベルスクリプト本体はコンパイルされない
ため、テストではホットループをドライバメソッドの中に置いている)。

### 4.1 配置

`compile_method_call`(`codegen/jitgen/compile/method_call.rs`)の
`FuncKind::ISeq` アーム内、`ISeqHint` fold の後・specialization 判定の前。
この時点で class-version ガードとレシーバクラスガードは発行済み。
`inline_method_cache` への push(再定義無効化の簿記)も既存と同様に行う。

### 4.2 シンボリック評価

callee の `AbstractFrame` は作らない。命令セットがデータ移動のみなので、
小さな環境で本体を評価する:

```
env: callee SlotId → InlineOperand
InlineOperand =
    Const(Value)        // リテラル(immediate / frozen ヒープ値)
  | CallerSlot(SlotId)  // caller のスロットの別名(初期値: slot0 = recv,
                        //   slot i = args + i - 1、他のローカルは Const(nil))
  | PendingIvar(IvarId) // まだ実体化していない ivar ロード(遅延)
```

- `FrozenLiteral` → `env[dst] = Const(v)`、コード生成なし
- `Mov` → `env[dst] = env[src]`、コード生成なし
- `LoadIvar` → store-to-load forwarding: 本体中で既にストアした id なら
  `env[dst] = ivar_env[id]`(コード生成なし)。未ストアの id なら
  `env[dst] = PendingIvar(id)`(遅延、消費時に実体化)
- `StoreIvar` → `env[src]` を実体化してストアを発行、`ivar_env[id] = env[src]`
- `Ret` → `env[ret]` に応じて caller の dst を定義

`PendingIvar` は消費のたびに実体化する(ロードは純粋で、対象 ivar が未変更で
ある限り再ロードは正しい)。**ストア対象の id を指す `PendingIvar` が env に
残っている状態でその id へストアする場合はインライン化を断念する**
(実体化済みの値を保持するレジスタがないため。上の `swap!` パターンが該当。
将来はスクラッチスロットの確保で解決可能)。

### 4.3 発行するコード

```
   ; class-version guard / receiver-class guard(既存コードが発行済み)
   ; --- ストアがある場合のみ ---
   load recv → rdi
   guard_frozen → deopt(MethodCall pc)
   ; --- 各 StoreIvar(本体順)---
   <src を実体化>            ; Const → lit2reg(rax) / CallerSlot → load_or_reg
                             ; PendingIvar → load recv → rdi; LoadIVar* → rax
   load recv → rdi           ; 実体化で rdi が壊れるため毎回リロード
   StoreIVarInline / StoreIVarHeap   ; attr_writer と同一の AsmInst
   ; --- Ret ---
   Const → def_C / def_lit2gp、CallerSlot → copy_slot、
   PendingIvar → load recv → rdi; alloc_gp_for(dst); LoadIVar*; bind_gp_resident
```

- inline ivar(ObjTy::OBJECT かつ `ivarid < OBJECT_INLINE_IVAR`)は
  `LoadIVarInline` / `StoreIVarInline`、それ以外は境界チェック付きの
  `LoadIVarHeap { self_: false }` / `StoreIVarHeap`(`using_fpr` 前に
  `get_using_fpr` で GP プールを flush — `attr_writer` と同一)。
- `IvarId` はレシーバクラスから **コンパイル時解決**。未登録なら
  `CompileResult::Recompile(IvarIdNotFound)`(既存の attr 経路と同一)。
- ストアが1つでもあれば `state.unset_side_effect_guard()`。
- `send` と違い class-version / const-version ガード状態を **unset しない**
  (本体に呼び出しがないため無効化事象は起き得ない)。二次的な最適化効果。

### 4.4 正当性の整理

- **deopt pc**: 全ガードが副作用より前 → `MethodCall` pc + caller の
  `WriteBack` で再実行が常に安全。
- **無効化**: メソッド再定義は class-version guard + `inline_method_cache`。
  レシーバクラスの変動は既存のレシーバクラスガード。
- **GC 到達性**: 本体中にポールなし。本体終了後は値が caller のスロット /
  dirty resident に載り、既存の `WriteBack` 機構で到達可能。
- **観測可能性**: 本体は呼び出しを含まないため、`caller` / backtrace / `$!`
  等からフレームの不在を観測する手段がない。

## 5. 将来拡張

- **可変 `Literal`**(String リテラル): `value_deep_copy` は raise しない。
  FPR 退避を伴う C 呼び出しのため第1段階では除外。
- **`swap!` パターン**: caller フレームの `prologue_bytes` を
  `resolve_dyn_var_offsets` で拡張してスクラッチスロットを確保すれば、
  `PendingIvar` の先行実体化で解決できる。
- **分岐**: 合流点での env join が必要。
- **ガード付き算術**: Fixnum 加算等は「deopt を伴うが raise しない」。
  deopt を副作用前に並べ替えられる場合のみ許可、という形で漸進拡張。
- **例外ありメソッド**: deopt / raise 時に callee フレームを合成してから
  インタプリタに渡す「遅延フレーム実体化」が必要。D1
  (`forward_rest_deferral`)と同じ遅延実体化ファミリーの延長線上にある。
