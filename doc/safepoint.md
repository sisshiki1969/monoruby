# セーフポイント(safepoint)

monoruby の VM / JIT が「実行を割り込んでよい」と保証する地点を**セーフポイント**と呼ぶ。
GC(`doc/gc.md`)・タイムスライスプリエンプション(`doc/threads.md` §8)・シグナル配送
(`doc/signal.md`)という 3 つの非同期イベントは、**すべてこの同一のセーフポイント
機構に集約されている**。本書はその共通機構を横断的にまとめる。

対象読者はランタイム実装者。関連ソース:

| 対象 | ファイル |
| --- | --- |
| poll のコード生成(x86-64) | `codegen/arch/x86_64/jit_module.rs`(`execute_gc_inner`) / `vmgen/init_method.rs`(`vm_init`) / `vmgen.rs`(`vm_loop_start`) |
| poll のコード生成(aarch64) | `codegen/arch/aarch64/codegen.rs`(`a64_vm_execute_gc`) / `vmgen.rs` |
| セーフポイント本体 | `executor.rs`(`execute_gc`) |
| poll フラグ | `alloc.rs`(`alloc_flag` / `set_alloc_flag` / `unset_alloc_flag`) / `preempt.rs`(`PREEMPT_BIT` / `consume_poll_flag`) |
| JIT tier の poll IR | `codegen/jitgen/asmir.rs`(`AsmInst::ExecGc`) / `codegen/jitgen/compile.rs` |

---

## 1. セーフポイントとは何か

セーフポイントとは、実行中のインタプリタが**フレームを完全に整合した(GC-complete な)状態
にしたうえで poll フラグを検査する**地点である。フラグがトリガ帯に立っていれば、その場で
`execute_gc`(`executor.rs`)を呼び、以下のいずれか(または複数)を行う:

- **GC**: マーク&スイープ(`doc/gc.md`)。
- **シグナル配送**: 保留シグナルを Ruby 例外 / `Signal.trap` ハンドラ呼び出しに変換
  (`doc/signal.md`)。
- **タイムスライスプリエンプション**: `Thread.pass` 相当のスケジューラ切替
  (`doc/threads.md` §8)。

3 つとも「**フラグを立てて、次のセーフポイントで実行する**」という遅延実行モデルを共有する。
イベント発生源(ページ充填 / シグナルハンドラ / 別 OS スレッドのプリエンプトタイマ)は
フラグを立てるだけで、実処理はセーフポイントに到達した VM スレッド自身が行う。

---

## 2. なぜ即時実行してはいけないか

イベント発生の瞬間に処理を走らせると危険なため、セーフポイントまで遅延する:

- **未退避のライブレジスタ**: JIT コンパイル済みコードは Ruby のローカル変数やレシーバを
  マシンレジスタにキャッシュしている。任意地点で GC ルート走査に入ると、レジスタ上の
  `Value` を取りこぼす(=生存オブジェクトの誤回収)。
- **半端なフレーム**: フレーム構築の途中(引数のホーミング前、`rsp` 調整前など)では、
  ルート走査が読むスロットが不定値を含みうる。
- **シグナルの非同期性**: シグナルハンドラは async-signal 文脈で走るので、そこで
  Rust のアロケータや `RefCell` に触れられない。ハンドラはビットを立てるだけにする。

セーフポイントは「**そこでなら GC ルート走査が完全に整合したフレームを見られる**」と
設計上保証された地点であり、そこへ到達したときにライブレジスタを退避(write-back、§6)
してから処理に入る。プリエンプションが安全なのも「GC が起きうるのと**同じ**地点でしか
切り替わらない」ため — register write-back を含め GC と全く同じ扱いになる。

---

## 3. poll フラグ `alloc_flag`(`u32`)

VM/JIT が参照する単一の `u32`。3 イベントすべてがこの 1 語を共有する。**ベース値(下位)が
8 以上でトリガ帯**、上位ビット `PREEMPT_BIT` がプリエンプト要求。

| 書き手 | 操作 | 意味 |
| --- | --- | --- |
| ページ充填 | `set_alloc_flag`:`+= 1` | ほぼ満杯ページごと(約 8 ページで 8 に到達)→ GC |
| malloc 圧 / `GC.start` | `>= 8` 帯へ持ち上げ | 外部バッファ圧・明示 GC → GC |
| シグナルハンドラ | `+= 10` | 保留シグナル配送 |
| プリエンプトタイマ | `\|= 1 << 30`(`PREEMPT_BIT`) | タイムスライス切替 |

- **ビット 30** であってビット 31 ではない: x86-64 poll は `cmpl …; jge`(符号付き比較)なので、
  ビット 31 だと負値に読めて発火しない。
- タイマは別 OS スレッドから書くので、フラグアクセスはすべてアトミック。GC 後の
  `unset_alloc_flag` は `fetch_and(PREEMPT_BIT)` でベース帯だけ落とし、並行して立った
  プリエンプトビットは保存する。
- 詳細な相互作用は `doc/gc.md` §4.1、`doc/threads.md` §8.2 を参照。

---

## 4. poll の配置

**callee-entry(呼び出し先エントリ)+ ループバックエッジのみ**に poll を置く。
call-site(呼び出し側)には置かない — これは古典的な JVM / CRuby 方式である。

### 4.1 callee entry(`vm_init` / JIT `InitMethod`)

`vmgen/init_method.rs` の `vm_init`:プロローグ直後、`fill_nil` の後に `vm_execute_gc()` を出力。
この位置が「最も安全な poll 地点」である理由(同ファイルのコメント):

- フレームが完全にリンクされ、`rsp` はその下(ステージング用のレッドゾーンなし)、
- 引数はスロットに収まり、残りのレジスタは直前に nil 詰めされている。

よって GC ルート走査は**完全に整合したフレーム**を見る。しかも callee entry は
**全呼び出し経路が必ず 1 度通る唯一の合流点**なので、Rust の invoker
(`invoke_method` / `invoke_block`)から呼ばれた場合も含め、あらゆる dispatch 経路で
一様に発火する(§8)。

### 4.2 ループバックエッジ(`vm_loop_start` / JIT `LoopStart`)

`vmgen.rs` の `vm_loop_start` は先頭で `vm_execute_gc()` を出力する。JIT tier でも
`compile.rs` の `TraceIr::LoopStart` が `state.exec_gc(ir, false)` を出す。これにより
メソッド呼び出しを一切含まない tight loop でも、反復ごとに poll を通る。

### 4.3 call site には置かない

`vmgen/method_call.rs` のコメントどおり、呼び出し側には**スタックオーバーフロー
チェック(`CheckStack` / `vm_check_stack`)だけ**を残し、GC/preempt poll は置かない。
callee entry が全呼び出しで poll するので二重にならず、かつ Rust invoker の caller 側で
poll してはならない制約(§8)とも整合する。

### 4.4 poll 間距離の有界性

native(非 Ruby)の callee には entry poll がないが、任意の非有界実行は必ずループ
バックエッジか Ruby フレームの entry を通過するので、poll から poll までの距離は有界に保たれる。

---

## 5. poll のコード生成

### 5.1 VM tier(x86-64)

`execute_gc_inner`(`jit_module.rs`)が出力する。ホットパスは 1 比較 + fall-through:

```asm
    cmpl [rip + alloc_flag], 8
    jge  gc          ; ベース値 >= 8(またはプリエンプトビット)なら収集パスへ
exit:
    ; --- 別ページ ---
gc:
    write_back        ; 生きたレジスタを退避(§6)
    call exec_gc      ; = executor::execute_gc()
    testq rax, rax
    jne  exit         ; Some(nil)(=正常)なら復帰
    jmp  error        ; None(=例外/シグナル/割り込み)なら伝播
```

- **fall-through が最頻ケース**(フラグ未武装)で、収集本体は `select_page(1)` の別ページに
  置いてホットパスの I-cache を汚さない。
- `exec_gc` は `execute_gc` を呼ぶスタブ。戻り値 `Some` を `rax != 0`、`None` を `rax == 0` として
  分岐する。

### 5.2 VM tier(aarch64)

`a64_vm_execute_gc`(`codegen.rs`)が対称に出力する:

```asm
    mov x10, alloc_flag_addr
    ldr w11, [x10]
    cmp x11, #8
    b.lt skip
    bl  gc
skip:
```

### 5.3 JIT tier

JIT コンパイル済みコードでは、`state.exec_gc(...)` が `AsmInst::ExecGc { write_back, error }`
(`asmir.rs`)を積み、アーキ別バックエンド(`arch/*/compile`)が `execute_gc_inner` /
`jit_execute_gc` に落とす。`InitMethod` と `LoopStart` の両方で発行される(`compile.rs`)。
VM tier との違いは、退避すべきライブレジスタ集合が**その地点の抽象状態から算出した
`WriteBack`** として渡ること(§6)。

---

## 6. write-back(ライブレジスタの退避)

poll でフラグが立っていたら、収集本体に入る**前**に、レジスタにキャッシュされた
生存 `Value` をスタックスロットへ書き戻し、フレームを GC-complete にする:

- **VM tier**: entry poll は直前の `fill_nil` で残スロットを nil 詰め済み。JIT のように
  レジスタキャッシュを持たないので、追加退避は基本的に不要(`execute_gc_inner` の
  `write_back` クロージャは VM 経路では空)。
- **JIT tier**: `gen_write_back`(`jitgen.rs`)が、その poll 地点の抽象状態が示す
  「レジスタに載っている生きたスロット」をスタックへ書き出す。これにより GC ルート走査
  (`Executor::mark` が `lfp()` のスロットを辿る、`doc/gc.md` §8)がレジスタ値を取りこぼさない。

同じ write-back 規律は**コンテキストスイッチ**にも適用される。JIT インライン `Fiber.yield`
は切替前に write-back(`exec_gc`)を発行し、サスペンドされる側のフレームを GC-complete に
してから rsp を差し替える(`doc/threads.md` §1)。スレッドのプリエンプション切替も、
セーフポイントで write-back 済みだからこそ安全に行える。

---

## 7. セーフポイント本体(`execute_gc`, `executor.rs`)

セーフポイントから呼ばれる `extern "C"` 関数。3 イベントを 1 か所で捌く。順序:

1. **`watchdog::poll()`** — poll 到達はインタプリタの進捗なので、ハングウォッチドッグの
   カウントダウンをリセット(`doc/signal.md`)。
2. **`preempt::consume_poll_flag()`** — プリエンプトビットを剥がし `(ベース値, プリエンプトか)`
   を得る。以降の GC 判定はベース値で行う(純プリエンプト tick で偽の full GC を起こさない)。
   フラグ未登録なら防御的に `(8, false)`。
3. **保留シグナルの drain** — `PENDING_SIGNALS` ビットマップを取り、最小番号のシグナルを
   `Signal.trap` ハンドラ呼び出し / 既定例外(SIGINT ⇒ `Interrupt` 等)に変換。エラーを
   立てて `None` を返しうる。
4. **GC(ベース値 `>= 8` のときだけ)** — `parent_fiber` を辿ってルート Executor へ行き、
   `ALLOC.borrow_mut().gc(&Root { globals, executor })`。
5. **`preempt::stress_renudge()`** — stress モードでは切替の**前に**フラグを再武装し、
   切替先スレッドも poll するようにする。
6. **プリエンプション** — `preempt && scheduler::preempt_ok()` のとき `scheduler::pass`。
   `Err`(このスレッドへ配送された kill/raise)は `set_error` + `None` で浮上させる。

戻り値の意味は poll コード(§5.1)と対になる: `Some(nil)` = 正常復帰、`None` = 例外 /
シグナル / 割り込みを伝播せよ。

---

## 8. Rust invoker と「caller 側で poll しない」原則

セーフポイントを **callee entry** に置く設計上の要石は、Rust 側の invoker
(`invoke_method` / `invoke_block`)を**呼び出し側で poll してはならない**という制約である。

- Rust の caller はルート化されていないヒープ `Value` をローカル変数に握ったまま、これらの
  invoker を呼ぶ。caller 側に poll を置くと、その `Value` が GC ルートから外れて誤回収される
  (汎用 `invoke_block` の caller-side poll が `File.open {}` を壊した実績がある — レシーバが
  ブロック復帰後に Rust ローカルにしか無かった)。
- callee entry poll は、これら Rust invoker から呼ばれた Ruby フレームでも一様に発火する。
  結果として `Kernel#loop` / `Array#each` などの Rust 側イテレーションビルトインも、ブロック
  本体が poll-free でも**1 反復ごとに**プリエンプト・シグナル応答可能になる
  (ビルトインごとの監査は不要)。

この不変条件のおかげで、ビルトインは他スレッドに対して**アトミック**になる(自分の
blocking/poll 地点以外では切り替わらない)—— GVL 下で CRuby が C 関数に与える保証と同じ。
一方、**純 Ruby コードのアトミック性はプリエンプションで失われている**ので、純 Ruby の
同期プリミティブは別途 park permit とロックで守られる(`doc/threads.md` §5)。

---

## 9. まとめ

- セーフポイントは monoruby の GC・プリエンプション・シグナルを束ねる**単一の割り込み点**。
  3 イベントとも「フラグを立て、次のセーフポイントで実行」する遅延モデルを共有する。
- 配置は **callee entry(`vm_init` / `InitMethod`)+ ループバックエッジ(`vm_loop_start` /
  `LoopStart`)** のみ。call-site には置かず、スタックチェックだけ残す。
- 各 poll はホットパス 1 比較で、フラグが立ったときだけ**ライブレジスタを write-back**
  してから `execute_gc` に入る。ゆえにサスペンド/収集時のフレームは常に GC-complete。
- callee-entry 配置は「Rust invoker を caller 側で poll しない」原則と表裏一体で、これが
  ビルトインのアトミック性と Rust 側イテレーションの応答性を同時に成立させる。
- 詳細は `doc/gc.md`(収集本体)・`doc/threads.md` §8(プリエンプション)・
  `doc/signal.md`(シグナル)を参照。
