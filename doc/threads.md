# Thread / Fiber / non-blocking IO の実装

monoruby のスレッド機構(`thread` ブランチ系列 #941–#944)の現状をまとめる。
対象読者はランタイムの実装に手を入れる人。関連ソース:

| ファイル | 内容 |
|---|---|
| `monoruby/src/scheduler.rs` | グリーンスレッド・スケジューラ本体 |
| `monoruby/src/value/rvalue/thread.rs` | `ThreadInner`(スレッド制御ブロック)と状態機械 |
| `monoruby/src/value/rvalue/fiber.rs` | `FiberInner`(Fiber 制御ブロック) |
| `monoruby/src/builtins/thread.rs` | Thread クラスのネイティブ・ビルトイン |
| `monoruby/src/builtins/fiber.rs` | Fiber クラスのビルトイン + JIT インライン `Fiber.yield` |
| `monoruby/src/codegen/arch/{x86_64,aarch64}/invoker.rs` | コンテキストスイッチのスタブ(両アーキ) |
| `monoruby/builtins/startup.rb` | Mutex / Queue / SizedQueue / ConditionVariable(純 Ruby)、Thread の簿記系メソッド |
| `monoruby/src/builtins/io.rs` | `blocking_io_region` / `IO.select` のグリーンパス |

## 0. 全体像

- **M:1 グリーンスレッド**。Ruby の `Thread` は 1 本の OS スレッド上で協調的に多重化される。
  真の並列性はない(GVL 型でもない — そもそも OS スレッドが 1 本)。
- **切替はブロッキング地点のみ**。`sleep` / `Thread.stop` / `#join` / `Thread.pass` /
  ブロックする IO / 同期プリミティブの待機で、実行中のスレッドがスケジューラに制御を返す。
  タイムスライス・プリエンプションは未実装(§8)。
- この「切替はブロッキング地点のみ」という性質が設計全体を貫く不変条件になっている:
  - 純 Ruby のコードはスケジューラに対して**アトミック**
    (2 つの非ブロッキング文の間に他スレッドが割り込むことはない)。
    Mutex / Queue / ConditionVariable が純 Ruby で正しく書けるのはこのため(§5)。
  - 切替は必ず VM のセーフポイント(ビルトイン内)で起きるので、
    サスペンド中のスレッドのフレームは常に GC-complete(§6)。

## 1. Fiber(前提となる既存機構)

Thread は Fiber の機構(スタック切替)を土台にしている。まず Fiber 側の構造:

- `FiberInner { handle: Box<Executor>, proc: Proc, stack: Option<NonNull<u8>> }`
- 各 Fiber は **256 KiB の専用マシンスタック**(最下位ページは `mprotect(PROT_NONE)` の
  ガードページ)と、自分専用の `Executor`(VM コンテキスト: cfp、エラー情報、`$~`/`$_` など)を持つ。
- コンテキストスイッチは `Executor::rsp_save` フィールドの **rsp 交換**で実現する。
  スタブ(`fiber_invoker` / `resume_fiber` / `yield_fiber`)は callee-saved レジスタを
  スタックに退避してから rsp を差し替える。
- Fiber の状態は `rsp_save` から導出される:
  `None` = Created、`-1` = Terminated、それ以外 = Suspended。
- **`parent_fiber` チェーン**: resume した側が子の `parent_fiber` に記録され、
  `Fiber.yield` はそこへ戻る(非対称コルーチン)。
- GC は保守的スタックスキャンを行わない。サスペンド中の Fiber のフレームは
  `FiberInner::mark` → 子 `Executor` の CFP チェーン歩行で**精密に**マークされる。
  そのために JIT インライン `Fiber.yield` は切替前に write-back(`exec_gc`)を発行し、
  フレームを GC-complete にしてから切り替える。

## 2. Thread の構造

`ThreadInner`(`value/rvalue/thread.rs`、`ObjTy::THREAD` / `THREAD_CLASS = 59`)は
FiberInner の拡張形で、RValue セルに収まらないため union 内では `Box` 化されている:

```
handle:      Option<Box<Executor>>   // main スレッドのみ None(Executor は埋め込み側所有)
proc/args:   本体ブロックと Thread.new の引数
stack:       専用 256 KiB スタック(初回起動時に遅延確保)
state:       Created | Runnable | Sleeping | Joining | IoWaiting | Dead
resume_exec: park した実行コンテキスト(スレッド root、またはスレッド内で park した nested Fiber)
result / exception:  終了結果(#join / #value が参照)
joiners:     このスレッドを #join で待っているスレッド
pending:     未配送の非同期割り込み(Kill | Raise(err))
killed:      kill 配送済みフラグ(終了時の unwind をクリーンな死として扱う)
```

重要な設計判断: **スレッド root の `parent_fiber` は常に `None`**。
このため `Fiber.yield` をスレッド本体で呼ぶと、main fiber と同じ既存のエラー経路
(Rust 側 / JIT インライン側とも)が**無変更で**正しく FiberError を出す。
スレッドの切替は `parent_fiber` を使わず、専用スタブ(§3)で行う。

## 3. スケジューラ

`src/scheduler.rs`。OS スレッドごとの `thread_local!` シングルトン(`SCHEDULER: RefCell<Scheduler>`)。

```
threads:    生存スレッドの registry(main 含む)— GC ルート
ready:      実行可能キュー(FIFO)
sleepers:   (Option<Instant>, Thread)  — sleep / タイムアウト付き join・IO 待ち
io_waiters: (fd, poll events, Thread) — fd 待ち(§7)
current / main
main_exec:  scheduler_run 実行中のみ有効な main の Executor ポインタ(GC 用)
```

### 3.1 トポロジ: main のスタック上で回るイベントループ

スケジューラループ(`scheduler_run`)は **main スレッドのコンテキストでしか実行されない**:

- **main が park するとき**: 自分のスタック上で `scheduler_run` を普通の関数として呼ぶ。
  ループは main が Runnable に戻るまで green thread を dispatch し、戻ったら return する。
- **green thread が park するとき**: 起床条件を登録してから、プロセス(正確には OS
  スレッド)グローバルなスロット `SCHED_RSP` に保存されたスケジューラ・コンテキストへ
  switch する。つまり制御はループ中の pending な dispatch 呼び出しから「返って」くる。
- **本体が終了したとき**: `thread_invoker` のエピローグが `rsp_save = -1`(Terminated)を
  マークして同じくスケジューラへ switch し、ループが finalize する。

ループが制御を手放す直前に必ず自分のコンテキストを `SCHED_RSP` へ保存するので、
スロットは 1 個で足りる(green thread は `scheduler_run` を呼ばないため、
ループのインスタンスは常に高々 1 つ)。

`SCHED_RSP` は **OS スレッドごと**(`thread_local` の `Cell<u64>`)。各 OS スレッドの
`Codegen` が自分のスロットのアドレスをスタブに焼き込む(alloc_flag と同じ構図)。
テストハーネスのように複数のインタプリタが別 OS スレッドで並走しても衝突しない。

### 3.2 コンテキストスイッチのスタブ(×2 アーキ)

`codegen/arch/{x86_64,aarch64}/invoker.rs` に 3 種。いずれも `parent_fiber` に触らない:

| スタブ | 役割 |
|---|---|
| `thread_invoker` | 初回起動。スケジューラ・コンテキストを `SCHED_RSP` に保存 → 新スタックへ切替 → フレーム構築 → 本体実行。終了時は `rsp_save=-1` をマークして `SCHED_RSP` へ復帰 |
| `switch_to_scheduler(cur, val)` | park。現コンテキストを `cur.rsp_save` へ保存し `SCHED_RSP` へ switch。`val` はスケジューラ側の resume 呼び出しの戻り値になる |
| `scheduler_resume(exec, val)` | 再開。ループのコンテキストを `SCHED_RSP` へ保存し `exec.rsp_save` へ switch。`val`(u64)は park 側の戻り値。**0 を渡すとエラー再開**(§6) |

`parent_fiber` を使わないため、スレッド内にネストした Fiber の resume チェーンは
スケジューラ切替をまたいでも壊れない(`resume_exec` は「park した実行コンテキスト」
そのものを指すので、nested Fiber の中で park してもそこへ直接戻る)。

### 3.3 ブロッキング API の流れ

`sleep(dur)` / `join(target, timeout)` / `pass()` / `wait_fd(s)` は全て同型:

1. 短い `RefCell` 借用で自分を適切な待機構造(sleepers / joiners / io_waiters / ready)に
   登録し、state を更新する(借用をスイッチをまたいで保持しない・借用中に Ruby
   アロケーションをしない、が規律)。
2. green thread なら `park_switch`(`resume_exec` を記録して `switch_to_scheduler`)。
   main なら `scheduler_run` を呼び、戻ったら `take_main_pending()` で
   割り込みの配送を受ける。
3. 起床後、呼び出し元のループで条件(join 対象死亡 / タイムアウト / fd ready)を再検査する。
   スプリアス起床は常に許容される設計。

アイドル時(ready が空)のループは:
- `io_waiters` があれば全 fd を `poll(2)`(タイムアウトは直近の deadline、なければ無限)
- fd 待ちがなく deadline だけなら `nanosleep`
- どちらもなければ **デッドロック**: CRuby と同じく fatal
  `No live threads left. Deadlock?` を main に投げる
  (fd 待ちは外部入力で解決しうるのでデッドロック扱いしない)。
- どちらの待機も EINTR で VM ポーリング地点(`execute_gc`)を経由するので、
  シグナル(SIGTERM 等)への応答性は保たれる。

### 3.4 GC との統合

- **registry が GC ルート**: `Root::mark`(executor.rs)から `scheduler::mark` が呼ばれ、
  全 Thread オブジェクト(→ `ThreadInner::mark` → 各スレッドの Executor の CFP チェーン)
  をマークする。
- **main のフレーム**: GC のトリガが green thread 側だと、main のフレームは current の
  チェーンから辿れない。`scheduler_run` / `pass` は実行中 `main_exec` ポインタを公開し、
  `in_scheduler` フラグが立っている間だけそれを deref してマークする。
- 切替はセーフポイントのみなので、サスペンド中のどのスレッドのフレームも GC-complete。

## 4. Thread API の実装状況

ネイティブ(`builtins/thread.rs`):
`Thread.new/start/fork`(本体はキューされ、どこかのスレッドが最初にブロックした時点で
初実行される)、`Thread.current/main/list/pass/stop`、
`#join(timeout)` / `#value`(終了例外を再 raise)、`#status`("run" / "sleep" / false / nil)、
`#alive?` / `#stop?`、`#wakeup` / `#run`、`#raise` / `#kill` / `#exit` / `#terminate`、
`Thread.kill(th)` / `Thread.exit`。

Ruby 側(startup.rb): `#name`、fiber-local(`[]` / `[]=`)、
`#thread_variable_get/set`、`#report_on_exception`(インスタンスのみ)、
`Thread.handle_interrupt`(マスキング未実装のため単に yield)、
`Thread::Waiter`(`Process.detach` 用。native `Thread.new` はブロック必須なので
`allocate` ベースで生成)。

`Kernel#sleep` は他に生存スレッドがいるときだけスケジューラ経由になる
(無引数 sleep は `#wakeup` まで park)。単独スレッド時は従来の nanosleep ループ。

## 5. 同期プリミティブ(純 Ruby)

Mutex / Queue / SizedQueue / ConditionVariable は **startup.rb の純 Ruby 実装**。
ネイティブコードはない。正しさの根拠は §0 のアトミック性:
「条件検査 → waiter 配列へ self を追加 → park」の列は
間に他スレッドが挟まらないので、check-then-act 競合が構造的に存在しない。

- park は `Thread.stop`(または timeout 付きは `Kernel.sleep`)、
  unpark は `Thread#wakeup` をそのまま使う。
- `Mutex#sleep` / `ConditionVariable#wait` は unlock → park → **ensure で re-lock**。
  `#kill` / `#raise` が park 中に配送されても(§6 の unwind は ensure を実行するので)
  mutex は正しく再取得・解放される。
- `Queue#close` は全 waiter を起こす(consumer は残要素を排出後 nil、
  producer は `ClosedQueueError`)。`ClosedQueueError < StopIteration` は startup.rb で定義。
- デッドロックは §3.3 のスケジューラ検出に自然に乗る。

## 6. 非同期割り込み(`Thread#raise` / `#kill`)

割り込みは対象の `ThreadInner.pending` にキューされ、スケジューラが配送する:

- **park 中の対象**: 起こして(Runnable 化)、dispatch 時に
  「park していた Executor に `set_error` してから `scheduler_resume(exec, 0)`」。
  park 側の `park_switch` は戻り値 `None`(=0)を見て `Err(vm.take_error())` を返すので、
  例外は**ブロックしていたまさにその地点**から unwind する(ensure 実行)。
  この「0-resume = エラー再開」が予約済みの配送経路。
- **kill の unwind**: `Throw`(タグは新規生成した Object なのでどの `catch` にも
  一致しない)として配送する。monoruby の unwinder は `Throw` を rescue 節
  (`rescue Exception` 含む)を素通しにしつつ ensure 節を実行する —
  これは CRuby の kill の意味論と一致する。スレッド root まで到達したら
  `killed` フラグにより「クリーンな死」(status false、join は正常返り、
  report_on_exception 出力なし)として finalize される。
  ※ `FatalError` は ensure をスキップするため使えない。ユーザーの
  「uncaught throw」は throw サイトで `UncaughtThrowError` に変換されるので、
  スレッド root に `Throw` が素で届くのは kill 配送だけ。
- **自分自身が対象**: その場で raise(kill なら kill-unwind、main の kill は
  `SystemExit` = プロセス終了)。
- **未起動(Created)の対象**: 本体を実行せずに死ぬ。
- **park 中の main**: `scheduler_run` から戻った直後に `take_main_pending()` が配送する。

## 7. non-blocking IO(fd ポーラ統合)

前提として、シグナル対応の際に全ブロッキング IO は `blocking_region`
(EINTR → VM ポーリング → 再開)という単一のチョークポイントに集約されている
(SA_RESTART なしのシグナルハンドラ、EINTR を握り潰さない独自 read/write プリミティブ)。
グリーンスレッド統合はこの上に載る:

- **`blocking_io_region(vm, globals, io, events, f)`**(builtins/io.rs):
  fd を扱う 13 のビルトイン(read 系 / write 系 / `IO.copy_stream` の両側)を包む。
  1. 他に生存スレッドがいなければ従来どおり(本当にブロックする)。
  2. read 系は先にバッファ(ungetc pushback / BufReader 内残データ)を確認し、
     あれば fd を見ずに実行(バッファがあるのに fd 未 ready で park すると自己デッドロック)。
  3. ゼロタイムアウト `poll` で readiness を確認し、未 ready なら
     `scheduler::wait_fd(fd, events)` で park(§3.3)。起床後に再検査。
  4. ready なら操作本体 `f()` を実行(直前に他スレッドは走れないので、
     readiness が横取りされる競合はない)。
- **スケジューラ側**: `io_waiters` に登録された fd 群をアイドル時に一括 `poll(2)` し、
  revents が立ったスレッドを起こす(POLLERR/HUP/NVAL も起床 → 本体が実 errno を出す)。
  `#kill` / `#raise` は fd 待ちのスレッドも起こす。
- **`IO.select`**: グリーンパスでは与えられた全 fd を `wait_fds` で待つ
  (timeout は deadline として sleepers に併載)。非 IO オブジェクトは `#to_io` で変換。
  全セット空 + timeout なしは CRuby 同様「status "sleep" で永眠(kill/wakeup 可能)」。
  単独スレッド時は従来の `select(2)`(EINTR → ポーリング → 再試行)。

### 制限(重要)

**ガードされるのは操作の入口だけ。** 操作が「利用可能なデータを消費し、さらに要求する」
場合(例: パイプ上の `read(n)` で n が到着済みバイト数を超える、行が複数チャンクに
分かれて届く `gets`)、2 チャンク目以降の待ちはプロセス全体をブロックする。
完全な解決は fd の nonblocking 化 + EWOULDBLOCK での park(将来課題)。
ruby/spec の典型パターン(空パイプで park → 相手スレッドが一括書き込み)はカバーされる。

## 8. 既知の制限と今後

1. **`Thread.handle_interrupt` マスキング未実装**(現状は yield するだけ)。
   割り込み配送(§6)に per-thread マスクスタックを足せば実装可能。
2. **タイムスライス・プリエンプションなし**: busy-loop するスレッドは
   `Thread.pass` を呼ばない限り CPU を譲らない。ruby/spec のアクティブな例は
   すべて協調型(`loop { Thread.pass }`)なので spec 上は問題にならないが、
   実用上は watchdog(既存の SIGALRM + setitimer)から alloc_flag を突いて
   次のポーリング地点で強制 yield する拡張が自然。
3. **mid-operation の IO ブロック**(§7 の制限)。
4. **シグナルは「ポーリングしたスレッド」で変換される**(CRuby は main に配送)。
5. `Thread.new` のサブクラスは Ruby の `initialize` オーバーライドを実行しない。
6. `Thread#priority` は保存のみ(スケジューリングに影響しない)。
   `ThreadGroup` / `native_thread_id` / `fork` との相互作用は未実装。
7. 真の並列化は別の話(Ractor 型の分離が現アーキテクチャ —
   OS スレッドごとの ALLOC / CODEGEN / SCHEDULER — と整合的)。

## 9. テスト

- `builtins/thread.rs` の `#[cfg(test)]`: CRuby 4.0.2 との差分テスト約 20 群
  (インターリーブ順序、status ポーリング、kill/raise 意味論、同期プリミティブ、
  thread+IO の 5 パターン、IO.select エッジ)。
- ruby/spec: `bin/spec` またはリポジトリ外の spec/mspec で
  `core/thread` / `core/mutex` / `core/queue` / `core/sizedqueue` /
  `core/conditionvariable` / `core/io` を実行。
  かつてスペックランナーをハングさせた `core/io/copy_stream_spec.rb` と
  `core/io/select_spec.rb` は完走・全パスする(ハング用の `spec/tags` は削除済み)。
