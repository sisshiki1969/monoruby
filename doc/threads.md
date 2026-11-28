# Thread / Fiber / non-blocking IO / プリエンプション の実装

monoruby のスレッド機構の現状をまとめる。
対象読者はランタイムの実装に手を入れる人。関連ソース:

| ファイル | 内容 |
|---|---|
| `monoruby/src/scheduler.rs` | スレッド registry、割り込み、終了処理(§3・§6) |
| `monoruby/src/scheduler/native.rs` | カーネルスレッドの起動・park・GVL の受け渡し(§12) |
| `monoruby/src/gvl.rs` | Global VM Lock |
| `monoruby/src/preempt.rs` | タイムスライス・プリエンプションのタイマ(§8) |
| `monoruby/src/native_pool.rs` | fd で待てないカーネルブロッキング操作(`NativeOp`、§9) |
| `monoruby/src/value/rvalue/thread.rs` | `ThreadInner`(スレッド制御ブロック)と状態機械 |
| `monoruby/src/value/rvalue/fiber.rs` | `FiberInner`(Fiber 制御ブロック) |
| `monoruby/src/builtins/thread.rs` | Thread クラスのネイティブ・ビルトイン |
| `monoruby/src/builtins/fiber.rs` | Fiber クラスのビルトイン + JIT インライン `Fiber.yield` |
| `monoruby/builtins/thread.rb` | Mutex / Queue / SizedQueue / ConditionVariable(純 Ruby)、Thread の簿記系メソッド |
| `monoruby/builtins/monitor.rb` | Monitor(純 Ruby、§5) |
| `monoruby/src/builtins/io.rs` | `blocking_io_region` / `IO.select` のスレッド対応パス |

関連ドキュメント:
[scheduler_state_diagram.md](scheduler_state_diagram.md) —
Thread / Fiber の状態遷移図(mermaid + SVG)と遷移⇔実装対応表。

## 0. 全体像

- **1:1 + GVL**。Ruby の `Thread` 1 つにカーネルスレッド 1 つ(§12)。インタプリタの
  共有状態(ヒープ、`Globals`、JIT、インラインキャッシュ)に触れるのは Global VM
  Lock(`gvl.rs`)を持つスレッドだけなので、一度に Ruby を走らせるのは 1 スレッド。
  fd で待てないカーネル待ちも、呼び出したスレッドが GVL を手放して実行する(§9・§12.7)。
- **GVL の受け渡しの 2 系統**:
  1. **ブロッキング地点**。`sleep` / `Thread.stop` / `#join` / `Thread.pass` /
     ブロックする IO / 同期プリミティブの待機で、実行中のスレッドが GVL を手放す。
  2. **タイマ駆動プリエンプション**(§8)。生存スレッドが 2 本以上あるとき、
     10 ms ごとに専用タイマ OS スレッドが GC poll フラグにプリエンプトビットを立て、
     走行中のスレッドが次のセーフポイント(callee-entry / ループバックエッジ)で
     `Thread.pass` 相当(GVL を先頭の待ち手へ渡す)を行う。busy-loop するスレッドも
     CPU を譲る。
- **どちらの受け渡しも VM のセーフポイントでしか起きない**。この不変条件が設計全体を貫く:
  - GVL の解放は必ずビルトイン内のブロッキング地点か、GC が起きうるのと同じ
    セーフポイントで起きる。よって GVL を持たないスレッドのフレームは常に
    GC-complete(§3.1)。プリエンプションが安全なのはこのため — 「GC が起きうる
    場所でしか手放さない」ので register write-back を含めて GC と全く同じ扱いになる(§8)。
  - ビルトインは他スレッドに対して**アトミック**(ビルトイン内では自分のブロッキング/
    poll 地点以外で GVL を手放さない — CRuby が C 関数に与える保証と同じ)。
  - ただし**純 Ruby コードのアトミック性はプリエンプションで失われた**。2 つの
    非ブロッキング文の間に他スレッドが割り込みうる。純 Ruby の Mutex / Queue /
    ConditionVariable は park permit とロックで書いてある(§5)。

## 1. Fiber(前提となる既存機構)

Fiber はスレッドの中でスタックを切り替える機構。スレッドとは独立だが、
スレッド root の扱い(§2)と GC(§3.1)で関わる:

- `FiberInner { handle: Box<Executor>, proc: Proc, stack: Option<NonNull<u8>> }`
- 各 Fiber は **256 KiB の専用マシンスタック**(最下位ページは `mprotect(PROT_NONE)` の
  ガードページ)と、自分専用の `Executor`(VM コンテキスト: cfp、エラー情報、`$~`/`$_` など)を持つ。
- コンテキストスイッチは `Executor::rsp_save` フィールドの **rsp 交換**で実現する。
  スタブ(`fiber_invoker` / `resume_fiber` / `yield_fiber`)は callee-saved レジスタを
  スタックに退避してから rsp を差し替える。
- Fiber の状態は `rsp_save` から導出される:
  `None` = Created、`-1` = Terminated、それ以外 = Suspended。
  状態遷移図は [scheduler_state_diagram.md §2](scheduler_state_diagram.md#2-fiber-の状態遷移)
  ([SVG](fiber_state_diagram.svg))。
- **`parent_fiber` チェーン**: resume した側が子の `parent_fiber` に記録され、
  `Fiber.yield` はそこへ戻る(非対称コルーチン)。
- GC は保守的スタックスキャンを行わない。サスペンド中の Fiber のフレームは
  `FiberInner::mark` → 子 `Executor` の CFP チェーン歩行で**精密に**マークされる。
  そのために JIT インライン `Fiber.yield` は切替前に write-back(`exec_gc`)を発行し、
  フレームを GC-complete にしてから切り替える。

## 2. Thread の構造

`ThreadInner`(`value/rvalue/thread.rs`、`ObjTy::THREAD` / `THREAD_CLASS = 59`)は
RValue セルに収まらないため union 内では `Box` 化されている:

```
handle:        Option<Box<Executor>>   // main スレッドのみ None(Executor は埋め込み側所有)
proc / args:   本体ブロックと Thread.new の引数
state:         Created | Runnable | Sleeping | Joining | IoWaiting | Blocking | Dead
resume_exec:   GVL を手放している間の実行コンテキスト(スレッド root、またはスレッド内で park した nested Fiber)
result / exception:  終了結果(#join / #value が参照)
joiners:       このスレッドを #join で待っているスレッド
pending:       未配送の非同期割り込み(Kill | Raise(err))
killed:        kill 配送済みフラグ(終了時の unwind をクリーンな死として扱う)
masks:         Thread.handle_interrupt のマスクスタック(§6)
park_permit:   park permit。running な対象への #wakeup/#run が立て、次の park が即戻る(§5・§8)
last_status:   $? / Process.last_status をスレッドごとに保持(#972)
parker / park_indefinite / park_fds:  park の wake パイプと状態(§12.3・§12.4)
blocking_tid / blocking_ubf:  GVL を手放したカーネル待ちへの割り込み先(§12.7)
native_tid:    Thread#native_thread_id
```

`state` の状態遷移図は
[scheduler_state_diagram.md §1](scheduler_state_diagram.md#1-thread-の状態遷移)。

重要な設計判断: **スレッド root の `parent_fiber` は常に `None`**。
このため `Fiber.yield` をスレッド本体で呼ぶと、main fiber と同じ既存のエラー経路
(Rust 側 / JIT インライン側とも)が正しく FiberError を出す。

注: `priority` / fiber-local / thread-variable は `ThreadInner` のフィールドではなく、
thread.rb の純 Ruby 側で隠しインスタンス変数(`@__priority` / `@__fiber_locals` /
`@__thread_variables` 等、`doc/hidden_ivars.md`)として実装される(§4)。

## 3. スレッド registry

`src/scheduler.rs`。インタプリタ・インスタンス(`vm::Vm`、`src/vm.rs`)が持つ
`SCHEDULER: RefCell<Scheduler>`。GVL のホルダだけが触るので `RefCell` で足りる
(借用を GVL の解放をまたいで保持しない・借用中に Ruby アロケーションをしない、が規律)。

```
threads:          生存スレッドの registry(main 含む)— GC ルート
current / main:   current は GVL ホルダ(acquire のたびに書き換える)
main_exec:        main が GVL を手放す直前に公開する root Executor(GC 用、§3.1)
pending_reports:  遅延した report_on_exception のテキスト(終了処理で生成し後で flush)
pending_unblocks: fiber scheduler 経由の joiner への unblock の積み残し
flushing_reports: flush_pending_reports の再入ラッチ
native_joins:     カーネルスレッドの JoinHandle(terminate_all で join)
terminating:      terminate_all が kill 中のスレッド(GC ルート、#2027)
ignore_deadlock:  Thread.ignore_deadlock
```

起動・park・GVL の受け渡しは `scheduler/native.rs`(§12)。`scheduler.rs` の
エントリポイント(`spawn` / `sleep_woken` / `pass` / `join` / `wait_fds` /
`without_gvl` / `terminate_all` / `fork_child_reset_threads`)はそこへ委ねる。

**本体の終了**: `native::run_body` は本体ブロックの戻り直後に、スレッド自身の
コンテキストで終了フック(`scheduler::thread_body_exit`)を走らせる — 握ったままの
Mutex を解放し(`Thread#__unlock_on_exit`)、fiber scheduler 経由で `#join` している
fiber に `unblock` を送る(CRuby の `thread_start_func_2` 末尾に相当。Ruby を実行するので
park もありうる)。その後 `finalize` が結果 / 例外を記録し、joiner を起こし、registry から
外し、report_on_exception をキューし、必要なら main へ例外を転送する。

### 3.1 GC との統合

- **registry が GC ルート**: `Root::mark`(executor.rs)から `scheduler::mark` が呼ばれ、
  全 Thread オブジェクト(→ `ThreadInner::mark` → 各スレッドの Executor の CFP チェーン)
  をマークする。詳細は `doc/gc.md` §8。
- **main のフレーム**: GC のトリガが main 以外だと、main のフレームは current の
  チェーンから辿れない。main は GVL を手放す直前に root Executor を `main_exec` に
  公開し、main が current でない間だけそれを deref してマークする(§12.5)。
- GVL の解放はセーフポイントのみなので、GVL を持たないどのスレッドのフレームも
  GC-complete。

## 4. Thread API の実装状況

ネイティブ(`builtins/thread.rs`、`init`):
`Thread.new/start/fork`(カーネルスレッドを起動し、生成直後の `Thread.pass` で
最初のタイムスライスを渡す)、`Thread.current/main/list/pass/stop`、
`Thread.kill(th)` / `Thread.exit`、`Thread.handle_interrupt` / `Thread.pending_interrupt?`、
`#join(timeout)` / `#value`(終了例外を再 raise)、`#status`("run" / "sleep" / false / nil)、
`#alive?` / `#stop?`、`#wakeup` / `#run` / `#__wakeup_permit`(§5)、
`#raise` / `#kill` / `#exit` / `#terminate`、`#pending_interrupt?`。

`Thread.handle_interrupt` は**ネイティブで機能する**:マスク(`(例外クラス, タイミング)` の列)を
`ThreadInner::masks` へ push/pop し、マスク境界で保留割り込みを配送する(§6)。

Ruby 側(thread.rb、`class Thread`):
- `#name`
- fiber-local `[]` / `[]=` / `key?` / `keys` / `fetch`(`@__fiber_locals` に格納)
- `#thread_variable_get` / `_set` / `#thread_variable?` / `#thread_variables`(`@__thread_variables` に格納)
- `#priority` / `#priority=`(-3..3 にクランプして保存するのみ。実際の
  スケジューリングには影響しない。`@__priority`)
- `#native_thread_id`(走っているカーネルスレッドの id。Linux は `gettid(2)`、Darwin は
  `pthread_threadid_np`。死後は `nil`。各スレッド固有)
- `#report_on_exception`(インスタンスのみ)
- `Thread.ignore_deadlock` / `=`(真偽値に丸めて `Scheduler::ignore_deadlock` へ。検出器を止める、§12.4)
- `Process::Waiter`(`Process.detach` 用。native `Thread.new` はブロック必須なので
  `allocate` ベースで生成)

`Kernel#sleep` は他に生存スレッドがいるときだけ park(GVL を手放す)になる
(無引数 sleep は `#wakeup` まで park)。単独スレッド時は従来の nanosleep ループ。

## 5. 同期プリミティブ(純 Ruby)+ プリエンプション下での正しさ

Mutex / Queue / SizedQueue / ConditionVariable は **thread.rb の純 Ruby 実装**で
ネイティブコードはない。プリエンプション(§8)があるので、2 つの非ブロッキング文の
間にも他スレッドが割り込みうる。そこで次の 2 機構で正しさを保っている
(thread.rb のコメントブロック参照):

1. **test-and-set はセーフポイントのない一直線コードにする**。判定の前に
   `Thread.current` 等の呼び出しを巻き上げておき、判定〜フラグ設定の間にセーフポイント
   (=切替点)を挟まない(`Mutex#try_lock`)。ただし callee-entry poll がある以上、
   メソッド呼び出しを巻き上げても**複数呼び出しの列**をアトミックにはできない
   (どの呼び出しもセーフポイント)。複合的な状態遷移にはロックが唯一の正しい道具。
2. **park permit で lost-wakeup を塞ぐ**。*running*(park 中でない)スレッドに対する
   `Thread#wakeup`/`#run` は対象の `park_permit`(`ThreadInner`)を立て、対象の次の park は
   即座に戻る。これで「waiter として登録 →(プリエンプトされ、起こす側が走って
   まだ running な対象を wake)→ 永遠に park」という古典的な lost-wakeup 窓が閉じる。
   全 park 地点はリトライループの中にあるので、早期復帰しても条件は再検査される。
   純 Ruby からは `Thread#__wakeup_permit` を使う(公開 `#wakeup` は permit なし版で、
   running を起こしても no-op という CRuby 意味論に一致)。

その他:
- park は `Thread.stop`(または timeout 付きは `Kernel.sleep`)、
  unpark は上記 `#wakeup` / `#__wakeup_permit` を使う。`Mutex#sleep` の timeout 付き park は
  `Thread.__sleep(sec)`: `Kernel#sleep` と同じ待ちで、戻り値が「起こされた(true)/ 時間切れ
  (false)」(`scheduler::sleep_woken`、`park` の戻り値)。`Mutex#sleep` は CRuby(`rb_mut_sleep`)と同じく時間切れなら
  nil、起こされたら経過秒を返し、`ConditionVariable#wait` は引数の `sleep` の戻り値をそのまま返す。
- `Mutex#sleep` / `ConditionVariable#wait` は unlock → park → **ensure で re-lock**。
  `#kill` / `#raise` が park 中に配送されても(§6 の unwind は ensure を実行するので)
  mutex は正しく再取得・解放される。スレッドが握ったまま終了したロックは
  終了フック(§3、`Thread#__unlock_on_exit`)が解放して waiter を起こす
  (CRuby の `rb_threadptr_unlock_all_locking_mutexes`)。そのためにスレッドは保持中の
  Mutex を `Mutex#__held_next` で連結したリスト(`Thread#__held`、CRuby の
  `keeping_mutexes`)として持ち、`try_lock` / `unlock` が push / pop する(属性アクセサだけで
  済ませ、ホットパスに呼び出しを増やさない)。起動前に kill されたスレッド等のための
  保険として、所有者が死んでいるロックは次の取得者が回収する。`Mutex#owned?` は
  **Fiber 単位**の所有で判定する。
- **fiber scheduler**(`Fiber.set_scheduler`)が入っていて現在の fiber が non-blocking なら、
  park はスケジューラ経由になる(CRuby の fiber scheduler プロトコル): `Mutex#lock` は
  `block(mutex, nil)`、Queue / SizedQueue の待ちは `block(queue, timeout)`、
  `Mutex#sleep`(= `ConditionVariable#wait`)は `kernel_sleep(timeout)`。waiter 配列には
  Thread の代わりに `Thread::SchedulerWaiter`(fiber + blocker)が入り、`#__wakeup_permit`
  を受けるとスケジューラの `unblock(blocker, fiber)` に変換する(`Mutex#unlock` /
  `ConditionVariable#signal` 側は変更なし)。`Thread#join` も同様に `block(thread, 残り秒)`
  で待ち、対象スレッドの終了フックが `unblock(join 側のスレッド, fiber)` で答える
  (`ThreadInner::fiber_joiners`)。
- 子プロセスの待ちも同じプロトコルに乗る(#2211)。non-blocking fiber でスケジューラが
  `#process_wait(pid, flags)` を持つなら、`Process.wait` / `wait2` / `waitpid` / `waitpid2` /
  `waitall`、`Process::Status.wait`、`Kernel#system`、
  バッククォート、popen した IO の `close` は、`waitpid(2)` を自分で呼ばずにこのフックに
  渡す(CRuby の `rb_process_status_wait` → `rb_fiber_scheduler_process_wait`)。
  フックは任意(`rb_check_funcall` 相当)で、`WNOHANG` 付き・blocking fiber・スケジューラ
  なしのときは呼ばない。答えの読み方は呼び出し側ごとに CRuby と同じ:
  `Process::Status.wait` は無検査でそのまま返し、`system` は `Process::Status` 以外
  (`nil` も)なら TypeError(`rb_check_typeddata`)。`Process.wait` 系・`close`・
  バッククォートは `rb_waitpid` の読み方で、`nil` は「何も回収しなかった」(`$?` は消えた
  まま、`Process.wait` は nil を返す)、それ以外の非 Status だけが TypeError。pid が -1 の
  Status は子ではなく、失敗した `waitpid(2)` の errno を運ぶ(子がいなければ `ECHILD`、
  フラグが不正なら `EINVAL`。`Process::Status.wait` 自身は raise せずこれを返し、
  `Process.wait` 系がその errno を raise する — #2222)。`IO.popen { }` の close でフックが
  raise すると、ブロックが raise していてもフックの例外が残る(CRuby は `rb_ensure` から
  閉じる)。`system` は `Process.wait(spawn(...))`、バッククォートは
  `IO.popen(cmd) { |io| io.read }` と同じ経路(`builtins::spawn`)で子を作る(#2221)ので、
  バッククォートは stdout のパイプを読み切ってからフックに渡す(stderr は子がそのまま継承)。
  待ちを自分で行う場合は GVL を手放すので、子が走っている間も他スレッドは動く。
- **Monitor**(`builtins/monitor.rb`)は CRuby の `ext/monitor/monitor.c` を Ruby で書いたもので、
  Mutex + 所有 **Fiber**(`@__owner`)+ 進入回数(`@__count`)。`@__count` を読み書きするのは所有者
  だけ、`@__owner` が変わるのは mutex を握っている間だけなので、この 2 つに別のロックは要らない。
  `MonitorMixin` と `MonitorMixin::ConditionVariable` は CRuby の `ext/monitor/lib/monitor.rb`
  そのまま(`wait` は `Monitor#wait_for_cond` 経由で `Thread::ConditionVariable#wait(@mutex, timeout)`。
  回数ごと手放し、戻るときに同じ回数で取り直す)。注意点は最外側の `exit`: 「`@__owner = nil`」と
  「`Mutex#unlock`」の間(= `unlock` の callee-entry poll)で `Thread#raise` / kill が配送されると、
  mutex は握ったままなのに monitor は無所有という状態が残る(同じスレッドの次の `enter` が
  "deadlock; recursive locking"、他スレッドは永久待ち)。そこでこの 2 つを
  `Thread.__uninterruptible_begin` / `__uninterruptible_end`(`Thread.__uninterruptible { }` の
  ブロックなし版。区間が短く、ブロック呼び出しが区間のコストの大半になる場所用)で挟む。
  どちらもネイティブ関数なので区間に入るまでの間にも配送点はない。`enter` 側は
  `try_lock || lock` が戻ってから `@__owner` を書くまでに poll がないので保護不要。
  mutex は初回使用時に作る(CRuby はアロケータで作るので `super` を呼ばないサブクラスでも動く)。
- Queue / SizedQueue は Mutex + ConditionVariable の上に載る(CRuby thread_sync.c と同じ構造)。
  すべての check-and-take をキューの mutex 下で行うので、「空か調べて取り出す」の間の
  セーフポイント窓(2 つの pop が競合し片方が幻の nil を得る)は相互排他で閉じている。
- 待機ループは `loop do` ではなく **`while true`** を使う: `Kernel#loop` は StopIteration を
  握り潰し、`ClosedQueueError < StopIteration` なので `loop` ブロック内の
  `raise ClosedQueueError` が黙って飲まれてしまう。
- `Queue#close` は全 waiter を起こす(consumer は残要素を排出後 nil、
  producer は `ClosedQueueError`)。`ClosedQueueError < StopIteration` は thread.rb で定義。
- デッドロックは §12.4 の検出に自然に乗る。

## 6. 非同期割り込み(`Thread#raise` / `#kill`)

割り込みは対象の `ThreadInner.pending` にキューされ、対象スレッド自身が配送地点
(`deliver_pending_now`)で受け取る:

- **park 中の対象**: `wake_parked` で起こす(wake パイプに 1 byte)。対象は `park` から
  戻り、`sleep` / `join` / `wait_fds` の出口の `deliver_pending_now` が `Err` を返すので、
  例外は**ブロックしていたまさにその地点**から unwind する(ensure 実行)。
- **GVL を手放したカーネル待ちの対象**: `interrupt_blocking` がそのカーネルスレッドに
  `SIGVTALRM` を送り、syscall を `EINTR` で戻す(§12.7)。
- **走行中・GVL 待ちの対象**: プリエンプション(§8)が対象を次のセーフポイントで
  `pass` に落とし、その出口で pending が配送される。busy-loop 中のスレッドにも
  `#kill` / `#raise` が届く。
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
- **`Thread.handle_interrupt`**: マスク(`ThreadInner::masks`)にマッチする割り込みは
  即配送せず保留し、マスク境界(handle_interrupt ブロックの出入り)で配送する。

## 7. non-blocking IO(fd ポーラ統合)

前提として、シグナル対応の際に全ブロッキング IO は `blocking_io_region`
(EINTR → VM ポーリング → 再開)という単一のチョークポイントに集約されている
(SA_RESTART なしのシグナルハンドラ、EINTR を握り潰さない独自 read/write プリミティブ。
fd を持たない旧 `blocking_region` は全サイトが `blocking_io_region` に統合され削除)。
スレッド対応はこの上に載る:

- **`blocking_io_region(vm, globals, io, events, f)`**(builtins/io.rs):
  fd を扱う 13 のビルトイン(read 系 / write 系 / `IO.copy_stream` の両側)を包む。
  1. 他に生存スレッドがいなければ従来どおり(本当にブロックする)。
  2. read 系は先にバッファ(ungetc pushback / BufReader 内残データ)を確認し、
     あれば fd を見ずに実行(バッファがあるのに fd 未 ready で park すると自己デッドロック)。
  3. ゼロタイムアウト `poll` で readiness を確認し、未 ready なら
     `scheduler::wait_fd(fd, events)` で park(§12.3)。起床後に再検査。
  4. ready なら操作本体 `f()` を実行(GVL を持ったままなので、直前に他スレッドは
     走れず、readiness が横取りされる競合はない)。
- **park 側**: `native::park` が自分の wake パイプと待ち fd 群を `poll(2)` する
  (POLLERR/HUP/NVAL でも戻り → 本体が実 errno を出す)。`#kill` / `#raise` は
  wake パイプ経由で fd 待ちのスレッドも起こす。
- **`IO.select`**: 他スレッドがいるときは与えられた全 fd を `wait_fds` で待つ
  (timeout は park の deadline)。非 IO オブジェクトは `#to_io` で変換。
  全セット空 + timeout なしは CRuby 同様「status "sleep" で永眠(kill/wakeup 可能)」。
  単独スレッド時は `select(2)`(EINTR → ポーリング → 再試行)。fd 集合は最大 fd に
  合わせて伸びるビット列(`FdSet`、CRuby の `rb_fdset_t` 相当)で、`FD_SETSIZE` の
  上限はない(macOS は `select$DARWIN_EXTSN`)。
  - 3 つの Array は添字で辿り、長さを毎回読み直す(`#to_io` が Array を変え得る)。
    待つ前に持ち出すのは fd の整数だけで、戻り値は待ったあとに Array をもう一度
    辿って(各要素を再変換して)作る。CRuby の `select_internal` と同じ 2 回走査で、
    待っている間に他スレッドやトラップが Array を変えても、消えた要素には触れない。
  - 何が ready かは他スレッドがいるときも `select(2)` が決める(CRuby と同じ)。
    他スレッドがいるときはゼロタイムアウトの `select(2)` で判定し、何もなければ `wait_fds`
    (`poll(2)`)で眠って、起きたら判定し直す。`poll(2)` の revents をそのまま答えに
    しないのは、hangup / error を read・write・error のどれに数えるかが `select(2)` と
    `poll(2)` で違い、OS でも違うため(macOS の `poll(2)` は `POLLHUP` と一緒に
    `POLLPRI` を返し、macOS の `select(2)` は終了したパイプの読み側を書き込み可能と
    する、など)。`poll(2)` が何か返すのに `select(2)` が ready としない fd は待ち対象
    から外し(外さないと空回りする)、以後は 100 ms ごとに `select(2)` を聞き直す。
- **ソケット**: TCP の `connect` は非ブロッキング発行(`EINPROGRESS`)→ `POLLOUT` を
  `wait_fd` で待って `SO_ERROR` 確認、`accept` はリスナ fd を恒久 non-blocking にして
  `POLLIN` の park-retry ループで受ける(いずれも fd で park)。DNS(`getaddrinfo`)は
  GVL を手放して実行する(§12.7)。
- **poll できないカーネルブロッキング**(flock、FIFO の open)は GVL を手放して
  実行する(§9)。

### mid-operation の would-block エミュレーション

入口の readiness チェックだけでは、「利用可能なデータを消費し、さらに要求する」操作
(例: パイプ上の `read(n)` で n が到着済みバイト数を超える、行が複数チャンクに分かれて
届く `gets`、パイプ容量を超える `write`)の 2 チャンク目以降が GVL を持ったまま
ブロックし、他の全スレッドを止める。
これを防ぐため、他に生存スレッドがいる間は `f()` の実行中だけ fd を一時的に
`O_NONBLOCK` にする(`NonblockGuard`、drop で元のフラグへ復元):

- ブロックするはずだったカーネル突入は `EAGAIN` を返し、read/write プリミティブは
  消費済みバイトを pushback に戻して内部マーカー `MonorubyErr::would_block_interrupt`
  を浮上させる(シグナルの `signal_interrupt` マーカーと同型の配管)。
- `blocking_io_region` がマーカーを捕捉し、fd のモードを復元してから
  `scheduler::wait_fd` で park → ready 後に操作を再開(pushback から再読するので
  データは失われない。write は `*progress` が書けた分を記録しているので重複しない)。
- 生存スレッドがいないのにマーカーが浮上した場合(`read_nonblock` /
  `write_nonblock` が恒久的に nonblocking 化した fd)は、シグナル割り込み可能な
  素の `poll(2)` で待つ(CRuby も nonblocking fd 上のバッファド IO はブロックする)。
- 標準ストリーム(fd 0–2)はガード対象外: open file description を親シェルと
  共有しており、異常終了でフラグが漏れると外側の IO を壊すため
  (古典的な「nonblocking stdout」問題)。入口 park のみでカバーする。

## 8. タイムスライス・プリエンプション(`preempt.rs`)

協調型だけでは、busy-loop するスレッドが `Thread.pass` を呼ばない限り CPU を独占し、
他スレッドの飢餓・`Thread#kill` 未配送・mspec の `--timeout` watchdog スレッドが
永久に走らない、といった公平性の問題が残る。プリエンプションはこれらを、GC・JIT
バックエンド・スレッドローカルシングルトンに一切触れずに解消する。

プリエンプションは正確に **「全スレッドが次のセーフポイントで `Thread.pass` を
呼んだかのように」** 振る舞う — `Thread.pass` と同じ GVL の受け渡しを再利用する。

### 8.1 タイマ

- **10 ms tick**(`TICK`)の専用タイマ OS スレッド。**生存スレッドが 2 本以上ある間だけ**
  走る(`on_thread_count(live)` が `live >= 2` で起動、`live < 2` で停止)。単独スレッドの
  プログラムはタイマを 1 本も生やさずコストゼロ。
- `MONORUBY_NO_PREEMPT` / `MONORUBY_PREEMPT_STRESS` が設定されているとタイマは起動しない。
- 毎 tick、`flag_addr` の mutex を取ってから poll ワードの PREEMPT レーンを `fetch_or`。
  `Codegen::drop`(`codegen_dropped`)が同じ mutex 下で `flag_addr` を 0 に落とすので、
  タイマが解放済み JIT メモリを触ることはない(マルチインタプリタのテストハーネス対策)。

### 8.2 レーンプロトコル

poll ワードは **GC・シグナルと同じ 1 つの `u32`** で、8bit×4 のレーンに分割される
(全体像は `poll_flag.rs` / `doc/safepoint.md` §3):

| byte | レーン | 書き手 |
|---|---|---|
| 0 | GC | アリーナのページ圧力 / malloc トリガ / `GC.start` |
| 1 | PREEMPT | プリエンプトタイマ |
| 2 | SIGNAL | シグナルハンドラ |

- 全書き込みは冪等なアトミック `fetch_or` / 自レーン byte のみの `fetch_and` clear。
  タイマ(別 OS スレッド)とシグナルハンドラ(async-signal 文脈)が同じ語に書いても、
  レーンが byte で分かれているため互いの更新を失わない。
- poll はワード全体の**ゼロ判定**(`cmpl …, 0; jne` / `ldr; cbz`)なので符号の罠もない
  (旧設計は符号付き `jge` の都合で PREEMPT がビット 30 に制限されていた)。

### 8.3 poll 配置

**callee-entry + ループバックエッジのみ。call-site poll は無い**(JVM/CRuby 方式)。

- **callee entry**(`vmgen/init_method.rs` = VM の `vm_init` / JIT の `InitMethod`):
  プロローグ直後、フレームがリンクされ `rsp` がその下、引数が rooted スロットに収まり、
  残レジスタが nil 詰めされた「最も安全な」位置で poll する。GC ルート走査が
  完全に整合したフレームを見る。全 dispatch 経路で一様に発火する。
- **call site**: スタックオーバーフローチェック(`CheckStack`)のみ残す。Rust invoker
  (`invoke_method` / `invoke_block`)は caller 側にルート化されていないヒープ Value を
  ローカルに握るので、caller 側で poll してはならない(汎用 `invoke_block` の caller-side
  poll が `File.open {}` を壊した実績あり)。この callee-entry poll のおかげで、
  Rust 側のイテレーションビルトイン(`Kernel#loop` / `Array#each` 等)もブロック本体が
  poll-free でも 1 反復ごとにプリエンプト・シグナル応答可能になる(ビルトインごとの
  監査は不要)。
- **ループバックエッジ**(`vm_loop_start`):同じく poll。
- aarch64 も対称に実装(`a64_op_init_method` / `a64_op_loop_start` が poll、call-site なし)。

### 8.4 `execute_gc` での消費

セーフポイントから呼ばれる `execute_gc`(executor.rs)の順序:

1. `watchdog::poll()`。
2. `let preempt = poll_flag::consume_preempt() || preempt::stress();` — PREEMPT レーンを
   消費する。
3. 保留シグナルを drain(エラーを立てて `None` を返しうる)。
4. GC レーンが立っているときだけ実際に GC(純プリエンプト tick は GC レーンに触れないので
   スキップ = 偽の full GC を起こさない)。
5. `stress_renudge()` — stress モードでは切替の**前に**レーンを再武装し、切替先スレッドも
   poll するようにする。
6. `if preempt && scheduler::preempt_ok() { scheduler::pass(vm, globals)? }`。
   `pass` の `Err`(kill/raise がこのスレッドに配送された)は `set_error` + `None` で浮上。

`preempt_ok()` = `main.is_some() && has_other_live_threads()`。

### 8.5 安全性(CRuby 互換)

- 切替は GC が起きうる地点でしか起きず、register write-back も GC と同一。
  よってサスペンド中のフレームは常に GC-complete。
- ビルトインは他スレッドに対してアトミック(自分の blocking/poll 地点以外で切り替わらない)—
  GVL 下で CRuby が C 関数に与える保証と同じ。
- JIT がローカルをレジスタにキャッシュしても他スレッドへ古値が漏れない: 他スレッドが
  フレームのローカルに触れるのは capture(ヒープ退避)経由のみで、JIT は capture された
  ローカルを必ずスタックスロットに書き戻す(block 渡しの call site は `locals_to_S`、
  ループ tier コンパイルは uncaptured 前提でガード、外側変数特殊化は `no_capture_guard`)。
  uncaptured フレームは他スレッドから到達不能なのでレジスタキャッシュは観測不能。

### 8.6 スイッチ

- `MONORUBY_NO_PREEMPT=1` — タイマを起動しない(協調型のみ)。
- `MONORUBY_PREEMPT_STRESS=1` — 全 poll 地点で切替を試みる(`gc-stress` のスケジューリング版。
  「ここで切り替わるはずがない」系の潜在状態バグを炙り出す拷問モード)。

## 9. fd で待てないカーネル待ち(`native_pool.rs`)

poll(2) で readiness を待てるもの(ソケット等)は fd で park するが(§7)、
**待つべき fd を持たないカーネルブロッキング操作** は、呼び出したカーネルスレッドが
GVL を手放して実行する(`native_pool::run_blocking` → `native::run_blocking`、§12.7)。
GVL を持ったまま実行すると、その間ほかの全スレッドが止まる。

- **対象**(`NativeOp`、生 fd / フラグ / `CString` パス / マーシャル済み C 引数だけを
  運び、Ruby ヒープに触れない):
  - `flock(2)` のブロッキング取得(`File#flock`)。`LOCK_NB` / `LOCK_UN` は
    カーネルでブロックしないのでインライン実行。
  - **FIFO** に対するブロッキング `open(2)`(相手が開くまでブロックする)。事前に
    `stat` して FIFO のときだけ GVL を手放し、それ以外の open はインライン。
  - `fcntl(2)` の**待つコマンド**(`NativeOp::Fcntl`): `F_SETLKW`(および Linux の
    `F_OFD_SETLKW`)は record lock が空くまでカーネルで待つ。`IO#fcntl` に String
    (packed `struct flock`)を渡す形だけが対象で、待たないコマンドは
    インライン実行のまま(`F_GETLK` は文字列バッファに書き戻すため、なおさら
    インラインでなければならない)。
  - **blocking 指定された FFI 呼び出し**(`NativeOp::Ffi`)。走るのは Ruby
    プログラムが選んだ任意の C なので、「生データのみ」の規律は fiddle 側で担保する:
    `FfiWorkerCall` は不死の descriptor のアドレスとマーシャル済み C 引数だけを持ち、
    `run` は Ruby ヒープに触れない。生の 64bit 結果を GVL を取り直した側が
    `bits_to_value` で box する。opt-in で、`Fiddle.___prepare` の flags
    (`2 = BLOCKING`)で宣言する。
  - 拡張の `call_blocking`(`NativeOp::Ext`)。
- **`EINTR`**: syscall はシグナル(main の trap、または割り込みの `SIGVTALRM`)で
  `EINTR` を返し、呼び出し側が GVL の下で poll point と `deliver_pending_now` を
  済ませてから再発行する(冪等な待ちなので)。FFI / 拡張は再発行しない(§12.7)。
- **fork(2) とプロセスグローバルなロック** —— 識別子テーブル
  (`id_table::ID`)、正規表現のコンパイルキャッシュ(`REGEX_CACHE` / `NATIVE_CACHE`)、
  標準ストリームのバッファ(`STDIN_BUF` / `STDOUT_BUF` / `STDERR_BUF`)、GVL の状態。
  Ruby スレッドはそれぞれカーネルスレッドで、複数のインタプリタを別 OS スレッドで走らせる
  embedder(テストハーネスはテストスレッドごとに 1 つ走らせる)もあるので、fork の瞬間に
  別スレッドがどれかを握っていることがある。実際に
  `forking_while_a_thread_keeps_offloading` の子が `File.open` → `IdentId::get_id` で
  止まった(識別子テーブルの `RwLock` を別スレッドが握っていた)。`crate::fork::prepare`
  がこれらすべてを固定順で、フォークするスレッド上で取る
  (どのパスもこれらを入れ子では取らないので、この順で他スレッドとデッドロックしない)。
  親は guard を drop、子は `ForkGuards::reset_child` でタイマの状態を捨てつつ
  解放する。`Process._fork` / `Process.daemon` / `do_spawn` の 3 箇所の `fork(2)` は
  すべてここを通る。`fork::tests` に、別 OS スレッドが intern し続ける中で 200 回 fork
  する再現テストがある(修正前は最初の 10 秒で子が止まる)。
  ただし子が受け継いだ `RwLock` を **unlock してはいけない**: Linux の std `RwLock` は
  futex 語で、子での unlock は store と誰も起こさない wake だが、Apple では queue lock
  (`std::sys::sync::rwlock::queue`)で、unlock は親の待機スレッドがスタックに残した
  ノードを辿って各スレッドを `dispatch_semaphore` で起こしにいく —— libdispatch は
  マルチスレッドプロセスの fork 子で trap する(darwin ランナーで、fork 時に待機者が
  いた最初の回に `SIGTRAP`)。そこで識別子テーブルと正規表現キャッシュは
  `fork::ForkableRwLock`(`AtomicPtr<RwLock<T>>` の間接参照)にし、子は自分が持つ
  guard 越しにデータを `mem::take` で取り出し、guard を `forget` して、新しい lock を
  差し込む。受け継いだ lock はロックされたまま leak し、二度と触らない
  (`preempt::ForkState` がタイマの mutex にしているのと同じ)。`Mutex`(ストリーム)は Apple では pthread、Linux では futex で、所有者による子での unlock は
  誰も起こさない普通の unlock なので、そのまま drop する。

## 10. 既知の制限と今後

1. `Thread#priority` は保存のみ(スケジューリングに影響しない)。`ThreadGroup` との
   相互作用は未実装。
2. 真の並列化は別の話(Ractor 型の分離が現アーキテクチャ —
   インタプリタ・インスタンスごとの ALLOC / CODEGEN / SCHEDULER — と整合的)。
   GVL の下で一度に走る Ruby スレッドは 1 つ。
3. `sqlite3_step` のように行単位で呼ばれる関数は blocking 指定しない(GVL の
   受け渡しが呼び出しごとに乗る)ので、busy_timeout 待ちの間は他スレッドが止まる。

(解決済み: `Thread.handle_interrupt` マスキング、mid-operation の IO ブロック(§7 の
would-block エミュレーション)、タイムスライス・プリエンプション(§8)、
カーネルブロッキング操作(§9)、`Thread` サブクラスの `initialize` オーバーライド、
park 中の main への trap ハンドラ配送(§12.8)。)

## 11. テスト

- `builtins/thread.rs` の `#[cfg(test)]`: CRuby 4.0.2 との差分テスト
  (インターリーブ順序、status ポーリング、kill/raise 意味論、同期プリミティブ、
  thread+IO の各パターン、IO.select エッジ、fiber-local / thread-variable、
  `native_offload_flock_and_fifo`(flock / FIFO open が呼び出したスレッドだけを
  ブロックし他スレッドを止めないことの検証)ほか)。
- ruby/spec: `bin/spec` またはリポジトリ外の spec/mspec で
  `core/thread` / `core/mutex` / `core/queue` / `core/sizedqueue` /
  `core/conditionvariable` / `core/io` を実行。
  `core/io/copy_stream_spec.rb` と `core/io/select_spec.rb` も完走・全パスする。

## 12. カーネルスレッドと GVL(`scheduler/native.rs`)

Ruby `Thread` 1 つにカーネルスレッド 1 つを割り当てる。

### 12.1 registry と GVL

- §3 の registry(`threads` / `main` / `current` / `main_exec` / `pending_reports`)は
  `Vm` の中に置く。GVL(`gvl.rs`)を持つスレッドだけが触るので `RefCell` で足りる。
  `current` は「GVL ホルダ」そのもので、acquire のたびに書き換える
  (`native::acquire`)。GVL の FIFO 待ち行列が実行可能キューの役をする。
- 待つスレッドは **GVL を手放して自分の wake パイプ(`Parker`)+ 待ち fd を
  `poll(2)` する**(`native::park`)。起こす操作(`wakeup` / joiner 起床 / 割り込み)は
  状態を `Runnable` にしてパイプへ 1 byte 書く `unpark`。

### 12.2 スレッドの一生(`native::spawn` → `thread_main`)

1. `Thread#initialize` → `scheduler::spawn` → `native::spawn`: GVL に `register`
   し、`Parker` を作り、`std::thread::Builder`(8 MiB スタック)でカーネルスレッドを
   起動。起動側は新スレッドが **GVL の待ち行列に並ぶまで** spin(`Gvl::is_waiting`)
   してから戻る。続く `Thread.pass` = `Gvl::yield_now` が新スレッドへロックを
   渡すので、生成直後に本体が最初の park まで走る。
2. カーネルスレッド側(`thread_main`): `vm::adopt` で起動側の `Vm` を自分の
   `CURRENT` にし、`poll_flag::adopt` でポールワードを共有し、非同期シグナルを
   `pthread_sigmask` でブロック(→ プロセス向けシグナルはカーネルが main に配る)。
   GVL を取り、`Executor::invoke_proc` で本体ブロックを **自分のカーネルスタック上で**
   実行(`init_stack_limit` で main と同じ 1 MiB 予算、`enter_root_svar_scope`)。
3. 終了: 終了フック(§3)→ `set_terminated` → `finalize`(結果 / 例外の記録、joiner の
   起床、registry からの除去、report_on_exception のキュー、main への転送)。
   その後は Thread オブジェクトに触らず GVL を `release` → `unregister` →
   `vm::unadopt`。

### 12.3 park / unpark

```
park(cur, state, fds, deadline):        // GVL を持って呼ぶ
  state / park_indefinite / resume_exec を記録; main なら main_exec を publish
  fds も deadline も無い park なら deadlock 検査(12.4)
  release(GVL)
  poll([wake_fd] + fds, timeout)
  acquire(GVL)                          // ここから書き手は居ない
  wake パイプを drain; woken = (state が Runnable に変えられていた)
  EINTR なら execute_gc(main の trap ハンドラ配送)
```

書き手(`wakeup` / `interrupt` / `finalize_common` / deadline)は必ず GVL を
持っているので、park 側が GVL を取り直した時点でパイプに新しい byte は来ない。
drain してから次の park に入るため、古い byte で空振りすることはない。
`sleep` は `woken || deadline` まで park を繰り返し(EINTR は継続)、`join` は
ループ(flush → 割り込み配送 → dead 判定 → park)、`wait_fds` は
1 回 park して呼び出し側の再チェックに任せる。`pass` は `Gvl::yield_now`: 状態
ロックの下で先頭の待ち手にロックを渡す**のと同時に**自分を待ち行列の末尾に並べる
(`release` → `acquire` の 2 段ではない)。2 段だと間に待ち行列が空になる瞬間があり、
渡された側がそこで `Thread.pass` すると誰もいないと見て走り続ける — `Thread.pass`
の ping-pong(と、待っている間に相手の compiled frame を evict する §12.9)は
「譲った側が次の順番にいる」ことを前提にしている。

fd 待ちには起床経路がもう 1 つ要る: 別スレッドがその fd を `close` したとき、
`poll(2)` 中のカーネルスレッドは(カーネルが元のファイルを見続けるので)起きない。`FileDescriptor::drop` →
`scheduler::fd_closing(fd)` が、その fd を `park_fds` に持つ `IoWaiting` スレッドを
unpark し、起きた側は stream が閉じているのを見て `IOError`(CRuby の
`rb_thread_fd_close` 相当)。

### 12.4 デッドロック検出と停止

スケジューラループは無いので **park の入口で** 判定する: 自分が deadline も fd も無い park に入ろうと
していて、他の生存スレッドがすべて同じ状態(`park_indefinite`)なら
`fatal "No live threads left. Deadlock?"`。main 自身なら即 `Err`、他スレッドなら
main の `pending` に `Raise(fatal)` を積んで unpark する。判定は park の入口だけ
なので、待っている間に他のスレッドが死んで残りが全員 park 中になっても起きない —
CRuby も同じ(`rb_check_deadlock` は `sleep_forever` の入口でしか呼ばれない)。
GVL 待ちや `without_gvl` 中のスレッドは `Runnable` なのでデッドロックではない。

`Thread.ignore_deadlock = true` は検出器を止める(`Scheduler::ignore_deadlock`、
`check_deadlock` の先頭で見る)。用途は「全員が永遠に待ち、trap ハンドラが誰かを
起こす」プログラムで、§12.8 によりそのハンドラが park 中の main で走り、
起こされた main は park から戻る(ハンドラは park 状態のまま走るので、
自分への `wakeup` / `Queue#push` も起床として数える)。単独の `Thread.stop` は
CRuby と同じく `ThreadError`
(stopping only thread)で、純 Ruby の Mutex / ConditionVariable の待ちは
`Thread.__stop`(検査なし)を使うので、`Queue#pop` 単独は従来どおり fatal。

`terminate_all`(プロセス終了時)は各スレッドに `Kill` を積んで unpark し、
全員 dead になるまで GVL を渡し続け(待ち手が居なければ 1 ms 手放す、上限 10,000 回)、
dead になったカーネルスレッドを `join` してから戻る — インタプリタ(`Vm`)より
長生きするカーネルスレッドを残さないため。

### 12.5 GC / fork / プロセス終了

- GC はホルダだけが起こす。他スレッドは全員 safepoint で park 中なので、
  `Scheduler::mark` が `threads` 経由で各 `Executor` を、`main_exec` 経由で
  main の root executor を辿れる(`main_exec` を deref するのは **main が current
  でないときだけ** — main が走っているときは main の executor はコレクタ自身の
  ルートで、`Executor::init` の途中で publish した後に移動した古いポインタかも
  しれない)。
  JIT コードとの関係は §12.9。
- `fork`: `fork::prepare` が GVL の状態ロックを fork をまたいで保持する
  (`Gvl::prepare_fork`)。fork の瞬間に別のカーネルスレッドが `acquire` / `release`
  の中でそのロックを握っていると、そのスレッドのいない子は最初の `state.lock()` で
  永久に止まるので、その瞬間を作らない。子はそのガード経由で「登録 1、待機者なし、
  fork したスレッドが保持」にリセットする(`GvlForkGuard::reset_child`)。子では
  fork したスレッドだけが残り **main になる**(`s.main = cur`)。他のスレッドは
  dead になり `Thread.list` からも消える。wake パイプの
  作り直し(fd テーブル共有のため)、ブロックしていたシグナルの解除、
  `native_thread_id` の更新(新しいプロセスなので tid が変わる)。main 以外の
  スレッドから fork しても同じで、そのスレッドが子の main になる。
  `fork { ... }` の子はブロックが終わったら(または例外で抜けたら)main スレッドの
  終了として扱い、下のプロセス終了シーケンスを通ってから exit する
  (`Globals::exit_forked_child`: at_exit → 未捕捉例外の報告 → finalizer →
  ストリーム flush。`exit` の status はそのまま、ハンドラ内の `exit` が優先、
  シグナル例外はそのシグナルで死ぬ)。
- プロセス終了の順序は CRuby と同じ: `at_exit` ハンドラ →
  `terminate_all`(残りスレッドを kill し ensure を走らせ、カーネルスレッドを
  join)→ `ObjectSpace` の finalizer(`Executor::run_at_exit_handlers` /
  `run_finalizers`)。
- ブロッキング IO は §7 の経路(fd 待ちを `wait_fds` で park)をそのまま使う。
  park が GVL を手放すので、fd 待ちの間は他のスレッドが走る。
  `has_other_live_threads` が偽なら従来どおり本当にブロックする。fd で待てない
  カーネル待ちは §12.7。
- `Thread#backtrace` は park 中の `resume_exec`(main は `main_exec`)を読む。

### 12.6 テスト

スレッドのテストは `builtins::{thread,socket,process,io,file,fiber,fiddle}` /
`gvl` / `vm` / `fork` のユニットテストと、`tests/` の実バイナリを起動する
テスト(`exit_order.rs` など)。CI(`bin/test`)は通常の nextest で全部走らせる。

### 12.7 GVL を手放す区間(`scheduler::without_gvl`)

fd で待てないカーネル待ち — `flock` / FIFO の `open` / `fcntl(F_SETLKW)` /
`waitpid` / `getaddrinfo` / `blocking: true` の FFI 呼び出し / 拡張の
`call_blocking` — は、**呼び出したカーネルスレッド自身が GVL を手放して**
実行する(`native::without_gvl`)。`native_pool::run_blocking` は
`native::run_blocking` に委ね、`run_op` は呼び出し側で 1 回だけ発行する。

- **区間の約束**: クロージャはインタプリタに触れない(`Value` / `Globals` 禁止、
  raw fd・整数・自前バッファのみ)。GVL 側の手続きは park と同じで、
  `resume_exec`(`Thread#backtrace` 用)、main なら `publish_main_exec`
  (他スレッドが起こす GC が main のルートを辿るため)、そして
  `ThreadInner::blocking_tid`(自分の `pthread_self`)を立ててから
  `release()` → `f()` → `acquire()` → 両方を戻す。区間中の `state` は
  `ThreadState::Blocking`: Ruby からは眠っている(`#status` "sleep" /
  `#stop?` true。CRuby の blocking region が `THREAD_STOPPED` になるのと同じで、
  spec は `Thread.pass while t.status != "sleep"` で待つ)が、park では
  ないので `wake_parked` は触らず(割り込みは下の ubf 経路)、
  `check_deadlock` にも数えない。
- **子プロセスのシグナルマスク**: 非 main のカーネルスレッドは
  `block_async_signals` でほぼ全シグナルをブロックしており、そのスレッドから
  fork した子はそのマスクを継承する(`SIGTERM` が永遠に保留 — ruby/spec の
  `Process.kill` fixture がまさにこれ)。`system` / バッククォート /
  `IO.popen` / `Process.spawn` / `exec` はどれも `spawn::child_exec` を通り、
  そこで `reset_child_sigmask` が子のマスクを空にしてから `execve` する。
  `Process.fork` の子は fork 直後に forker が main になる際に全解除する(§12.5)。
- **割り込み(ubf)**: `Thread#kill` / `#raise` の相手が park 中でなく
  `blocking_tid` を持っていれば、`native::interrupt_blocking` が
  `pthread_kill(tid, SIGVTALRM)` を送る。ハンドラは空(`ubf_handler`、
  `SA_RESTART` なし)で、目的はブロック中の syscall を `EINTR` で戻すこと
  だけ。CRuby の `ubf_pthread` と同じシグナルで、そのため `VTALRM` は
  trap 不可の予約シグナルになっている。非 main のカーネルスレッドは
  `block_async_signals` でほぼ全シグナルをブロックするが、このシグナルだけは
  通す。ハンドラの登録は最初の `native::spawn` で 1 回。
- **`EINTR` の扱い**: `run_op` は `flock` / `open` / `fcntl` の `EINTR` を
  呼び出し側に返す。呼び出し側は GVL を取り直してから poll point(`execute_gc`: main の
  trap ハンドラ)と `deliver_pending_now`(kill / raise の配送)を実行し、
  配送がなければ同じ syscall をもう一度発行する(いずれも冪等な待ち)。
  FFI / 拡張の呼び出しは再発行しない(`NativeOp::retries_eintr`)。
  戻ってきた `EINTR` は C 側の流儀に任せ、戻った後で配送する。
  `waitpid` は既存の `EINTR` ループ(poll point → 再試行)がそのまま使える。
- **fd 待ちは park**: §7 の `blocking_io_region`(fd 待ち)は park 経路の
  まま。読み取りバッファを GVL の内外で分けて
  `read(2)` 自体を GVL 外に出す設計は採っていない — park が GVL を手放す
  ので、fd 待ちの間に他スレッドが走る目的はすでに達している。

### 12.8 シグナルと割り込みの配送

- **シグナルは main で変換する**。非 main のカーネルスレッドは
  `block_async_signals` で非同期シグナルをブロックしているので、プロセス宛の
  シグナルはカーネルが main のカーネルスレッドに配る。ハンドラは SIGNAL レーンを
  立てるだけで、変換(trap ハンドラの呼び出し / `Interrupt` 等への変換)は
  main の poll で行う。`signal_delivery_ok` は「current が main か」だけを
  見る(スレッドはコンテキストを保存して切り替わらないので、保存中の
  コンテキストの上でハンドラを走らせる心配はない)。したがって main は **自分の park(`poll` の `EINTR`)や
  `without_gvl` の `EINTR` の直後の poll でもハンドラを走らせ**、その後 sleep の
  残りや待ちを続ける(CRuby と同じタイミング)。
- **ホルダは GVL を譲る**。main 以外がホルダのときに poll でシグナル保留を見たら、
  その poll を preempt として扱い `pass`(`Gvl::yield_now`)する。main は
  `EINTR` で起きて GVL の待ち行列に並んでいる(か、並ぼうとしている)ので、
  次のタイムスライス(10 ms)を待たずにハンドラが走る。待ち手がいなければ
  `yield_now` は何もしない。
- **kill / raise は全状態に届く**: 走行中(次の poll で `pass` → 配送)、GVL 待ち
  (待っていた `pass` / park / `without_gvl` の戻りで `deliver_pending_now`)、
  park 中(`wake_parked` → unpark)、`without_gvl` 中(`interrupt_blocking` →
  `SIGVTALRM` → `EINTR`、§12.7)。`handle_interrupt` のマスクと
  `pending_interrupt?` は §6 の割り込みキューをそのまま使う。

### 12.9 JIT コードと GVL

JIT コードの生成・パッチ・実行はすべて GVL の下で起きるので、複数のカーネル
スレッドが同じコードページを同時に触ることはない。それでも、すべてが 1 つの
OS スレッドで回っていれば起きない点が 3 つある。

- **命令ストリームの可視性**。ホルダがコードを書いて(コンパイル、BOP 再定義の
  entry 巻き戻し、salvage のバージョン語の書き換え)手放した後、次のホルダは
  別のコアでそれを実行する。書き手側のフラッシュ(aarch64 の `dc cvau` /
  `ic ivau`、x86 は不要)は monoasm が `set_executable` でやる。実行側の
  シリアライズは `Gvl::on_acquired` が **取得のたびに無条件で** 行う
  (`cpuid` / `isb`)。「コードが書かれたときだけ」の世代カウンタは、書き込み
  箇所を漏れなく列挙し続ける必要があり、しかも GVL のハンドオフ(futex の
  起床)の隣では 1 命令の差は測れないので、無条件にした。
- **macOS arm64 の `MAP_JIT`**。書き込み可否は `pthread_jit_write_protect_np`
  による **スレッドごと** の状態だが、monoasm はページ全体で 1 つの `writable`
  フラグを持つ。フラグが信用できるのは「ホルダでないスレッドは全員フラグと同じ
  状態にいる」ときで、ホルダは GVL の下でしか書かず、手放す前に必ず
  `set_executable`(フラグ false)まで済ませるので、ハンドオフ時点の状態は常に
  「実行可」。新しいカーネルスレッドもその状態で始める必要があり、
  `thread_main` が最初に `pthread_jit_write_protect_np(1)` を呼ぶ
  (`jit_pages_executable_for_this_thread`)。プラットフォームの既定が書き込み可
  だと、最初の JIT 命令でフォールトし、フラグは既に「書ける」と言っているので
  遅延フリップも助けない。darwin の CI ジョブは `bin/test` を通すので、この
  経路は nextest で実機検証される。
- **BOP 再定義と他スレッドのフレーム**。`def Integer#+` を実行したスレッド以外は
  全員コンパイル済みフレームを持ったまま止まっている。`check_bop_redefine` は
  自分の CFP 鎖に加えて `scheduler::suspended_executors` が返す各スレッドの鎖
  (park / pass / `without_gvl` が公開する `resume_exec` と、その
  `parent_fiber` 連鎖)も変換する。呼び出し側でない最内フレーム — タイムスライス
  やシグナルで **poll** から手放したフレーム — は poll 自身から抜ける:
  `execute_gc` が自 iseq の `bop_evictions` を跨いで比べ、動いていれば
  `POLL_DEOPT` を返し、コンパイル済み poll のコールドパスがそれを見て deopt
  する。詳細は
  `doc/chain_deopt.md` §10.1。

これらは `cargo test --lib` と、
`MONORUBY_PREEMPT_STRESS=1` を重ねた同じフィルタで検証する。gc-stress は
手動ワークフロー(`gc-stress.yml`)で回す。
