# Thread / Fiber / non-blocking IO / プリエンプション の実装

monoruby のスレッド機構(`thread` ブランチ系列 #941–#962)の現状をまとめる。
対象読者はランタイムの実装に手を入れる人。関連ソース:

| ファイル | 内容 |
|---|---|
| `monoruby/src/scheduler.rs` | グリーンスレッド・スケジューラ本体 |
| `monoruby/src/preempt.rs` | タイムスライス・プリエンプションのタイマ(§8) |
| `monoruby/src/native_pool.rs` | カーネルブロッキング syscall のネイティブ・オフロード(§9) |
| `monoruby/src/value/rvalue/thread.rs` | `ThreadInner`(スレッド制御ブロック)と状態機械 |
| `monoruby/src/value/rvalue/fiber.rs` | `FiberInner`(Fiber 制御ブロック) |
| `monoruby/src/builtins/thread.rs` | Thread クラスのネイティブ・ビルトイン |
| `monoruby/src/builtins/fiber.rs` | Fiber クラスのビルトイン + JIT インライン `Fiber.yield` |
| `monoruby/src/codegen/arch/{x86_64,aarch64}/invoker.rs` | コンテキストスイッチのスタブ(両アーキ) |
| `monoruby/builtins/startup.rb` | Mutex / Queue / SizedQueue / ConditionVariable(純 Ruby)、Thread の簿記系メソッド |
| `monoruby/src/builtins/io.rs` | `blocking_io_region` / `IO.select` のグリーンパス |

## 0. 全体像

- **M:1 グリーンスレッド**。Ruby の `Thread` は 1 本の OS スレッド上で多重化される。
  真の並列性はない(GVL 型でもない — そもそも VM を回す OS スレッドが 1 本)。
  カーネルブロッキング syscall のオフロード(§9)には別の短命 OS スレッドを使うが、
  それらは Ruby ヒープにも VM にも触れない。
- **切替の 2 系統**:
  1. **ブロッキング地点**。`sleep` / `Thread.stop` / `#join` / `Thread.pass` /
     ブロックする IO / 同期プリミティブの待機で、実行中のスレッドが自発的に
     スケジューラへ制御を返す(協調型)。
  2. **タイマ駆動プリエンプション**(§8、#962)。生存スレッドが 2 本以上あるとき、
     10 ms ごとに専用タイマ OS スレッドが GC poll フラグにプリエンプトビットを立て、
     走行中のスレッドが次のセーフポイント(callee-entry / ループバックエッジ)で
     強制的に `Thread.pass` 相当を行う。busy-loop するスレッドも CPU を譲る。
- **どちらの切替も VM のセーフポイントでしか起きない**。この不変条件が設計全体を貫く:
  - 切替は必ずビルトイン内のブロッキング地点か、GC が起きうるのと同じセーフポイントで
    起きる。よってサスペンド中のスレッドのフレームは常に GC-complete(§6・§3.4)。
    プリエンプションが安全なのはこのため — 「GC が起きうる場所でしか切り替わらない」
    ので register write-back を含めて GC と全く同じ扱いになる(§8)。
  - ビルトインは他スレッドに対して**アトミック**(ビルトイン内では自分のブロッキング/
    poll 地点以外で切り替わらない — GVL 下で CRuby が C 関数に与える保証と同じ)。
  - ただし**純 Ruby コードのアトミック性はプリエンプションで失われた**。2 つの
    非ブロッキング文の間に他スレッドが割り込みうる。かつて純 Ruby の Mutex / Queue /
    ConditionVariable が check-then-act 競合なしに書けていた根拠はこれだったので、
    プリエンプション導入に合わせて park permit とロックで作り直してある(§5)。

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
handle:        Option<Box<Executor>>   // main スレッドのみ None(Executor は埋め込み側所有)
proc / args:   本体ブロックと Thread.new の引数
stack:         専用 256 KiB スタック(初回起動時に遅延確保)
state:         Created | Runnable | Sleeping | Joining | IoWaiting | Dead
resume_exec:   park した実行コンテキスト(スレッド root、またはスレッド内で park した nested Fiber)
result / exception:  終了結果(#join / #value が参照)
joiners:       このスレッドを #join で待っているスレッド
pending:       未配送の非同期割り込み(Kill | Raise(err))
killed:        kill 配送済みフラグ(終了時の unwind をクリーンな死として扱う)
masks:         Thread.handle_interrupt のマスクスタック(§6)
park_blocking: 直近の park がブロッキング操作だったか(#status のポーリング用)
park_permit:   park permit。running な対象への #wakeup/#run が立て、次の park が即戻る(§5・§8)
last_status:   $? / Process.last_status をスレッドごとに保持(#972)
```

重要な設計判断: **スレッド root の `parent_fiber` は常に `None`**。
このため `Fiber.yield` をスレッド本体で呼ぶと、main fiber と同じ既存のエラー経路
(Rust 側 / JIT インライン側とも)が**無変更で**正しく FiberError を出す。
スレッドの切替は `parent_fiber` を使わず、専用スタブ(§3)で行う。

注: `priority` / `native_thread_id` / fiber-local / thread-variable / `ignore_deadlock` は
`ThreadInner` のフィールドではなく、すべて startup.rb の純 Ruby 側で
インスタンス変数(`@priority` / `@fiber_locals` / `@thread_variables` 等)として実装される(§4)。

## 3. スケジューラ

`src/scheduler.rs`。OS スレッドごとの `thread_local!` シングルトン(`SCHEDULER: RefCell<Scheduler>`)。

```
threads:          生存スレッドの registry(main 含む)— GC ルート
ready:            実行可能キュー(FIFO)
sleepers:         (Option<Instant>, Thread)  — sleep / タイムアウト付き join・IO 待ち
io_waiters:       (fd, poll events, Thread) — fd 待ち(§7)
current / main
main_exec:        scheduler_run 実行中のみ有効な main の Executor ポインタ(GC 用)
in_scheduler:     scheduler_run のイベントループ実行中か(main_exec の有効期間)
machinery:        スケジューラ自身の機構(dispatch ループ / fd ポーリング)が
                  main コンテキストで走行中か — プリエンプション抑止マーカー(§8)
pending_reports:  遅延した report_on_exception のテキスト(machinery 中に生成し後で flush)
flushing_reports: flush_pending_reports の再入ラッチ
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

park の直前には `park_permit` を確認し、立っていればクリアして即戻る(§5)。
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
  をマークする。詳細は `doc/gc.md` §8。
- **main のフレーム**: GC のトリガが green thread 側だと、main のフレームは current の
  チェーンから辿れない。`scheduler_run` 実行中は `main_exec` ポインタを公開し、
  `in_scheduler` フラグが立っている間だけそれを deref してマークする。
- 切替はセーフポイントのみなので、サスペンド中のどのスレッドのフレームも GC-complete。

## 4. Thread API の実装状況

ネイティブ(`builtins/thread.rs`、`init`):
`Thread.new/start/fork`(本体はキューされ、どこかのスレッドが最初にブロックした時点で
初実行される)、`Thread.current/main/list/pass/stop`、
`Thread.kill(th)` / `Thread.exit`、`Thread.handle_interrupt` / `Thread.pending_interrupt?`、
`#join(timeout)` / `#value`(終了例外を再 raise)、`#status`("run" / "sleep" / false / nil)、
`#alive?` / `#stop?`、`#wakeup` / `#run` / `#__wakeup_permit`(§5)、
`#raise` / `#kill` / `#exit` / `#terminate`、`#pending_interrupt?`。

`Thread.handle_interrupt` は**ネイティブで機能する**:マスク(`(例外クラス, タイミング)` の列)を
`ThreadInner::masks` へ push/pop し、マスク境界で保留割り込みを配送する(§6)。
※ startup.rb 冒頭に残る「#raise / #kill はまだ未実装」旨のコメントは古い名残り
(現在はネイティブ `raise`/`kill` と `pending`/`masks` の割り込み機構が動いている)。

Ruby 側(startup.rb、`class Thread`):
- `#name`
- fiber-local `[]` / `[]=` / `key?` / `keys` / `fetch`(`@fiber_locals` に格納)
- `#thread_variable_get` / `_set` / `#thread_variable?` / `#thread_variables`(`@thread_variables` に格納)
- `#priority` / `#priority=`(-3..3 にクランプして `@priority` に保存するのみ。実際の
  スケジューリングには影響しない)
- `#native_thread_id`(生存中は `object_id`、死後は `nil` を返す。実カーネル tid ではなく
  オブジェクトごとに一意なトークン)
- `#report_on_exception`(インスタンスのみ)
- `Thread.ignore_deadlock` / `=`(クラス変数に丸めるだけ。デッドロック検出器自体は止めない)
- `Thread::Waiter`(`Process.detach` 用。native `Thread.new` はブロック必須なので
  `allocate` ベースで生成)

`Kernel#sleep` は他に生存スレッドがいるときだけスケジューラ経由になる
(無引数 sleep は `#wakeup` まで park)。単独スレッド時は従来の nanosleep ループ。

## 5. 同期プリミティブ(純 Ruby)+ プリエンプション下での正しさ

Mutex / Queue / SizedQueue / ConditionVariable は **startup.rb の純 Ruby 実装**で
ネイティブコードはない。かつて(協調型のみだった頃)は §0 のアトミック性
「2 つの非ブロッキング文の間に他スレッドが割り込まない」を根拠に、
「条件検査 → waiter 配列へ self を追加 → park」の列を素朴に書けた。

**プリエンプション(§8)はこの前提を壊す**ので、次の 2 機構で正しさを保っている
(startup.rb のコメントブロック参照):

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
  unpark は上記 `#wakeup` / `#__wakeup_permit` を使う。
- `Mutex#sleep` / `ConditionVariable#wait` は unlock → park → **ensure で re-lock**。
  `#kill` / `#raise` が park 中に配送されても(§6 の unwind は ensure を実行するので)
  mutex は正しく再取得・解放される。終了スレッドが握ったまま放置したロックは
  次の取得者側で回収される(#966)。`Mutex#owned?` は **Fiber 単位**の所有で判定する(#967)。
- Queue / SizedQueue は Mutex + ConditionVariable の上に載る(CRuby thread_sync.c と同じ構造)。
  すべての check-and-take をキューの mutex 下で行うことで、かつての
  `@items.empty?` / `@items.shift` のセーフポイント窓(2 つの pop が競合し片方が幻の nil を
  得る)を相互排他で閉じる。
- 待機ループは `loop do` ではなく **`while true`** を使う: `Kernel#loop` は StopIteration を
  握り潰し、`ClosedQueueError < StopIteration` なので `loop` ブロック内の
  `raise ClosedQueueError` が黙って飲まれてしまう(実際に `SizedQueue#push` on closed が
  nil を返すバグだった)。
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
- **走行中の対象**: プリエンプション(§8)が対象を次のセーフポイントで `pass` に落とし、
  そこで pending が配送される。busy-loop 中のスレッドにも `#kill` / `#raise` が届く。
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
- **`Thread.handle_interrupt`**: マスク(`ThreadInner::masks`)にマッチする割り込みは
  即配送せず保留し、マスク境界(handle_interrupt ブロックの出入り)で配送する。

## 7. non-blocking IO(fd ポーラ統合)

前提として、シグナル対応の際に全ブロッキング IO は `blocking_io_region`
(EINTR → VM ポーリング → 再開)という単一のチョークポイントに集約されている
(SA_RESTART なしのシグナルハンドラ、EINTR を握り潰さない独自 read/write プリミティブ。
fd を持たない旧 `blocking_region` は全サイトが `blocking_io_region` に統合され削除)。
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
- **ソケット**: TCP の `connect` は非ブロッキング発行(`EINPROGRESS`)→ `POLLOUT` を
  `wait_fd` で待って `SO_ERROR` 確認、`accept` はリスナ fd を恒久 non-blocking にして
  `POLLIN` の park-retry ループで受ける(いずれも native worker ではなくスケジューラの
  fd ポーラで処理)。DNS(`getaddrinfo`)は CRuby 同様インラインでブロックする。
- **poll できないカーネルブロッキング**(flock、FIFO の open)は native worker に
  オフロードする(§9)。

### mid-operation の would-block エミュレーション

入口の readiness チェックだけでは、「利用可能なデータを消費し、さらに要求する」操作
(例: パイプ上の `read(n)` で n が到着済みバイト数を超える、行が複数チャンクに分かれて
届く `gets`、パイプ容量を超える `write`)の 2 チャンク目以降がプロセス全体をブロックする。
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

## 8. タイムスライス・プリエンプション(`preempt.rs`, #962)

協調型だけでは、busy-loop するスレッドが `Thread.pass` を呼ばない限り CPU を独占し、
他スレッドの飢餓・`Thread#kill` 未配送・mspec の `--timeout` watchdog スレッドが
永久に走らない、といった公平性の問題が残る。プリエンプションはこれらを、GC・JIT
バックエンド・スレッドローカルシングルトンに一切触れずに解消する。

プリエンプションは正確に **「全スレッドが次のセーフポイントで `Thread.pass` を
呼んだかのように」** 振る舞う — 既存の協調切替機構をそのまま再利用する。

### 8.1 タイマ

- **10 ms tick**(`TICK`)の専用タイマ OS スレッド。**生存スレッドが 2 本以上ある間だけ**
  走る(`on_thread_count(live)` が `live >= 2` で起動、`live < 2` で停止)。単独スレッドの
  プログラムはタイマを 1 本も生やさずコストゼロ。
- `MONORUBY_NO_PREEMPT` / `MONORUBY_PREEMPT_STRESS` が設定されているとタイマは起動しない。
- 毎 tick、`flag_addr` の mutex を取ってから poll フラグに `fetch_or(PREEMPT_BIT)`。
  `Codegen::drop`(`codegen_dropped`)が同じ mutex 下で `flag_addr` を 0 に落とすので、
  タイマが解放済み JIT メモリを触ることはない(マルチインタプリタのテストハーネス対策)。

### 8.2 フラグプロトコル

poll フラグは **GC の `alloc_flag` と同じ 1 つの `u32`**。複数の書き手がいる:

| 書き手 | 操作 |
|---|---|
| RValue アリーナ(ページ充填) | `+= 1` |
| シグナルスタブ | `+= 10` |
| malloc トリガ / `GC.start` | `>= 8` 帯へ持ち上げ |
| プリエンプトタイマ | `\|= 1 << 30`(`PREEMPT_BIT`) |

- **ビット 30**であってビット 31 ではない: x86-64 の poll は `cmpl [rip+alloc_flag], 8; jge`
  という**符号付き**比較なので、ビット 31 だと負値に読めて発火しない。
- タイマは別 OS スレッドから書くので、フラグアクセスはすべてアトミック
  (タイマ `fetch_or`、GC 後の `unset_alloc_flag` は `fetch_and(PREEMPT_BIT)` で
  ベース帯だけ落として並行設定されたプリエンプトビットを**保存**する)。

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
2. `let (flag_base, preempt) = preempt::consume_poll_flag();` — プリエンプトビットを剥がし、
   `(ベース値, プリエンプトか)` を返す(フラグ未登録なら防御的に `(8, false)`)。
3. 保留シグナルを drain(エラーを立てて `None` を返しうる)。
4. `flag_base >= 8` のときだけ実際に GC(純プリエンプト tick はベースが 8 未満なので
   スキップ = 偽の full GC を起こさない)。
5. `stress_renudge()` — stress モードでは切替の**前に**フラグを再武装し、切替先スレッドも
   poll するようにする。
6. `if preempt && scheduler::preempt_ok() { scheduler::pass(vm, globals)? }`。
   `pass` の `Err`(kill/raise がこのスレッドに配送された)は `set_error` + `None` で浮上。

`preempt_ok()` = `!machinery && main.is_some() && has_other_live_threads()`。
`machinery` マーカー(§3 の Scheduler フィールド)は、スケジューラ自身の機構
(dispatch ループ / fd ポーリング)が main コンテキストで走る間 true・dispatch された
スレッドの Ruby コードが走る間だけ false。よってプリエンプションは resume されたスレッドの
コード中でだけ発火する(`in_scheduler` はこの役に立たない — dispatch 中のスレッド走行中も
true のままだから)。

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

## 9. ネイティブ syscall オフロード(`native_pool.rs`, #962)

poll(2) で readiness を待てるもの(ソケット等)はスケジューラの fd ポーラで扱えるが、
**待つべき fd を持たないカーネルブロッキング syscall** はグリーンスレッドの単一 OS
スレッドをそのままブロックしてしまう。これらだけを別の短命 OS スレッドへ逃がす。

- **オフロード対象は 2 つだけ**(`NativeOp`):
  - `flock(2)` のブロッキング取得(`File#flock`)。`LOCK_NB` / `LOCK_UN` は
    カーネルでブロックしないのでインライン実行。
  - **FIFO** に対するブロッキング `open(2)`(相手が開くまでブロックする)。事前に
    `stat` して FIFO のときだけオフロードし、それ以外の open はインライン。
- **プールではない**: `submit` は操作ごとに `std::thread::spawn` で**専用の短命 OS
  スレッドを 1 本**生やし、syscall が返ったら終了する(ワーカー数・キュー・再利用なし)。
- **ワーカーは Ruby ヒープにも VM のスレッドローカルにも触れない**。`NativeOp` は生 fd /
  フラグ / `CString` パスだけを運ぶ(ヒープ参照を持たない)。共有状態はプロセスグローバル:
  `results`(`Mutex<HashMap<ticket, Completion{ret, errno}>>`)、`orphans`、`NEXT_ID`。
- **完了通知は eventfd ではなく pipe(2)**。各インタプリタ OS スレッドが `thread_local` に
  自己パイプ(read/write)を遅延生成し、ワーカーは結果を `results` に置いてから提出元の
  write 端へ 1 バイト書く。read 端はスケジューラの**通常の fd ポーラ**に登録される
  (スケジューラは `native_pool` を一切知らない)。
- **flow**(`run_blocking`): `submit(op)` → チケット取得 → `try_take` で完了を回収、
  未完なら `scheduler::wait_fd(read端, POLLIN)` で park(他 green thread に譲り、main が
  park しているなら `poll(2)` で寝る)→ 起床で pipe を `drain` → 再試行。パイプは複数
  waiter で共有なので、未充足の waiter は再 park する。
- **キャンセル**: park 中に kill/raise が来て `wait_fd` が `Err` を返したら、チケットを
  `discard`(結果が既に届いていれば除去、まだなら `orphans` に入れてワーカーの結果を
  到着時に捨てる)しエラーを伝播する。ワーカー本体はそのまま無害に完走させる
  (可搬なキャンセル syscall がないため)。

## 10. 既知の制限と今後

1. **シグナルは「ポーリングしたスレッド」で変換される**(CRuby は main に配送)。
2. `Thread.new` のサブクラスは Ruby の `initialize` オーバーライドを実行しない。
3. `Thread#priority` は保存のみ(スケジューリングに影響しない)。`native_thread_id` は
   実 tid ではなくオブジェクト単位トークン。`ThreadGroup` / `fork` との相互作用、
   `Thread.ignore_deadlock` の実効(検出器の停止)は未実装。
4. ネイティブオフロード(§9)は flock / FIFO open のみ。`fcntl(F_SETLKW)` 等、他の
   カーネルブロッキング操作は未対応(将来 `NativeOp` を増やす余地)。
5. 真の並列化は別の話(Ractor 型の分離が現アーキテクチャ —
   OS スレッドごとの ALLOC / CODEGEN / SCHEDULER — と整合的)。

(解決済み: `Thread.handle_interrupt` マスキング、mid-operation の IO ブロック(§7 の
would-block エミュレーション)、タイムスライス・プリエンプション(§8)、
カーネルブロッキング syscall のオフロード(§9)。)

## 11. テスト

- `builtins/thread.rs` の `#[cfg(test)]`: CRuby 4.0.2 との差分テスト
  (インターリーブ順序、status ポーリング、kill/raise 意味論、同期プリミティブ、
  thread+IO の各パターン、IO.select エッジ、fiber-local / thread-variable、
  `native_offload_flock_and_fifo`(flock / FIFO open がグリーンスレッドだけをブロックし
  プロセス全体を止めないことの検証)ほか)。
- ruby/spec: `bin/spec` またはリポジトリ外の spec/mspec で
  `core/thread` / `core/mutex` / `core/queue` / `core/sizedqueue` /
  `core/conditionvariable` / `core/io` を実行。
  かつてスペックランナーをハングさせた `core/io/copy_stream_spec.rb` と
  `core/io/select_spec.rb` は完走・全パスする。
