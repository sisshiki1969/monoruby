# シグナル処理

monoruby の POSIX シグナル処理の**現行実装**を、コードに即して解説する。
シグナルは GC・プリエンプションと同じ**セーフポイント機構**(`doc/safepoint.md`)の上に
載っており、非同期に到着したシグナルを「フラグを立てて次のセーフポイントで Ruby 例外 /
`Signal.trap` ハンドラに変換する」遅延配送モデルで扱う。

> 注: コードコメントが参照する節ラベル(A2 / A3 / A4 / A6 / A7 / B+)は本書の見出しに対応する。

主な実装ファイル:

| 対象 | ファイル |
| --- | --- |
| ペンディングビットマップ・signo↔例外・disposition | `codegen/signal_table.rs` |
| ハンドラスタブ(async-signal-safe) | `codegen/arch/{x86_64,aarch64}/jit_module.rs`(`signal_handler_for`) |
| sigaction インストール・スタブ事前生成 | `codegen/codegen.rs` |
| セーフポイント配送 | `executor.rs`(`execute_gc`) |
| `Signal` / `Kernel#trap` ビルトイン・名前表 | `builtins/process.rs` / `builtins/kernel.rs` |
| trap テーブル(GC ルート) | `globals/globals.rs` |
| `signal_interrupt` マーカー | `globals/error.rs` |
| ブロッキング IO 統合 | `builtins/io.rs`(`blocking_io_region`)/ `value/rvalue/io.rs` |
| ハングウォッチドッグ | `watchdog.rs` |
| 終了時のシグナル死 | `main.rs` / `executor.rs`(`terminate_with_signal`) |

---

## 1. 全体設計 — 遅延配送

シグナルハンドラは async-signal 文脈で走るため、そこでは Rust のアロケータ・`RefCell`・
libc 呼び出しに触れられない。そこで **2 段階**にする:

1. **記録(async-signal 文脈)**: Rust ハンドラ(`signal_table::signal_handler`)が、
   プロセスグローバルなビットマップに自分のビットを `fetch_or` し、poll ワード
   (`poll_flag.rs`)の SIGNAL レーンを `fetch_or` して即 return。lock-free な
   アトミック演算 2 つだけ(ロックなし・確保なし・TLS なし)。
2. **配送(セーフポイント)**: 次に VM/JIT がセーフポイント(callee-entry / ループ
   バックエッジ、`doc/safepoint.md` §4)へ到達すると `execute_gc`(`executor.rs`)が
   ビットマップを drain し、最小番号のシグナルを Ruby 例外 / `Signal.trap` ハンドラ呼び出しに
   変換する。

この構造により、シグナルは GC・プリエンプションと同一の poll ワード・同一の poll・
同一の `execute_gc` を共有する。ワードは 8bit×4 のレーン(GC / PREEMPT / SIGNAL / 予備)に
分割されており、poll はワード全体のゼロ判定 1 個(`poll_flag.rs` 参照)。

---

## 2. ペンディングビットマップ(`signal_table.rs`)

```rust
pub(crate) static PENDING_SIGNALS: AtomicU32 = AtomicU32::new(0);
```

- **プロセスグローバル**な `AtomicU32`。ビット `n` = シグナル `n+1`(SIGINT=2 ⇒ bit1)。
- プロセスグローバルにする理由: `sigaction` はプロセス全体に効くので、`Codegen` ごとに
  ビットマップを分けると、2 個目の `Codegen` がハンドラを自分のビットマップへ向け直した際に
  シグナルを取りこぼす。1 枚のグローバルにすれば、記録側(ハンドラ)と drain 側が
  どの `Codegen` がインストールしたかによらず一致する。
- `take_pending_signals()` — `swap(0, Relaxed)` でアトミックに drain。
- `lowest_pending_signo(bitmap)` — `bitmap.trailing_zeros() + 1`。**最小番号の signo が優先**
  (§6)。1 回の drain で配送するのは 1 シグナルだけ。

### Rust ハンドラ(A2;`signal_table::signal_handler`)

`sa_handler` 形式で `sigaction(2)` される単一の `extern "C" fn(signo: i32)`:

```rust
pub(crate) extern "C" fn signal_handler(signo: i32) {
    if (1..=32).contains(&signo) {
        PENDING_SIGNALS.fetch_or(1 << (signo - 1) as u32, Ordering::Relaxed);
    }
    crate::poll_flag::set_signal_from_handler(); // SIGNAL レーンを fetch_or
}
```

**async-signal-safe な理由**: 本体は lock-free な relaxed アトミック演算 2 つだけ。
ロックなし・確保なし・TLS なし・libc 呼び出しなし。poll ワードのアドレスは
`poll_flag.rs` のプロセスグローバルなレジストリ(`AtomicUsize`、最後に登録した
`Codegen` が勝つ — `sigaction` のプロセスワイド性と対応)から取得する。

かつては signo ごとに JIT asm スタブ(`addl [alloc_flag], 10; orl [pending], bit`)を
事前生成していたが、(1) lock なし RMW がプリエンプトタイマの `fetch_or` と競合して
どちらかの更新を失いうる、(2) インストール元 `Codegen` の解放後にシグナルが来ると
解放済み JIT メモリを実行する、という 2 つの欠陥があり、Rust ハンドラ+アトミック演算に
置き換えた。

---

## 3. sigaction のインストール(A3)

`codegen.rs` の `sigaction_to` / `install_signal_handler` がプロセスワイドに `libc::sigaction` する。

- **`SA_RESTART` を付けない**(`flags = 0`)。これは意図的(§8)。シグナルがブロッキング
  syscall を EINTR で中断させ、インタプリタを poll 地点へ到達させるため。
- ハンドラは全 signo 共通の Rust 関数(§2)なので事前生成物はなく、実行時の trap は
  `sigaction(2)` だけで済む(稼働中のバッファに JIT コード生成を行わない、という
  旧スタブ方式の利点はそのまま)。`install_signal_handler` は `is_trappable` で守られる。
- **デフォルトインストール**: 起動時に `POSIX_SIGNALS`(HUP, INT, QUIT, ALRM, TERM,
  USR1, USR2)へ自動で `sigaction`。CHLD/CONT/WINCH/TSTP や PIPE は既定では張らない。
  ウォッチドッグが armed のとき(§9)は SIGALRM をスキップしてハンドラを奪わない。

### シグナル集合

| 集合 | 内容 |
| --- | --- |
| `POSIX_SIGNALS` | デフォルトで `sigaction` する集合。既定で rescuable な `SignalException`(INT のみ `Interrupt`)へ変換される。 |
| `TRAPPABLE_SIGNALS` | `Signal.trap` でハンドラを張ってよい集合(Linux/非 Linux で cfg 分岐)。KILL/STOP(捕捉不能)、SEGV/BUS/FPE/ILL/TRAP/ABRT(フォールト)を除外。SIGPWR は Linux のみ。 |

---

## 4. セーフポイントでの配送(A6;`execute_gc`)

`execute_gc`(`executor.rs`)がセーフポイントで実行する処理のうち、シグナル部分:

1. `watchdog::poll()`(§9)。
2. `poll_flag::consume_preempt()` で PREEMPT レーンを消費する。
3. **シグナル drain**: SIGNAL レーンをクリアしてから `take_pending_signals()` →
   `lowest_pending_signo()`(クリア→drain の順序: 間に届いたシグナルはレーンとビットを
   立て直すので余分な poll 1 回で済む。逆順はビットの arming を失いうる)。signo があれば
   `globals.signal_disposition(signo)` で分岐:
   - `Handler(handler)` → `arg = Value::integer(signo)`、`#call` を `invoke_method_inner` で
     呼ぶ。`Err` なら `set_error` + `return None`。
   - `Ignore{..}` → no-op(防御的。SIG_IGN のシグナルは通常ビットを立てない)。
   - `Default | SystemDefault` → `signo_to_error(signo)`。`Some(err)` なら
     `set_error(err); return None`。マップ外は何もしない。
   - 同じ drain 窓に複数立っていても**最小 signo だけ**配送し、残りは捨てる
     (CRuby も coalesce したシグナルの全配送を保証しない)。
4. GC 本体(GC レーンが立っているとき)。
5. プリエンプション(`scheduler::pass`)。

**シグナル配送は GC・プリエンプションより前**に同じ poll 内で行われる。
`execute_gc` は**ハンドラ呼び出し中に CODEGEN 借用を保持しない**ので、trap ハンドラが
その内部で JIT コンパイルや GC を起こしても自由に再入できる。

### 4.1 配送ゲート(`scheduler::signal_delivery_ok`)

すべての poll 地点で配送してよいわけではない。3 条件を満たす poll だけが drain する:

1. **メインのグリーンスレッド上**であること(`current == main`)。CRuby と同じく、
   非メインスレッド実行中に届いたシグナルはメインの次の poll まで保留される。
2. スケジューラ自身の**機構(`machinery`)が走っていない**こと。
3. スケジューラの**入口(`pass` / `sleep` / `join` / `terminate_all`)の中でない**こと。
   これらの内側ではコンテキストが既にスイッチ用に保存されている(あるいは直後に
   保存される)場合があり、その上に積んだ Ruby フレームは保存コンテキストの復帰時に
   **もう一度**実行されてしまう。

3 番目は `SCHED_CALL_DEPTH`(RAII ガード `SchedCall`)で数える。**この深度は
コンテキストスイッチと一緒に持ち回らなければならない** — カウンタは OS スレッドの
thread-local だが、ガードは各グリーンスレッドのスタック上にあり、park した
スレッドは Rust フレームごと凍結されるのでガードが解放されないまま残る。
`dispatch` は `machinery` と同じ区間で深度を退避・復元し、park 中の深度は
`ThreadInner::sched_call_depth` に置く。

> これを怠ると、`sleep` で park したスレッドが 1 本あるだけでカウンタが恒久的に
> 1 以上に張り付き、以後 `Signal.trap` ハンドラが**二度と走らなくなる**。
> `nil until flag` は無限に回り、次のブロッキング書き込みが内部マーカー
> `__monoruby_signal_interrupt__`(§ マーカー)を RuntimeError として表に出す。
> 回帰テスト: `process.rs` の `signal_delivered_while_another_thread_is_parked`。

---

## 5. signo → 既定例外(A4;`signo_to_error`)

`Signal.trap` ハンドラが無い場合、`Default`/`SystemDefault` は既定例外に落ちる:

| signo | 例外 |
| --- | --- |
| SIGINT | `Interrupt`(専用クラス。`Interrupt < SignalException`) |
| SIGTERM / SIGHUP / SIGQUIT / SIGALRM / SIGPIPE / SIGUSR1 / SIGUSR2 | `SignalException("SIG…")` |
| その他 | `None`(防御的フォールスルー) |

`Interrupt < SignalException`(A4)なので、`rescue SignalException` は SIGINT も捕まえる。
これらはいずれも rescuable な例外として VM の unwind 経路に乗る。

---

## 6. `Signal.trap` / `Kernel#trap`(A7)

### 登録

- モジュール `Signal` に `list` / `signame` / `trap`(`builtins/process.rs`)。
- `Kernel#trap`(`builtins/kernel.rs`)は同じ `process::signal_trap` に委譲。

### `Signal.trap` / `Kernel#trap` の挙動

1. `trap_signo` で signo を解決(Integer は妥当な signo、Symbol/String/`#to_str` は名前で。
   `"SIG"` 有無どちらも可。`#to_int` は呼ばない。それ以外は `ArgumentError: bad signal type`)。
2. KILL/STOP(捕捉不能)→ `ArgumentError: "Signal already used by VM or OS"`。
   予約シグナル(SEGV/BUS/ILL/FPE/VTALRM, EXIT)→ `ArgumentError: "can't trap reserved signal"`。
3. disposition はコマンド引数(`command_disposition`)かブロック
   (`Handler(generate_proc(...))`)から決まる(コマンド優先)。
4. **先に OS レベルでインストールし、その後 trap テーブルへ記録**する。`CODEGEN` 借用下で
   disposition ごとに `install_signal_stub` / `install_signal_ignore` /
   `install_signal_default` / `install_signal_system_default` を呼ぶ。失敗時は `Errno`。
   `set_signal_disposition` が**前の disposition** を返し、`disposition_to_value` で Ruby 値に
   変換して返す。

### コマンド文字列と disposition

| コマンド引数 | disposition | Ruby へ返る表現 |
| --- | --- | --- |
| `nil` | `Ignore{from_nil:true}` | `nil` |
| `""` / `"SIG_IGN"` / `"IGNORE"` | `Ignore{from_nil:false}` | `"IGNORE"` |
| `"SIG_DFL"` / `"DEFAULT"` | `Default` | `"DEFAULT"` |
| `"SYSTEM_DEFAULT"` | `SystemDefault` | `"SYSTEM_DEFAULT"` |
| その他の String/Symbol | `ArgumentError: unsupported command` | — |
| String/Symbol 以外のオブジェクト | `Handler(cmd)` | そのオブジェクト |

`SignalDisposition`(`signal_table.rs`)は `Default` / `SystemDefault`(OS の `SIG_DFL`)/
`Ignore{from_nil}` / `Handler(Value)` の 4 種。

### trap テーブル(`globals.rs`)

- `signal_handlers: Vec<SignalDisposition>`(signo で添字、0 は未使用)。初期値は全 `SystemDefault`、
  `POSIX_SIGNALS` のみ `Default`。
- `signal_disposition(signo)` / `set_signal_disposition(signo, disp)`(後者は前値を返す)。
- **GC ルート**: `Globals::mark` が各 `Handler(v)` をマークする(trap ハンドラの Proc は
  この表からしか到達できないが、将来任意の poll 地点で呼ばれうるため)。

### 名前 ↔ 番号(`SIGNAL_TABLE`)

`process.rs` の `SIGNAL_TABLE` が唯一の真実:`Signal.list`(名前→番号 Hash)、
`Signal.signame`(番号→名前)、`trap`、`Process.kill` が共有する。正準名がエイリアスに先行
(IOT=ABRT, CLD=CHLD, POLL=IO)。cfg 分岐(POLL/PWR は Linux、EMT/INFO はそれ以外)。

---

## 7. EINTR / ブロッキング IO 統合

### `SA_RESTART` を付けない理由(§3 再掲)

シグナルはブロッキング syscall を EINTR で中断させ、インタプリタを poll 地点へ運ぶ必要がある。
`SA_RESTART` を付けると、0 バイト転送で中断した `read(2)` が透過的に再開され、アイドルな
パイプでブロックしたプロセスを SIGTERM で終了させられなくなる。ブロッキングプリミティブは
**シグナルの伴わない素の EINTR は自前で再試行**する。
(ウォッチドッグ自身の SIGALRM は変換経路ではないので `SA_RESTART` を使う。)

### `signal_interrupt` マーカー(`globals/error.rs`)

- `MonorubyErr::signal_interrupt()` — 「ブロッキング IO プリミティブが、シグナル保留中に
  EINTR を見た」ことを表す内部マーカー例外(メッセージ `__monoruby_signal_interrupt__`)。
- `is_signal_interrupt()` で呼び出し側が判定。非ブロッキングの would-block 用に
  `is_would_block_interrupt` という同型マーカーもある(`doc/threads.md` §7)。
- プリミティブ(`value/rvalue/io.rs` の割り込み可能 read/write)は、EINTR がシグナル保留と
  重なったとき素の再試行をやめて `signal_interrupt()` を返す。シグナルの無い素の EINTR は
  再試行。入口で既にシグナル保留(EINTR なしでビットが立っている)なら、それも浮上させる。

### `blocking_io_region`(`builtins/io.rs`)

ブロッキング IO を包む単一のチョークポイント:

1. 入口で保留シグナルを先に drain(`PENDING_SIGNALS != 0` なら `execute_gc`)。
2. `f()` を実行。
3. `is_signal_interrupt()` なら poll 地点(`execute_gc`)を通す。既定 disposition は変換した
   SignalException を read の外へ raise、trap ハンドラなら実行して正常復帰時に操作を再開
   (消費済みバイトは pushback 済みなので失われない)。
4. `is_would_block_interrupt()` ならスケジューラの fd ポーラで park(`doc/threads.md` §7)。

### その他の EINTR→poll 経路(いずれも `SA_RESTART` なし)

`Process.waitpid`、`Kernel#sleep`(nanosleep 前に保留シグナルを drain)、`IO#write`/flush、
スケジューラの待機(`park_until_deadline` / fd 待ち)、`native_pool`(素の EINTR は再試行)。
**自分宛の `Process.kill`** は、単一スレッドではシグナルが `kill(2)` 復帰前に配送されビットが
既に立っているので、`kill` 呼び出し内で `execute_gc` を**インライン**で回して配送する(CRuby 同様)。

---

## 8. ハングウォッチドッグ(B+;`watchdog.rs`)

単一スレッド・非プリエンプティブなプログラムは、前進のないままスピン/ブロックしうる。
`MONORUBY_HANG_WATCHDOG_SEC=N`(N>0)で N 秒間 poll 地点に到達しなければ強制終了する
ウォッチドッグを arm する(既定では無効)。

- 状態は `BUDGET` / `COUNTDOWN`(`AtomicI32`)。`arm_from_env()` が **SIGALRM**(こちらは
  `SA_RESTART` 付き)ハンドラと **1 Hz の `setitimer(ITIMER_REAL)`** を仕込む。
- armed 中はウォッチドッグが SIGALRM を**所有**する(`Codegen::new` のデフォルトインストールは
  SIGALRM をスキップ)。
- `poll()` は `execute_gc` から呼ばれ、`COUNTDOWN` を `BUDGET` に戻す(=「前進した」)。
  無効時は relaxed ロード 1 回だけ。
- `handler(signo)` は async-signal-safe(atomics + `write(2)` + `_exit(2)` のみ)。毎秒
  `COUNTDOWN` を 1 減らし、0 で fd 2 に中断メッセージを書いて `_exit(134)`。
  **中断判断は poll 地点ではなくハンドラに置く** — 本当にハングしていれば poll 地点に
  そもそも到達しないため。

---

## 9. スレッド / スケジューラとの関係

- **どのスレッドが変換するか**: main グリーンスレッド(§4.1 の配送ゲート、CRuby と同じ)。
  非 main スレッドの poll ではビットマップと SIGNAL レーンが残り、main の次の poll が配送する。
- poll ワードは GC・プリエンプションと共有の `u32` で、8bit×4 のレーンに分割
  (`poll_flag.rs`)。書き手はアリーナのページ圧力 / malloc / `GC.start`(GC レーン)、
  **シグナルハンドラ(SIGNAL レーン)**、プリエンプトタイマ(PREEMPT レーン)。
  全書き込みは冪等なアトミック `fetch_or`/`fetch_and`(`doc/threads.md` §8.2 /
  `doc/gc.md` §4.1)。
- グリーンスレッドをまたぐブロッキング IO はスケジューラの fd ポーラで park する。
  シグナルによる EINTR が待機を起こし poll 地点を通すので、他スレッドが park していても
  シグナル応答性が保たれる。

---

## 10. 終了時の SignalException(シグナル死)

捕捉されなかった `SignalException` / `Interrupt` は、プロセスを `exit(1)` ではなく
**同じシグナルで自死**させる(`main.rs::handle_error`):

- `Interrupt` はまずエラーレポートを出力、素の `SignalException` は静かに死ぬ。
- `terminate_with_signal(signo)`(`executor.rs`)が `SIG_DFL` に戻し、`sigprocmask` で
  ブロック解除して `kill(getpid(), signo)`。これで親プロセスからはシグナル死に見え
  (`$?.signaled?` / `termsig`)、`Process.kill("TERM", child); $?.signaled?` が CRuby 同様に動く。

---

## 11. まとめ

- シグナルは async-signal 文脈で**ビットマップと SIGNAL レーンを立てるだけ**、実配送は
  次のセーフポイント(`doc/safepoint.md`)で `execute_gc` が行う遅延モデル。GC・
  プリエンプションと poll・ワード・入口関数を完全に共有する。
- ビットマップ・trap テーブルはプロセス/インタプリタ単位で、trap ハンドラは GC ルート。
- `SA_RESTART` を意図的に外し、ブロッキング IO は `signal_interrupt` マーカー経由で
  EINTR を poll 地点へ運ぶ(SIGTERM 応答性の担保)。
- 最小 signo 優先(A6)、SIGINT→`Interrupt`(A4)、`Signal.trap`(A7)、共通 Rust ハンドラ(A2)、
  デフォルトインストール(A3)、ハングウォッチドッグ(B+)。
- 捕捉されない SignalException は同じシグナルで自死し、親に正しいシグナル死を見せる。
