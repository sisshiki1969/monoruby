# Exception handling in monoruby — mechanism and CRuby contrast

How monoruby raises, unwinds, catches, and reports exceptions, and how the
design differs from CRuby. The through-line is **laziness**: monoruby stores
the minimum at raise time and defers the expensive work (exception-object
materialization, backtrace string formatting) until something actually asks
for it. This keeps the raise path — including the control-flow "pseudo
exceptions" (`return` from a block, `break`, `throw`, `retry`, and internal
`StopIteration`) that reuse the same machinery — cheap.

Primary sources:

- `../monoruby/src/globals/error.rs` — `MonorubyErr`, `MonorubyErrKind`,
  backtrace formatting.
- `../monoruby/src/executor.rs` — `set_error`/`take_error`, `$!` handling,
  `take_ex_obj` (materialization), `complete_backtrace_for_rescue`, ensure
  deferral.
- `../monoruby/src/codegen/jit_module.rs` — `handle_error` (the unwinder).
- `../monoruby/src/globals/store/iseq.rs` — the per-method exception table
  (`get_exception_dest`, `covering_ensure`, `errinfo_restore_slots`,
  `nonlocal_exit_needs_vm_unwind`).
- `../monoruby/src/builtins/exception.rs`, `../monoruby/builtins/startup.rb`
  — the Ruby-visible `Exception` API (`#backtrace`, `#backtrace_locations`,
  `#set_backtrace`, `#cause`, …).

---

## 1. The big picture

```
   raise / error in a builtin or VM op
        │  vm.set_error(MonorubyErr)                (executor.rs:1074)
        ▼
   error sentinel returned  ──►  entry_raise  ──►  handle_error(vm, globals, meta, pc)
                                                        │   (jit_module.rs:85)
        ┌───────────────────────────────────────────────┤
        │  For the *current* frame:                      │
        │   1. dispatch control-flow kinds early         │
        │      (MethodReturn / Throw / BlockBreak /      │
        │       Retry / Redo) — may resume or redirect   │
        │   2. push this frame's (loc, sourceinfo, fid)  │  ← incremental
        │      onto err.trace                            │    trace capture
        │   3. consult the frame's exception table:      │
        │      • rescue dest?  → complete backtrace,     │
        │        materialize object, goto rescue         │
        │      • ensure dest?  → defer unwind, goto      │
        │        ensure                                  │
        │      • neither?      → return error to caller  │
        └───────────────────────────────────────────────┘
                                   │ unwind one frame, re-enter handle_error
                                   ▼
                        … up to the top level (main.rs) if never caught
```

`handle_error` runs **once per frame** as the exception unwinds. There is no
separate "raise" bytecode that snapshots the whole stack; the stack is
recorded incrementally, one frame at a time, as control leaves each frame.

### CRuby contrast

CRuby captures the backtrace **eagerly at raise time**
(`rb_ec_setup_exception` → `rb_vm_get_backtrace` walks the whole control-frame
stack and stores it on the exception object) before unwinding starts. That is
simple and makes `#backtrace` a stored-field read, but it pays the full
stack-walk cost on *every* raise — including the many raises that are caught
immediately and whose backtrace is never inspected. monoruby instead pays only
for the frames it actually unwinds through, defers the caller frames to the
catch point, and defers string formatting to `#backtrace`.

---

## 2. `MonorubyErr` — the in-flight error

`MonorubyErr` (`error.rs:9`) is the value held in `Executor.exception` while an
error is propagating. It is a Rust struct, **not** a Ruby object:

| field            | purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| `kind`           | `MonorubyErrKind` — the error class / control-flow tag (see §3)         |
| `message`        | the message string                                                      |
| `trace`          | `Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)>` — the backtrace, built incrementally as cheap tuples (no strings) |
| `original`       | when re-raising an existing exception object (`raise exc`), that `Value`, so identity + ivars survive |
| `explicit_cause` | an explicit `cause:` keyword (`Some(nil)` for `cause: nil`)             |
| `payload`        | kind-specific extra data surfaced as hidden ivars on materialization (e.g. `LocalJumpError#exit_value`, `StopIteration#result`) |

The exception object (`RVALUE` of class `RuntimeError`, etc.) is **not**
created here. It is materialized lazily by `take_ex_obj` (§8) only when a
`rescue` actually binds it or the top level needs to print it. Deferring
`Value` allocation is the first half of the laziness story.

`MonorubyErr::mark` (`error.rs:91`) participates in GC: while an error is in
flight it is not a Ruby object, so the GC cannot reach the `Value`s it smuggles
(`original`, `explicit_cause`, `payload`, and the receiver/tag/value payloads
of a few kinds) through the normal object graph — `mark` roots them explicitly.

---

## 3. Two families of `MonorubyErrKind`

`MonorubyErrKind` (`error.rs:1111` and above) mixes **two conceptually
different things** into one enum, because monoruby routes both through the same
unwinder:

### 3a. Real exceptions (catchable by `rescue`)

`Runtime`, `NotMethod`, `Name`, `Type`, `Index`, `Key`, `Frozen`, `Load`,
`Range`, `DivideByZero`, `StopIteration`, `SystemExit`, `IO`, `Arguments`,
`Syntax`, `Other(ClassId)` (any user-defined subclass), … Each maps to a Ruby
exception class via `from_class_id` (`error.rs:1143`) / a class id, and each
may carry structured data (e.g. `NotMethod { name, receiver }`) that becomes
hidden ivars on the materialized object.

### 3b. Control-flow pseudo-exceptions (NOT ordinary `rescue` targets)

These reuse the unwinding machinery to implement non-local control flow, the
same way CRuby uses its `throw`/catch-table `TAG_*` mechanism:

| kind                       | Ruby construct                              | how it stops unwinding                                             |
| -------------------------- | ------------------------------------------- | ----------------------------------------------------------------- |
| `MethodReturn(val, lfp)`   | `return` from a block/proc/lambda           | stops at the target frame `lfp`                                   |
| `BlockBreak(val, fid, lfp)`| `break` out of a block                      | resumes the block's defining call, or degrades to `LocalJumpError`|
| `Throw(tag, val)`          | `Kernel#throw` / `Kernel#catch`             | intercepted only by a matching `catch`, never by `rescue`         |
| `Retry`                    | `retry` in a rescue clause                  | redirected to the begin-region start                              |
| `Redo`                     | `redo` in a loop                            | redirected to the loop body start                                 |
| `Fatal`                    | a Rust `panic!` caught at an `extern "C"` boundary | uncatchable — propagates straight to the top level         |

The crucial property, exploited for performance, is that **`handle_error`
dispatches every control-flow kind *before* it touches `err.trace`**
(`jit_module.rs:118`–`238`, all ahead of the `push_error_location` at
`jit_module.rs:242`). A `MethodReturn` / `Throw` / `BlockBreak` therefore
never accumulates a backtrace tuple and never materializes an exception object.
`return` from a block and `break` are as cheap as they can be while still
threading through `ensure` bodies correctly.

### CRuby contrast

CRuby likewise implements `return`/`break`/`next`/`redo`/`retry`/`throw` with
its internal tag mechanism rather than real exceptions, and likewise does not
build a Ruby backtrace for them. `Fatal` corresponds to CRuby's `rb_fatal` /
uncatchable `fatal` class. The taxonomy is deliberately parallel; monoruby just
folds it into one Rust enum.

---

## 4. The unwinder: `handle_error`

`handle_error` (`jit_module.rs:85`) is the heart of the mechanism. For the
current frame's `FuncKind`:

**ISeq (Ruby) frames:**

1. **Retry/Redo** (`jit_module.rs:106`) — take the error and `goto` the
   begin-region / loop start encoded in the instruction. No trace, no object.
2. **MethodReturn** (`jit_module.rs:118`) — if this frame is the target `lfp`,
   return the value here; if an `ensure` sits in the way, defer the unwind
   across it; otherwise keep propagating. `$!` is restored from the
   region-entry save on the way out (`restore_errinfo_on_exit`).
3. **Throw** (`jit_module.rs:157`) — run any intervening `ensure`, else keep
   propagating (a matching `Kernel#catch` frame consumes it).

   These three non-local-exit arms ask `covering_ensure(pc)`, not
   `get_exception_dest(pc)`: the `ensure` to run is the innermost covering
   region that *has* one, which is not always the innermost covering region.
   Nest a rescue-only `begin` inside an `ensure` region and the tightest
   entry carries no `ensure`, so asking `get_exception_dest` concluded there
   was none and skipped the body outright (#1185). Running the innermost
   ensure-bearing region chains the rest by itself — its `EnsureEnd`
   re-delivers the exit from a pc outside that region, where the next one out
   is now innermost.
4. **BlockBreak** (`jit_module.rs:173`) — at the block's defining frame, if the
   in-progress call site is the one that received this block, resume it with
   the break value (CRuby's `BREAK` catch table); otherwise degrade to
   `LocalJumpError` ("break from proc-closure").
5. **Incremental trace capture** (`jit_module.rs:242`) —
   `push_error_location(loc, sourceinfo, fid)` appends this frame's cheap
   tuple. Only *real* exceptions reach here.
6. **Fatal** (`jit_module.rs:247`) — never caught; skip `rescue`/`ensure`,
   propagate to the top.
7. **Exception table lookup** (`jit_module.rs:251`) — `get_exception_dest(pc)`
   returns `(rescue_pc, ensure_pc, err_slot)` for the innermost region
   covering `pc`:
   - **rescue** → call `complete_backtrace_for_rescue` (§7), materialize the
     object with `take_ex_obj`, store it into `$!` and the handler's error
     slot, and `goto` the rescue clause.
   - **ensure** → `defer_unwind` (§6) and `goto` the ensure body.
   - **neither** → `return ErrorReturn::return_err()`, unwinding one frame; the
     caller re-enters `handle_error`.

**Builtin (native) frames** (`jit_module.rs:265`): only the control-flow kinds
that can pass through a builtin are handled (`MethodReturn`, `Throw`,
`BlockBreak`); a real exception records an *internal* trace frame
(`push_internal_error_location`, no source location — printed as `<internal>`)
and unwinds. Builtins have no Ruby-level `rescue`.

The exception table itself is built by bytecodegen and stored per method
(`iseq.rs:504`). Entries nest innermost-first, so `get_exception_dest` returns
the tightest enclosing region — right for a raise, which the tightest `rescue`
catches, but not for a non-local exit, which wants the tightest `ensure` (see
arm 2 above).

### CRuby contrast

CRuby's unwinder (`vm_exec_handle_exception` / the `catch_table` on each ISEQ)
is structurally the same idea: a per-ISEQ table of `(type, start, end,
cont, sp)` entries scanned as the stack unwinds, with `CATCH_TYPE_RESCUE`,
`ENSURE`, `RETRY`, `BREAK`, `REDO`, `NEXT`. monoruby's `ExceptionMapEntry`
plays the role of a `catch_table` entry; `ErrorReturn::{goto, return_err,
return_normal}` plays the role of CRuby's `THROW_DATA` / continuation.

---

## 5. `$!` (errinfo) and the deferred-unwind stack

`Executor.errinfo` holds Ruby's `$!` — the exception currently being handled —
and is set when a `rescue` catches (`set_errinfo`, `executor.rs:1092`). Because
control can leave a frame while it is *suspended inside a rescue clause* (a
`return`/`break` jumping out mid-handler), the region-entry value of `$!` is
saved into a bytecode slot, and `restore_errinfo_on_exit`
(`jit_module.rs:72`) replays those saves (outermost wins) when such a frame is
exited. `errinfo_restore_slots` (`iseq.rs:543`) enumerates the relevant slots.

Only the generic unwind replays them, so a frame owing one cannot take the
JIT's specialized teardown — `ISeqInfo::nonlocal_exit_needs_vm_unwind` pairs
that condition with `covering_ensure` as the two reasons a non-local exit has
to go through `handle_error` at all. A protected region that is neither (a
plain `begin`..`rescue` the exit merely passes through) contributes nothing: a
`rescue` does not intercept a non-local exit.

---

## 6. `ensure` and deferred unwind

`ensure` complicates unwinding because the ensure body must run *with an empty
error slot* (so it can itself `raise`/`return`), yet the original in-flight
error must be re-raised afterwards unless the body overrides it. monoruby models
this with a **deferred-unwind stack** (`executor.rs:1102`–`1153`):

- `defer_unwind(lfp)` moves the in-flight error out of `exception` and stashes
  it keyed by frame, then `goto`es the ensure body.
- `finish_ensure(lfp)` (the `EnsureEnd` hook) re-raises the deferred error —
  **unless** the ensure body left a new error pending, in which case the new
  one wins (CRuby: a `raise`/`return`/`throw` inside `ensure` supersedes).
- `discard_deferred_unwind(lfp)` drops a deferral when the frame leaves by some
  other path so its `EnsureEnd` will not consume it.

This mirrors CRuby's `CATCH_TYPE_ENSURE` continuation plus the "ensure result
overrides pending throw" rule.

Note that **bytecodegen compiles the body once per edge**, which is what makes
the compiled `EnsureEnd` cheap (below). A `begin`..`ensure` without `rescue`
still gets a `rescue_pc`: it names a *second copy* of the body that ends in
`raise` rather than `EnsureEnd`.

```text
def m(a); begin; a * 2; ensure; $n = 1; end; end

[(:00002..:00004, rescue=:00005, ensure=:00008, err_slot=%4)]
  BB1  :00005 %5 = 1 / :00006 $n = %5 / :00007 raise %4    <- the exception edge
  BB2  :00008 %4 = 1 / :00009 $n = %4 / :00010 ensure_end  <- the normal edge
```

So an exception takes BB1 and never reaches BB2's `EnsureEnd`; a non-local exit
takes `handle_error`'s `goto(ensure)` into BB2 **in the VM**; and compiled code
falls into BB2 only on normal completion.

### 6.1 The compiled `EnsureEnd`'s gate

`EnsureEnd` asks one question — "is a deferred unwind parked for this frame?" —
and for JIT-compiled code the answer is always no. `defer_unwind` has five call
sites: three in `handle_error`, each immediately followed by
`ErrorReturn::goto(ensure)`, which resumes the VM, and two in the splice helpers
(§6.3), which pair with `ensure_end_spliced` rather than with this call. The
exception edge cannot arrive either, per the two-copy layout above.

It was nevertheless a runtime call on the normal path of every compiled
`ensure` region — measured at about 40 cycles per execution: a trivial
`begin`..`ensure` made a hot method 63% slower than the same method without
one. It now sits behind the same one-word mirror `emit_ret` tests
(`Executor::deferred_top_lfp`, #1186): a compare and a not-taken branch.

The gate is conservative rather than an elision — when the mirror *does* name
this frame it runs exactly the old sequence — which matters because compiled
code demonstrably can run with a deferral parked: a loop inside an `ensure`
body entered by an unwind can be re-entered by OSR, which is why `emit_ret`
carries the same gate. Worth about 10% on a hot method with a trivial
`ensure`, and 14% on §6.3's find-first shape.

### 6.2 The region-entry `$!` save

A protected region saves `$!` at its entry so a non-local exit leaving a
`rescue` clause can put it back (§5). That save read `$!` through the generic
hooked-global runtime call, once per invocation of *every* method carrying a
`begin`..`ensure` or `begin`..`rescue`:

```text
:00001 %2 = $(errinfo)
      mov  rdi,rbx / mov rsi,r12 / mov edx,0x6a      before
      movabs rax,<runtime::get_global_var> / call rax

      mov  rax,QWORD PTR [rbx+0x198]                 after
```

`$!` is a plain `Value` field of the `Executor` and `$(errinfo)`'s hook is
`Some(vm.errinfo())`, so `AsmInst::LoadErrinfo` is the same read — with the
call's FP save set and GP flush gone with it. The load is equivalent only
because `rbx` is the *current* `Executor`, which is what `$!` is per (CRuby
keeps errinfo per execution context, and so does monoruby), so a Fiber or
Thread reads its own.

Only the internal name is specialized. `$(errinfo)` is what bytecodegen emits
and is not a name Ruby's parser can produce, so no program can alias it,
`trace_var` it or otherwise put a hook in the way; user-written `$!` reads keep
the generic path. The name is pre-interned as
`IdentId::GVAR_ERRINFO_INTERNAL` so the recognition is an integer compare
rather than a lock and a string hash.

Worth about 28% on a hot method with a trivial `ensure` (0.663-0.675 s ->
0.475-0.486 s over three runs). Together with the gate above, the surcharge for
putting a `begin`..`ensure` around a hot method's body falls from +63% to +23%
over the same method without one.

### 6.3 Spliced non-local exits (issue #1185)

A `break` / non-local `return` whose whole chain is specialized-inlined into
one JIT unit lowers to the **specialized teardown** (`lea rbp += Σ; leave;
ret`) — three instructions, no `handle_error`. An `ensure` on the way out
used to disqualify that outright: `nonlocal_exit_needs_vm_unwind` (§5) sent
the exit down the generic unwind, which interprets the `ensure` bodies,
converts the suspended frames by the chain-deopt walk, and — for `break` —
leaves the defining frame in the VM until the next `loop_start` re-enters by
OSR.

Splicing keeps the teardown and reaches the `ensure` body as ordinary
compiled code: the exit **defers** its unwind exactly as `handle_error`
would (`defer_block_break_at` / `defer_method_return_at`), and the region's
`EnsureEnd` delivers it — `ensure_end_spliced` classifies the parked
deferral and the compiled arm runs the teardown for that kind. The gain is
that the unwind edge now *exists in the CFG*, so the `ensure`'s writes are
visible to the abstract interpreter instead of happening behind its back.

This was built in two stages, by which frame owns the region. **Stage 1**
(`SplicePlan::SameFrame`, #1187) handled the exit's *own* frame, where the
body is a block of the iseq being compiled and the exit is an ordinary
forward branch to it. §6.5 removed the need for it: the exit now replays
its own frame's bodies inline and crosses no region of its own, so
`covering_ensure` never names the current frame at an exit's pc.
Measured over the whole test suite, stage 1 went from 304 splices to 0, and
it was deleted; `try_splice_exit` still refuses a same-frame host rather
than assuming, in case bytecodegen ever stops replaying. What remains is:

**An intermediate frame** (stage 2). The owner is a *suspended* frame: its
compile is parked at the call that leads to the exit, and its `ensure` body
is several machine frames away, so there is no branch to emit. The splice
travels by the machine's own return path instead:

1. `AsmInst::SplicedExitToOuter`, at the exit, builds the error where
   `vm.cfp()` is still the exiting frame (that is what resolves a `break`'s
   target) but keys the deferral on the **host** frame's LFP, read from the
   frame chain — `defer_block_break_at` / `defer_method_return_at`.
2. The same instruction then sets rbp to the frame the host called and
   `leave; ret`s. That lands at the host's call site with
   `SplicedExitKind::outer_tag()` in the return register — a value no
   normal return can produce (low three bits `000`, and no `RValue` lives
   at address 8 or 16).
3. `AsmInst::SplicedExitLanding`, emitted after that call whenever a nested
   compile asked for one, recognizes the marker and branches into the
   host's `ensure` body. It is an ordinary **side branch of the host's own
   CFG**, so the body's entry merge sees this path exactly as it sees the
   normal fall-through.

The landing edge's state is the host's state right after the call, which is
what the `ret` really arrives at, with two corrections: every widen the call
reached is re-applied (the resume adopts the *return* join's kept claims,
and a spliced exit is by definition not a returning path), and every temp
still void at the call is claimed as a boxed `Value` in its slot (a temp the
`ensure` body keeps live may only be written later in the begin body; the
prologue nil-fills the frame, so the claim is true, and nothing on this path
reads it).

One invariant is re-proved rather than inherited: the call site's own capture
guard (`immediate_evict`) is emitted *after* the landing, so
`defer_*_at` checks the host's `Meta` for the two bits `branch_if_captured`
tests and degenerates to the generic unwind when the callee promoted the host
frame to the heap. A degenerate error (`LocalJumpError` out of a proc-escaped
block) takes the same exit: nothing is torn down and the generic raise runs
from the exit's own pc.

`try_splice_exit` refuses everything it cannot prove: a dispatch arm, a
loop-rooted frame (whose compile may not cover the body), a `$!` restore
owed anywhere on the way out, more than one `ensure` to run (chaining hop by
hop is the natural extension — each `EnsureEnd` would tear down to the next
host — but is not built), a body that is not a basic-block head, a body
containing an exit of its own (`next` / `break` / `return` / `retry` /
`redo`, which would leave the deferral parked past the frame) or a nested
handler, and a host whose in-progress call site is not one of the two shapes
that emit a landing. Every refusal falls back to the generic unwind, which
handles every case.

Measured on the shape the issue names — a `break`-with-`ensure` that is the
normal exit of an inner iteration inside a hot loop in the block's defining
frame — this is worth roughly 10% (0.79–0.83 s → 0.69–0.77 s over
repeated runs). The same exit *without* the `ensure` runs in 0.35 s, so most
of what is left is the deferral machinery itself — two runtime calls and a
`MonorubyErr` per exit — rather than the unwind the splice removed. On a
chain that merely tears down (no hot continuation to return to) the
difference is within noise.

### 6.4 Replayed `ensure` bodies and the exception table

The two copies above are not the only ones. A **non-local exit written inside
a region** — a local `return`, a loop `break` / `next` / `redo`, `retry` —
does not go through `handle_error` at all: bytecodegen replays the bodies of
every region the exit leaves *inline*, innermost first, immediately ahead of
the exit instruction (`gen_all_pending_ensures`, `gen_loop_pending_ensures`).

Those inline copies sit lexically **inside** the very regions they replay, so
the exception table covered them like any other code in the region. A copy
that raised was therefore handed straight back to the region whose body was
running, and the body ran a second time:

```ruby
begin
  begin
    return :never
  ensure
    $log << :inner     # ran twice; CRuby runs it once
    raise "E"
  end
ensure
  $log << :outer
end
```

The generator already states the rule on its own side: while it emits the body
of the region at stack index `idx`, it truncates its `ensure` stack to
`ensures[..idx]`, so a `return` *written* in an ensure body does not
re-generate that body (and `begin return 1 ensure return 2 end` returns 2, as
in CRuby). The table now says the same thing at run time.

Each region gets an **id** (`BytecodeGen::new_region_id`), carried by every
exception-table entry it emits and by its entry on the `ensure` stack. Each
replayed copy is recorded as a **replay span** — a `BcIndex` range plus the
ids of the regions it runs outside of: the one whose body it is, and the inner
ones the exit has already replayed. `ISeqInfo::active_entries` drops those
entries, and every "which regions are in force at this pc" lookup goes through
it (`get_exception_dest`, `covering_ensure`, `single_covering_ensure`), so the
raise path and the non-local-exit path agree.

Ids rather than nesting depths, because a `begin` written **inside** an ensure
body is a region of its own and must keep catching:

```ruby
begin
  return :done
ensure
  begin
    raise "E"
  rescue => e     # still catches
  end
end
```

Bodies are generated once per copy, so that nested region gets a fresh id in
each copy and never collides with the region being replayed — where a depth
count would, since the generator's truncated stack gives it the same depth as
the region whose body it is in.

Spans nest, too (an ensure body may hold an exit that replays further bodies),
so a pc is checked against *every* span covering it, not just the innermost.
`errinfo_restore_slots` takes the same cut for the same reason — a replayed
body is preceded by its region's `$!` restore, so restoring again on the way
out would undo whatever the body did to `$!` — but it cannot go through
`active_entries`, because it is keyed on the rescue *clause* spans rather than
the region spans. It applies the replay-span filter directly
(`ISeqInfo::is_replayed_at`).

### 6.5 Non-local exits replay their own frame inline (issue #1185)

A `break` out of a block and a non-local `return` were the last exits that left
the job to `handle_error`: the unwinder found the covering region, deferred the
exit, ran the body interpreted, and re-delivered it from `EnsureEnd`. That is
what §6.3's splice machinery was built to compile around.

They now replay their own frame's open regions inline, exactly as `emit_ret`
does for a local `return` — same set, same order, same `$!` protocol
(`gen_method_return` / `gen_block_break` → `replay_ensures_for_nonlocal_exit`).
With the bodies emitted ahead of the exit there is nothing left to compile
around: the exit crosses no region, the JIT lowers it to the plain specialized
teardown, and no deferral is created.

The exit value is generated *before* the replay and popped *after* it, so the
bodies take their temps above it and the exit instruction's recorded `sp` is
unchanged — raising it by the value's own slot changed JIT liveness even for
exits that replay nothing, and cost about 4%.

What stops the bodies running a second time is the §6.4 machinery, reused
rather than duplicated: `emit_nonlocal_exit` extends the spans that replay just
recorded over the **exit instruction itself**. `handle_error` is handed exactly
that pc, and asks the same "which regions are in force here?" question the
raise path asks — so `covering_ensure` finds nothing to run,
`errinfo_restore_slots` nothing to restore, `nonlocal_exit_needs_vm_unwind`
answers `false`, and `try_splice_exit` (which starts from
`single_covering_ensure`) declines on its own. No separate table, and no third
place to keep in sync.

A span is therefore recorded even when the body generated no code: an
`ensure nil end` still has a region the unwinder would otherwise run through
the VM. The empty span is inert until the exit extends it.

Measured on the `break`-inside-its-own-`begin`..`ensure` shape, with a trivial
body so the machinery is what is being timed: **0.389 s → 0.207 s** against
0.185 s for the same loop with no `ensure` at all — a 2.1× surcharge down to
1.12×. With a body that does real work (`$n += 1`) the remaining gap is the
body: 0.690 s → 0.605 s. Standard benchmarks are unchanged.


---

## 7. Backtrace construction — the key contrast

This is where the laziness pays off and where the recent work (PR #896)
focused. A backtrace has three cost components, and monoruby defers each:

**(a) The raise→rescue frames.** These are captured incrementally by
`push_error_location` as `handle_error` unwinds each frame (§4 step 5). They
must be captured *during* unwinding because those frames are destroyed as the
stack pops — they cannot be walked later. Cost: one 3-word tuple push per
frame, **no string formatting**.

**(b) The frames *above* the rescuing frame** (the rest of the live stack at
raise time). The incremental capture never sees these, because unwinding stops
at the rescuing frame. CRuby includes them (its eager snapshot walked the whole
stack). monoruby fills them in at the **catch point** with
`Executor::complete_backtrace_for_rescue` (`executor.rs:complete_backtrace_for_rescue`,
called from `jit_module.rs:256` just before `take_ex_obj`):

```rust
// Walk the rescuing frame's callers via each inner frame's saved
// call-site pc — the same mechanism as Kernel#caller — appending the
// cheap (loc, sourceinfo, fid) tuples. No strings; formatting stays lazy.
```

Why the catch point, and not lazily at `#backtrace` time? Because it is the
**last moment the full stack is coherent**: the raise→rescue tuples are already
collected in (a), and the caller frames are still live (we are about to run a
`rescue` clause nested inside them). If we deferred this walk to `#backtrace`,
an exception object that escaped its rescue clause and was inspected later would
find those caller frames gone — yielding a truncated, wrong backtrace. CRuby
avoids the problem by snapshotting everything eagerly at raise; monoruby
snapshots the *caller half* at catch, which is strictly cheaper (only exceptions
that reach a real `rescue` pay for it) while remaining correct.

**(c) String formatting.** Fully deferred to `Exception#backtrace`
(`exception.rs:backtrace`), which turns the tuples into `"file:line:in
'method'"` strings and **memoizes** the resulting Array in the `/backtrace`
hidden ivar, so repeated calls return the *same* mutable object (matching
CRuby's `e.backtrace.equal?(e.backtrace)` and `e.backtrace.unshift(x)`
visibility). `#set_backtrace` writes the same `/backtrace` ivar, unifying the
explicit store with the memo.

`#backtrace_locations` is intentionally decoupled from the string backtrace via
the `__raise_backtrace` intrinsic (raise-time capture only), so
`set_backtrace(strings)` on a never-raised exception keeps
`#backtrace_locations` nil, while an Array of `Thread::Backtrace::Location`
sets both — matching CRuby 3.4+.

### Cost summary for the hot paths

| scenario                              | backtrace cost in monoruby                          |
| ------------------------------------- | --------------------------------------------------- |
| `return` from block, `break`, `throw` | **none** — dispatched before trace capture (§3b)    |
| `StopIteration` caught by `loop`      | a few tuple pushes only — `loop` catches at the Rust level (`err.is_stop_iteration()`, `kernel.rs:908`), so it never hits a bytecode `rescue`, so `complete_backtrace_for_rescue` and `take_ex_obj` are **never called** |
| exception caught by a Ruby `rescue`   | raise→rescue tuples + one caller-stack walk (tuples only); strings only if `#backtrace` is called |
| uncaught exception (top level)        | full tuple trace; formatted once by the reporter    |

### CRuby contrast (backtrace)

- **When captured:** CRuby eagerly at raise; monoruby incrementally on unwind +
  once at catch.
- **What is stored:** CRuby a `rb_backtrace_t` (frame snapshots); monoruby cheap
  `(loc, sourceinfo, fid)` tuples.
- **`#backtrace` strings:** both format lazily and memoize; monoruby in the
  `/backtrace` ivar.
- **Control-flow tags:** neither builds a Ruby backtrace for them.
- **Frame labels:** monoruby renders owners with their fully-qualified name
  (`Ns::Cx.foo`, special-casing `Object#foo`) in `func_description`
  (`../monoruby/src/globals/store.rs`), matching CRuby's `Ns::Cx.foo`.

---

## 8. Materializing the exception object — `take_ex_obj`

`take_ex_obj` (`executor.rs:1174`) converts the in-flight `MonorubyErr` into a
Ruby `Value`, called only at a catch point or the top level:

- **Re-raise** (`err.original` set): return the *same* object, filling its
  `trace` only if still empty (CRuby assigns a backtrace only when the
  exception lacks one).
- **Fresh object:** allocate `Value::new_exception(err)` and attach
  kind-specific hidden ivars — `LoadError#path`, `SystemExit#status`,
  `NoMethodError#{name,receiver}`, `NameError#{name,receiver}`,
  `KeyError#{receiver,key}`, `FrozenError#receiver`, `LocalJumpError#exit_value`
  + `#reason`, `StopIteration#result`, `SyntaxError#path`, … Hidden ivars use
  `/`-prefixed names so they are excluded from `#instance_variables`.
- **Cause chaining** (`chain_cause`, `executor.rs:1335`): an explicit `cause:`
  keyword wins; otherwise CRuby's `exc_setup_cause` — if a different exception
  is currently being handled (`$!`), record it as `/cause`. `cause: nil`
  suppresses the implicit chain.

### CRuby contrast

CRuby builds the exception object at `raise` (it *is* the raise). monoruby's
split — Rust `MonorubyErr` while in flight, Ruby object only at catch — is what
lets it skip object allocation entirely for the immediately-caught and
control-flow cases. The materialized object's ivar layout and cause semantics
are kept CRuby-compatible.

---

## 9. Fatal errors

A Rust `panic!` caught at an `extern "C"` trampoline becomes
`MonorubyErrKind::Fatal`. `is_fatal()` (`error.rs:1137`) makes `handle_error`
skip **both** `rescue` and `ensure` and propagate straight to the top
(`jit_module.rs:247`), because VM/interpreter state may be inconsistent after a
panic. This matches CRuby's uncatchable `fatal` — not interceptable even by
`rescue Exception`.

---

## 10. Top-level reporting

An exception that reaches the top uncaught is printed by the reporter in
`error.rs` (`show_error_message_and_all_loc`, `error.rs:167`): the message line
plus each caller frame as `\tfrom <file>:<line>:in '<method>'`, honouring
`--backtrace-limit=N` (extra frames collapse into `\t ... K levels...`). The
compact single-location form (`show_error_message_and_loc`) is used where CRuby
prints only the origin (e.g. `SyntaxError`, which also gets a source excerpt).

---

## 11. File map

| concern                              | location                                             |
| ------------------------------------ | ---------------------------------------------------- |
| in-flight error type + kinds         | `../monoruby/src/globals/error.rs`                   |
| unwinder                             | `../monoruby/src/codegen/jit_module.rs` (`handle_error`) |
| set/take error, `$!`, ensure defer   | `../monoruby/src/executor.rs`                        |
| catch-time caller walk               | `../monoruby/src/executor.rs` (`complete_backtrace_for_rescue`) |
| object materialization + cause       | `../monoruby/src/executor.rs` (`take_ex_obj`, `chain_cause`) |
| per-method exception table           | `../monoruby/src/globals/store/iseq.rs`              |
| JIT-spliced non-local exits (§6.3)   | `../monoruby/src/codegen/jitgen/context.rs` (`try_splice_exit`), `jitgen/compile.rs` (`emit_spliced_exit`), `jitgen/compile/method_call.rs` (`emit_spliced_landing`) |
| frame-label rendering                | `../monoruby/src/globals/store.rs` (`func_description`) |
| Ruby `Exception` API (Rust side)     | `../monoruby/src/builtins/exception.rs`              |
| Ruby `Exception` API (Ruby side)     | `../monoruby/builtins/startup.rb`                    |
| `Kernel#raise` / `#loop` / `#caller` | `../monoruby/src/builtins/kernel.rs`                 |
| differential tests                   | `../monoruby/tests/backtrace.rs`, `tests/exception_api.rs` |
| spliced-exit regression tests        | `../monoruby/tests/nonlocal_exit_ensure.rs`, `tests/nonlocal_exit_intermediate_ensure.rs`, `tests/nonlocal_exit_rescue.rs` |
| `EnsureEnd` gate tests (§6.1)        | `../monoruby/tests/ensure_end_deferral_gate.rs` |
| region-entry `$!` tests (§6.2)       | `../monoruby/tests/errinfo_inline_load.rs` |

---

## 12. Design summary

monoruby's exception mechanism is CRuby-compatible at the Ruby surface
(`rescue`/`ensure`/`retry`/`redo`, `Exception` API, cause chaining, backtrace
format, uncatchable fatals) while diverging in **when** work happens:

- **Raise stores the minimum** — a Rust `MonorubyErr` with cheap trace tuples;
  no Ruby object, no formatted strings.
- **The stack is recorded incrementally on unwind**, not snapshotted eagerly.
- **Caller frames are completed once, at the catch point** — the last coherent
  moment — not eagerly at raise and not unsafely late at `#backtrace`.
- **Control-flow constructs pay nothing for backtraces** because they are
  dispatched before trace capture, and internal `StopIteration` (via `loop`) is
  caught at the Rust level below the bytecode-`rescue` path.

The net effect is CRuby-equivalent observable behavior with the backtrace cost
concentrated on exactly the exceptions that are genuinely caught and inspected.
