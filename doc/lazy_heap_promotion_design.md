# Lazy Heap Promotion — Design Plan

Fix the `Array#permutation(n)` / `Integer#downto` wrapper regression
described in `doc/permutation_break_bug.md` by deferring the stack →
heap promotion of a block's captured outer frame until that outer
method actually returns (YARV-style).

This document is the plan. Implementation follows it.

## Invariant we want to restore

**At any instant, there is exactly one canonical storage location for
a frame's registers.** Reads and writes through any handle — the live
`cfp`, the frame's own `r14`, a Proc's `outer_lfp`, a Binding — must
all see the same data.

Today, `Lfp::move_frame_to_heap` breaks this invariant: after the
move, the stack frame still exists and is read by the active method's
`r14`, while the heap frame is read by every descendant Proc via
`outer_lfp`. Writes to each diverge.

## Strategy

Two stages:

1. **Stack-only until escape is unavoidable**: Proc / `&block` /
   lambda captures set `outer_lfp` to a *stack* Lfp and remember, in
   a per-cfp list, that "this Proc's `outer_lfp` field must be
   rewritten when the cfp dies".

2. **Promote at frame pop**: right before a method's stack frame is
   released (epilogue / error unwind / fiber yield), walk the list
   for this cfp; if non-empty, copy the stack frame to heap and
   overwrite every registered `*mut Lfp` with the heap pointer.

While the method is running, there is still only one physical copy
(stack). When the method returns, it atomically moves to heap and
every escapee is updated in lock-step — no window where both copies
are live and writable.

## Data structures

Add to `Executor`:

```rust
/// For each still-live cfp that has produced at least one escape
/// (Proc / Binding / Enumerator outer pointer), a list of raw
/// pointers into those escapees' `outer_lfp` field.
///
/// Populated by `register_escapee` when an `&block` / lambda /
/// `Proc.new` / `Kernel#binding` captures a stack frame. Drained by
/// `promote_frame_on_return` at every exit from the matching cfp.
escapees: rustc_hash::FxHashMap<CfpAddr, Vec<*mut Lfp>>,
```

`CfpAddr` is a newtype around `usize` (the raw cfp pointer) since
`Cfp` is not `Hash`. The cfp address is stable while the frame is
alive and unique across live frames.

We skip the cfp lookup entirely in the common (no-escape) case by
adding **one flag bit** to the cfp's LFP meta:

```
Meta.kind bit 3:  0:no_escapee  1:has_escapee
```

`generate_proc_inner` sets the bit the first time it registers an
escapee for a cfp; the epilogue checks the bit in assembly.

## Code surface

### In `src/executor/frame.rs`

- `Lfp::move_frame_to_heap` stays, but becomes internal-only. It is
  still called from `promote_frame_on_return` to do the actual
  physical copy. Behaviour unchanged (still recursive on outer —
  because at the exit point the outer may also need promotion, and
  we WANT that transitively).
- Add `Lfp::detached_heap_copy(self) -> Self` that does what
  `move_frame_to_heap` does but **without** mutating `cfp.lfp`. The
  heap copy is a snapshot; nothing else refers to it. Used only by
  the promotion path.
- `Cfp::caller()` stays as-is: after the lazy fix, the outer lfp
  pointer stored in a Proc/Binding is always either pure-stack (no
  redirect) while the owning cfp is alive, or pure-heap (updated by
  promotion) after it has returned. The pointer-comparison in
  `caller()` is correct in both halves.

### In `src/executor.rs`

- `generate_proc_inner`, `generate_lambda`, `generate_binding`:
  - get the stack outer lfp (as today via `resolve_block_target` /
    `cfp.lfp()`)
  - allocate the Proc/Binding with `outer_lfp` = that stack lfp
  - call `vm.register_escapee(owner_cfp, &mut proc.outer_lfp as *mut Lfp)`
  - set the `has_escapee` meta bit on owner_cfp's lfp
  - recurse up the outer chain: for each ancestor that is still on
    stack, register an escapee for that cfp too so it gets promoted
    when it returns (this is how we handle the recursive-move
    semantics that today's code does eagerly)

- `pub(crate) fn promote_frame_on_return(&mut self, cfp: Cfp)`:
  - If `cfp.lfp().meta().has_escapee()` is false, return (fast path
    skipped by assembly flag check anyway — this is the
    belt-and-braces check).
  - Remove the entry from `self.escapees` for this cfp.
  - Detached heap copy of the stack frame.
  - For every `*mut Lfp` in the list, write the new heap lfp into
    `*ptr`.
  - (Outer chain: each escapee points to the captured stack lfp; if
    that frame owns its own outer, the outer chain inside the new
    heap copy still points at a still-live stack ancestor, which is
    fine — that ancestor will also have an escapee registration and
    promote when it returns.)

- `register_escapee(cfp, *mut Lfp)`:
  - Insert into the HashMap.
  - Set `has_escapee` bit on `cfp.lfp()`'s meta.

### In VM / JIT epilogue (`src/codegen.rs`)

Add the promote-on-return hook to exactly five places:

1. `Codegen::epilogue` — normal method return (`leave; ret`).
2. `Codegen::method_return_specialized` — `return` from inside a
   block.
3. `Codegen::method_return` — same, non-specialised.
4. The unwind path inside `handle_error` when it returns
   `ErrorReturn::return_err` (a frame is about to be popped).
5. The fiber yield path — a fiber yields, parent resumes. The fiber
   frame isn't dead yet but becomes inaccessible; we can either
   skip (the fiber itself is a Ruby object and survives) or
   treat it as a normal return. Deferring decision until impl.

Each of the five insert an assembly sequence:

```
movzbl  r10, [r14 - LFP_META + 4]  ; load Meta.kind
test    r10, FLAG_HAS_ESCAPEE
jz      epilogue_fast              ; common case: skip
                                   ; slow case:
movq    rdi, rbx                   ; vm
call    runtime::promote_on_return
epilogue_fast:
leave
ret
```

The flag is a single immediate bit test. No HashMap lookup on the
happy path.

### Exception / unwind

`handle_error` in `src/codegen/jit_module.rs` returns one of three
statuses. When `ErrorReturn::return_err`, the caller (JIT dispatch
code) pops the current frame. We add a promote check there too.

When `ErrorReturn::goto(dest)`, the current frame is *not* popped
(execution just jumps to a rescue/ensure label inside the same
method). No promotion needed.

When `ErrorReturn::return_normal(val)`, the frame is being returned
with a value — promote then.

### GC

GC marking already iterates:
- every live `cfp` (via `vm.cfp().prev()` chain), marking each
  `lfp`'s registers.
- every Ruby-visible value (Proc, Binding, Enumerator, ...), each of
  which owns its `outer_lfp` and marks that too.

With lazy promotion, a Proc's `outer_lfp` while the owner method is
still alive is a stack pointer. The live-cfp walk already visits
that frame. Mark through it is safe (it's the same memory the live
walk processes).

A Proc's `outer_lfp` after the owner has returned is a heap pointer
(updated by promotion). GC marks through it normally.

We need one small invariant: **don't mark a stack frame twice** (once
via cfp walk, once via Proc.outer_lfp walk). Monoruby's current
marking already handles duplicate visits via the generation counter
in `GC<RValue>`. Confirm this during implementation.

### Fibers

A fiber's Ruby stack lives inside the fiber object. When control
switches out of a fiber, its cfp chain is suspended; when it resumes,
execution continues. Stack lfps are valid across yields because the
memory isn't freed until the fiber itself is GC'd.

Risk scenario: a Proc captured in fiber A holds a stack lfp pointing
into fiber A's stack. From fiber B, that Proc is called. The read
goes to the fiber A stack — which is still alive. OK.

Real risk: the fiber dies (terminates) with a Proc outstanding. The
fiber's stack memory is freed. The Proc's outer_lfp is now dangling.
We need to promote-on-fiber-termination. Add the same hook to fiber
termination.

## Edge cases to validate during impl

1. **Transitive outer through multiple stack ancestors** (the minimal
   repro — `/main` → loop → BlockA → &block). Multiple escapee
   registrations up the chain: each ancestor promotes on its own
   return. The heap frame's `outer` pointer is updated by the
   intermediate promotion (its own escapee entry gets rewritten when
   its outer returns).

2. **Proc captured then immediately invoked** (today's fast path).
   Owner frame still alive; Proc reads stack; writes to Proc-local
   vs. captured-closure vs. outer — all go to the same stack
   location. ✓

3. **Proc captured, outer returns, Proc invoked later**. On outer
   return, promote runs, Proc.outer_lfp becomes heap. Subsequent
   invocation reads heap snapshot. Writes land on heap. The original
   method is gone, there's no consistency issue. ✓

4. **Proc captured inside Proc** (nested closures). Inner Proc's
   outer is the outer Proc. Each has its own escapee chain. When
   either's owner returns, that level promotes. ✓ (needs careful
   test).

5. **`Binding.of_caller`-style access** — monoruby has a `Binding`
   object. Its `outer_lfp` needs the same treatment. Ensure
   `Kernel#binding` goes through the escapee path.

6. **Lambda returned from a method, then called much later after
   many intervening cfps have come and gone**. At call time, the
   lambda's outer_lfp is heap (owner already returned and promoted).
   ✓

## Implementation order

1. **Plumbing first** — add `has_escapee` bit, `Executor.escapees`,
   `register_escapee`, `promote_frame_on_return`, assembly flag
   check wrapper. No behavioural change yet (the check is a no-op
   unless something registers).
2. **Switch `generate_proc_inner`** to register + stack-outer. Test
   with the minimal repro + `core/proc` + `core/enumerator`.
3. **Switch `generate_lambda` and `generate_binding`**. Test with
   `core/proc` / `core/method` / the existing Binding tests.
4. **Hook the error unwind path and fiber**. Test with
   `core/exception` / `core/fiber`.
5. **Remove the eager recursive move** inside `Lfp::move_frame_to_heap`.
   At this point all promotion is driven by return hooks.
6. **Full ruby/spec core regression**.

Between each step, `core/numeric`, `core/array`, `core/proc`,
`core/enumerator`, the minimal repro, and the specific partial
permutation check must remain green / improve.

## Risks & mitigations

- **Assembly epilogue invasiveness**. Five sites, but each addition
  is a few instructions gated by a flag test. Measured cost on the
  no-escape path: one `test reg, imm` + one `jz`. Acceptable.
- **GC double-mark**. Existing marking is idempotent — GC uses a
  mark-bit / generation counter, so visiting the same RValue twice
  is a no-op. We will still verify.
- **Dangling escapee entries on panic / non-local exit**. If a
  frame is popped in a path that doesn't reach our hook, escapee
  entries stay in the HashMap keyed to freed cfp addresses. Next
  allocation at the same address would misfire. Mitigate by
  hooking every pop site (epilogue, error return, fiber
  terminate); add a debug assertion that `escapees` is empty at VM
  top-level exit.
- **Stack address reuse**. Within a single deep recursion, the same
  stack address can be reused across frames. But because we drain
  the entry on pop (before the address is reused), the next
  installation at that address has a fresh entry. ✓.
- **Nested fibers**. Current monoruby is single-threaded with
  cooperative fibers. A Proc from fiber A invoked while fiber B is
  running accesses fiber A's stack; we need a way to detect "my
  frame is currently on a *different* fiber's stack, which is
  suspended". The existing `Executor::parent_fiber` chain handles
  this for `caller()` traversal already; extend the promote path
  similarly if needed.

## What we are *not* fixing in this pass

- The 2 `core/integer` `lazy-validation` errors (`1.upto("A").size`
  raising at `.size` rather than at `upto` call time). They reappear
  if the Ruby wrapper for `Integer#upto`/`#downto` is restored, and
  the wrapper becomes safe to restore only *after* this plan lands.
  Restoring the wrapper is a follow-up PR.
- `Array#permutation`'s `permutation(n == size)` ↔ `permutation(n)`
  Ruby-level algorithm refinements — orthogonal.
- JIT-side cached frames (Side exits, etc.). The `r14` register is
  loaded from `cfp.lfp()` when entering a frame and maintained in
  register for the duration; no mid-method reload, which is what we
  rely on. Verify no JIT op re-reads `lfp` expecting consistency
  after an `&block` capture.
