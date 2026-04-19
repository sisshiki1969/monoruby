# `Array#permutation(n)` partial-case regression — root-cause report

Branch for investigation: `claude/permutation-debug` (no code
changes retained; this report lives on `claude/enumerator-size-master`
as documentation only).

## Symptom

With a Ruby-level `Integer#downto` / `#upto` wrapper installed, i.e.:

```ruby
class Integer
  def downto(limit, &block)
    if block
      __downto(limit, &block)       # Rust kernel
    else
      to_enum(:__downto, limit)
    end
  end
end
```

partial permutations regress:

```
[1,2,3,4,5].permutation(3).to_a.size   # expected 60, got 21 (non-det)
```

Full permutations (`n == size`) are unaffected.

## Minimal reproducer (no permutation involved)

The bug is in the interaction between `&block` forwarding and `break`
inside an enclosing loop block. Reducing it:

```ruby
outer_iter = 0
loop do                          # BlockA — outer = /main
  outer_iter += 1
  break if outer_iter > 3        # <-- raises LocalJumpError on iter 4
  2.downto(0) do |i|             # BlockB — outer = BlockA
    break
  end
end
```

Expected: `outer_iter == 4`. Observed: `LocalJumpError: illegal break
from block`. The error does **not** come from `break` inside BlockB
(that one works correctly for iterations 1-3). It comes from the
ordinary loop-exit `break` in BlockA on the fourth iteration.

## Mechanism

`break` inside a block is compiled to `BlockBreak`. At runtime it
calls `err_block_break`
(`monoruby/src/codegen/runtime.rs:1005`), which computes the break
target by walking the caller chain:

```rust
let target_lfp = self.lfp().outer()?;         // lexical outer
let mut cfp = *self;
loop {
    let cfp_prev = cfp.prev()?;               // walk prev-cfp
    if cfp_prev.lfp() == target_lfp { … }     // match by lfp *pointer*
    cfp = cfp_prev;
}
```

It looks up a cfp on the stack whose *pointer* matches the block's
outer lfp. If no cfp matches, `caller()` returns `None`, and
`err_block_break` raises `LocalJumpError: illegal break from block`.

## The actual bug: `Lfp::move_frame_to_heap` redirects the live cfp

`monoruby/src/executor/frame.rs:330`:

```rust
pub fn move_frame_to_heap(self) -> Self {
    if self.on_stack() {
        unsafe {
            let mut cfp = self.cfp();
            …
            let mut heap_lfp = Lfp::new(…);
            heap_lfp.meta_mut().set_on_heap();
            cfp.set_lfp(heap_lfp);                // <-- (A)
            if let Some(outer_lfp) = heap_lfp.outer() {
                let outer = outer_lfp.move_frame_to_heap();  // <-- (B)
                heap_lfp.set_outer(Some(outer));
            }
            …
            heap_lfp
        }
    } else { self }
}
```

Two points:

* (A) the move is **destructive**: the live cfp's lfp pointer is
  mutated to point at the *heap* copy. The original stack frame
  becomes orphaned but the cfp still sits on the call stack.
* (B) the move is **recursive** up the outer chain. Moving BlockA's
  frame also moves `/main`.

`generate_proc_inner`
(`monoruby/src/executor.rs`) calls this in the proxy arm via
`outer.move_frame_to_heap()` — exactly what the `&block` forwarding
path triggers, because `def downto(limit, &block)` captures the block
as a Proc, and Procs must outlive their method, so their outer lfp has
to live on the heap.

So the sequence inside the reproducer is, per outer-loop iteration:

1. Enter BlockA (new stack frame). BlockA.outer = /main (stack lfp).
2. Evaluate `2.downto(0) { … }`. The block arg to `#downto` is
   captured into a Proc via `&block`.
3. `generate_proc_inner` calls
   `cfp.lfp().move_frame_to_heap()` to make the Proc's outer safe.
   That move:
   * copies BlockA to heap, mutates `BlockA.cfp.lfp = heap_A`;
   * recursively copies /main to heap, mutates
     `main.cfp.lfp = heap_main_i` (a **fresh** heap allocation this
     iteration).
4. The Proc (wrapping BlockB) is passed into `__downto`; break inside
   BlockB resolves against `my_downto`, fine.
5. Next iteration: `loop` re-invokes BlockA with a fresh stack frame.
   The invoker sets the new `BlockA.outer` from
   `main.cfp.lfp`, which is `heap_main_i` from step 3.
6. Go back to 2, and BlockA's outer is re-assigned to a **new**
   `heap_main_{i+1}` after the next `move_frame_to_heap` call.

The problem surfaces when BlockA runs `break` (step: `break if
outer_iter > 3`):

* BlockA.lfp.outer() = `heap_main_N` (whichever heap copy was active
  when BlockA was last set up)
* `main.cfp.lfp` currently = `heap_main_M`, where `M >= N`, but the
  two can differ because the recursive move on every iteration
  allocates a **fresh** heap copy and `cfp.set_lfp` overwrites the
  pointer.

`caller()` walks the prev-cfp chain looking for a cfp whose `lfp ==
heap_main_N`, but the live `/main` cfp now stores `heap_main_M`. No
match → `None` → `LocalJumpError`.

The instrumented trace confirms this:

```
iter 1  self_lfp=…b678  outer=0x5569…e850   (moved)
iter 2  self_lfp=…b678  outer=0x5569…e8d0   (different heap)
iter 3  self_lfp=…b678  outer=0x5569…bdd0   (different heap, again)
iter 4  self_lfp=…bc18  outer=0x7edf…c0a8   caller()=None → illegal
```

Each iteration produces a **different** outer lfp for BlockA. No
stack cfp ever holds the exact pointer BlockA.outer points to at
iteration 4's break.

The same reproducer with a pure-Ruby downto that uses `yield`
(no `&block`, no Proc creation, no `move_frame_to_heap`) keeps BlockA's
outer stable across iterations and `break` works correctly.

## Why full `permutation(n == size)` works

The full-permutation branch in `builtins/array.rb` doesn't hit the
same cfp-churn: its nested `(n - 1).downto(0) { … break … }` runs
inside an `each`/method frame whose cfp prev chain gets lucky and
finds a matching lfp before hitting the corrupted `/main`. The
partial branch has an extra layer (the outer `loop { … }` wrapping
the inner `downto` across many iterations), and it eventually loses
the match.

## Fix directions (ranked by risk)

1. **Non-destructive heap promotion**. Keep the stack frame live and
   back the Proc with a heap-allocated *snapshot* that is separate
   from `cfp.lfp`. Stop mutating the live cfp's lfp pointer in
   `move_frame_to_heap` while the frame is still executing. Promote
   to heap only at frame exit (or copy once, and lock further moves
   to be no-ops while the stack copy is still live). Low-risk once
   we add an "on heap snapshot" bit so `cfp.lfp` can stay on stack
   and `Proc.outer` points to the snapshot.
2. **Stable lfp identity token**. Decouple `cfp.caller()`'s matching
   from raw lfp pointers. Store a stable id (e.g. frame-creation
   counter) in the frame header; use it for identity checks while
   leaving the lfp pointer free to move. Higher-risk: many sites
   compare lfp pointers directly.
3. **Don't triple-move on repeat invocations**. `move_frame_to_heap`
   already has an on-stack / on-heap fast path. But the *recursive*
   move in step (B) re-allocates the outer heap copy every time
   because the outer's `on_stack()` flips to `on_stack` again each
   new frame invocation even if the outer's *identity* (via prev
   chain) is unchanged. A narrower fix: don't recursively move the
   outer if the prev-cfp chain already points to a valid heap frame
   for that lfp.

Any of the three unblocks the Ruby `Integer#upto` / `#downto`
wrapper pattern — and, by extension, lets `Range#step`,
`Enumerator::ArithmeticSequence` and other Ruby wrappers thread a
size proc through `to_enum` without risking `LocalJumpError` in
unrelated enclosing loops.

## Why a "check-before-copy" patch is not enough (experiment log)

An obvious-looking tweak: **"before copying the frame, see if it's
already on the heap and skip the copy if so."** The existing code
already does this at the first level (`if self.on_stack()`), and we
tried extending it to detect a stale stack `Lfp` pointer whose cfp
had been previously redirected, plus a matching extension in
`Cfp::caller()` so that the pointer comparison succeeded when the
block's `outer_lfp` still named the original stack slot:

```rust
// move_frame_to_heap entry:
let cfp = self.cfp();
if cfp.lfp().as_ptr() != self.as_ptr() {
    return cfp.lfp();         // stack slot was already redirected
}
```

```rust
// Cfp::caller(): normalise the target through its owning cfp
if target_lfp.on_stack() {
    let owner_cfp = unsafe { Cfp::new(target_lfp.as_ptr().add(16) as _) };
    let live = owner_cfp.lfp();
    if live.as_ptr() != target_lfp.as_ptr() {
        target_lfp = live;
    }
}
```

With both patches applied, the reproducer *no longer* raised
`LocalJumpError`:

* `[1,2,3,4,5].permutation(3).to_a.size` returned **60** (correct),
* the minimal-repro loop actually ran 4 iterations.

**But the loop counter came out wrong** (`1` instead of `4`). Reason:
`BlockA` writes `outer_iter` through `outer_lfp` -> heap `/main`
copy, while `/main` itself continues executing with `r14` pointing
at the *stack* `/main`. The frame's locals exist in two places that
diverge.

So the patch turns the `LocalJumpError` into a silent correctness
bug. The architectural invariant that `move_frame_to_heap` was
written against is "once we have promoted the frame to heap, nobody
still reads through the stack copy" — and that's violated as soon as
you recursively promote an outer frame that is still actively
executing (a `/main` that is still waiting for `Kernel#loop` to
return). A fix localised to `move_frame_to_heap` / `caller()` can
only paper over one symptom at a time.

The real fix has to address the invariant:

1. **Don't promote an outer that is still live.** Defer the heap
   promotion until the outer actually returns (or the Proc is
   first invoked past the outer's return). Until then, hold the
   Proc's `outer_lfp` as a "tag" that resolves to the current stack
   slot on demand. This is the YARV approach.
2. **Alias the stack and heap copies.** Either treat the stack slot
   as a cache of the heap (write-through) or vice versa. Expensive
   in the register-based VM but avoids the active-frame issue.
3. **Avoid the round-trip in user code.** Keep `Integer#upto` /
   `#downto` in Rust, accept the lazy-validation spec regression,
   and document the pattern "Ruby wrapper -> `&block` -> Rust
   builtin" as unsafe until (1) or (2) lands.

The check-before-copy idea was a good question to validate — it
falsifies itself in ~20 minutes because the counter test is a
single-line change and catches the write/read split immediately.

## What was temporarily needed to reproduce

* Rename Rust `upto` / `downto` registrations to `__upto` /
  `__downto`.
* Define Ruby wrappers in `builtins/integer.rb`:
  ```ruby
  def upto(limit, &block)
    block ? __upto(limit, &block) : to_enum(:__upto, limit)
  end
  def downto(limit, &block)
    block ? __downto(limit, &block) : to_enum(:__downto, limit)
  end
  ```

All three changes are **not** in the tree after this investigation —
the report is the deliverable.
