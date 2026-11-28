# Escape analysis: eliding Array and Range literals and `*rest`

`[a, b].max`, `[:get, :head].include?(verb)`, `[x, y].first`,
`(lo..hi).include?(v)`, `case v when 1..5`: an Array or Range literal
that is built only to be the receiver of one call, and is dead right
after it. The JIT does not allocate such a literal when it is compiled.
It records it in the abstract state as an **elided object** and decides
at its use whether the object is needed at all. The same object can be
handed, still elided, into a specialized callee's parameter — as the
literal argument of `size([a, b])`, or as the `*rest` parameter the
callee binds `f(a, b)` into (§6).

This replaced `try_fuse_array_minmax`, a peephole that recognized exactly
`array` + `min`/`max` on the next instruction. The pattern-specific part is
now data: what a callee may be handed instead of a real Array is its
**receiver escape summary**, declared next to the builtin.

Source: `codegen/jitgen/compile/escape.rs`; the state half in
`state/slot.rs` (`SlotState::virtuals`, `LinkMode::from_caller`) and
`state.rs`; the side-exit half in `WriteBack::virtual_obj`
(`codegen/jitgen.rs`, and `a64_gen_virtual_materialize` on aarch64); the
summaries in `builtins/array.rs` and `builtins/range.rs`
(`init_recv_escape`, `init_recv_escape_ruby`) and `executor/inline.rs`
(`RecvEscape`, `ElementsOp`); the Range runtime helpers in
`codegen/runtime.rs` (`create_range`, `range_cover`, `range_fold`).

**The scope is deliberately narrow.** Until 2026-11 the analysis also
kept objects in local variables (with a per-local scratch window
reserved by the bytecode compiler), carried them through forward merges
and loop heads, handed a returned object back to the caller's slots,
folded `x[i]` on an elided Array, folded lookups on Hash literals and
elided `**kwrest` parameters. Measured with each stage switched off, none
of them moved any of the 22 ruby-bench benchmarks (cachegrind
instruction counts), while together they were about 1,700 lines across
the bytecode compiler, the merge / loop machinery and both backends, so
they were removed. What is left is the part that pays: a literal
consumed where it is built, `*rest` and `...` handed into callees, and
Range literals.

## 1. The elided object

Three instructions and a method entry can produce one:

| Where | `VirtualSrc` | Condition |
|---|---|---|
| `%d = array[%s..%s+n]` | `Slots { src, len, callid }` | no splat; `%d` and the elements are temporaries, the elements dead once the literal is built (at or above the stack top) |
| `%d = literal[...]` | `Literal(template)` | `%d` is a temporary; the template is a plain `Array` whose elements are all immediates, or a `Range` whose endpoints are immediates |
| `%d = %s..%e` / `%s...%e` | `Range { start, end, exclude_end }` | `%d` is a temporary; each endpoint is a compile-time Fixnum or `nil` (`RangeEnd::Const`) or a slot proven to hold a Fixnum (`RangeEnd::Slot`), so building the Range later can neither dispatch `<=>` nor fail (§7) |
| a specialized callee's parameter | `CallerSlots { src, len, base }`, or the caller's `Literal` | the caller handed the object over (§6) |

The slot `%d` becomes `LinkMode::V` and the object lives only in
`SlotState::virtuals`. A `Slots` object's element slots, and a `Range`
object's endpoint temporaries, are *pinned* (`VirtualSrc::own_slots`):
`clear_above_next_sp` leaves them alone although the bytecode considers them
dead, because they are the object's contents. A `Literal` object needs no
storage: its contents are the template's.

The object ends in one of four ways.

- **Consumed** by its use (§2): the Array is never allocated.
- **Handed over** to a specialized callee as a parameter (§6), where it
  ends in one of these ways again.
- **Dropped**: its slot is overwritten, or falls above the stack top, without
  being read; or it is a parameter nothing reads any more; or it is the
  discarded result of a specialized callee (§8). Nothing ever saw it.
- **Materialized**: allocated after all, by exactly the code the literal
  would have run (`NewArray` / `DeepCopyLit`, `CreateArrayFrom` over the
  caller's window, or `CreateRange` from the endpoints), at the point
  where something could first observe it.

## 2. Consuming uses

A use that may consume the object is a method call with it as the
receiver, and nowhere else in the call's operands
(`may_consume_virtual`); for a `Literal` also the `%d = %base[%idx]`
index instruction (`consume_virtual_index`), which is `Array#[]` resolved
the same way; for a Range also the `===` comparison with it on the left
(`consume_virtual_teq`: the `case … when lo..hi` opcode and the explicit
`(lo..hi) === v`), which is `Range#===` resolved the same way. The call
resolves the method on the object's class (`VirtualSrc::class_id`) under
the class-version guard and reads the callee's `RecvEscape`:

- `elements: Some(op)` — an allocation-free lowering over the object's
  elements, for a `Slots` or `CallerSlots` object and an argument-less,
  block-less call. `Min` / `Max` compare the elements in their slots
  (`ArrayMinMax`, over the frame's own slots or the caller's window), in
  the order the call it stands for would: `best <=> v` for `Array#min`,
  `v <=> best` when the call is written on the literal itself
  (`[a, b].max`, CRuby's `opt_newarray_send`; `CallSiteInfo::newarray_recv`,
  `MinMaxOp`) — which is the operand a failed comparison names (#2007);
  `Size`, `Empty` are answered at compile time; `First`, `Last` are a copy
  of one element slot (`LoadCallerSlot` for a handed-over object). Over
  the endpoints of an elided Range (`Const` or `Slot`), `RangeCover`
  (`include?`, `member?`, `cover?`, `===`) is two tagged compares inline
  for a Fixnum argument — the tag bit is the same on both sides, so
  Fixnums order as tagged words, and a `nil` endpoint bounds nothing —
  with `runtime::range_cover` behind it for any other argument;
  `RangeSize`, `RangeCount`, `RangeSum`, `Min`, `Max`, `First`, `Last`
  are folded at compile time for constant endpoints
  (`runtime::range_fold`) and go through a helper otherwise;
  `RangeBegin`, `RangeEndpoint`, `RangeExcludeEnd` are the endpoint
  itself (`plan_range_op`, `emit_range_plan`). Where the in-place answer
  does not apply — a non-numeric argument, a `nil` endpoint the method
  rejects, a redefined `Integer#<=>` — the helper builds the Range and
  dispatches the resolved method on it, exactly the call that would have
  been made. A parameter stays elided after such a use if it is still
  live (the next `args.size` is free too); a temporary is done.
  None of these applies when a positional argument of the call is itself
  an elided object (`(1..10).cover?(1..5)`, `Range#cover?` being a Ruby
  method that takes it, §6): each object is judged on its own
  (`may_consume_virtual`), but the lowering here reads the argument slots
  as values, and an elided argument's slot holds none. The call is then
  the one that takes both — the receiver is materialized (a frozen
  literal is its template, at no cost) and the argument handed over
  (`consume_virtual`'s `args_elided`).
- `transparent: true` — the callee never mutates, stores or returns its
  receiver, nor returns an Enumerator over it, in any argument form. A
  `Literal` object's copy is then indistinguishable from the template to
  the callee, so the call is compiled as usual with the receiver slot bound
  to the template (`LinkMode::C`). Arguments and a block literal are fine:
  only the receiver's identity is at stake. The receiver slot must be dead
  after the call, so the template never becomes visible to later code.
  `Array#[]` is one of these, so `[:a, :b, :c][i]` indexes the template.

Anything else materializes the object and compiles the call as before.

A `yield [e1, .., en]` (n >= 2, no splat) is a consumer too
(`yield_expanded_for`). Bytecodegen gives such a site an expanded twin,
`CallSiteInfo::yield_expanded`: the call site of `yield e1, .., en`, whose
arguments are the temporaries the Array literal is built from (the yield's
argument slot + 1 ..), with the same result slot. When the block is the
literal one specialized in place and auto-splats a lone Array argument
(`FuncInfo::single_arg_expand`: block style, more than one positional
parameter) and takes no keywords, it binds the elements exactly as it would
bind the Array, so the JIT yields through the twin and the Array is never
built (`Hash#each`'s `yield [key, value]` into `{ |k, v| }`). The object
stays registered until the block's arguments are filled: a frameless
block's redo exit hands the original yield back to the interpreter, which
reads the Array from its slot, so that exit materializes it.

So is a multiple assignment from the object (`consume_virtual_expand`):
`a, b = [x, y]` expands it straight into its targets — a template's
constants, copies of the element slots, `nil` past the end — with the
object gone afterwards. The targets are the temporaries right above the
object, overlapping the element slots, so the copies run far end first
(a `memmove`) when the targets lie above the elements.

A `Mov` from a parameter's object into a temporary (`args.include?(a + 1)`
evaluates its receiver into a temporary before the argument) moves the
object when the parameter is dead afterwards, and otherwise gives the
temporary a second view of it when the first reader of the temporary is
a call that only reads its receiver's elements (`alias_consumed_by_call`;
`consume_virtual_mov`). The two views are never told apart: such a
method neither mutates nor lets out its receiver.

The class-version guard is emitted while the object is still registered,
so a redefinition (`class Array; def max = self; end`) deopts with the
object materialized and the interpreter calls the new method on a fresh
Array.

Adding a consumer is a summary entry. An entry is a promise about every
argument form of the method; `take` qualifies (`take(n)` larger than the
size returns a `dup`), `each` does not (it returns `self`), `index` does
not (blockless, argument-less it returns an Enumerator over `self`). The
Range table (`builtins/range.rs`, `init_recv_escape_ruby`) is registered
on the resolved methods after `builtins/range.rb` is loaded (from
`Executor::new`), since that file defines most of them (`===`,
`include?`, `cover?`, `size`, `sum`, `min`, `max`, …) over the builtins;
a Range is immutable, so every method of its own that returns neither
`self` nor an object holding it (`each`, `step`, `%` do) is transparent.

## 3. When an object is materialized

`settle_virtuals` runs before every instruction. An object not consumed
by the instruction is

- dropped if the instruction does not read it and it is dead after it;
- otherwise materialized if any of these hold:
  - the instruction reads it (a `Mov`, an argument, a `ret`, an index base);
  - the instruction could overwrite an element slot (its stack top reaches
    the elements: bytecode temporaries are only written by pushes), or
    writes a local that is an elided Range's endpoint;
  - the instruction ends the basic block;
  - the object is in a temporary and the instruction can run Ruby code
    or reach a call — only `Mov`, `Literal`, `FrozenLiteral`, `Array`,
    the `InlineCache` word and instructions writing nothing but a plain
    destination are survivable (`virtual_survives`, `defined_slot`).

`settle_virtuals_at_bb_end` does the same after an instruction that ends
its block by falling through.

**No object crosses a block boundary.** Every block ends with its live
objects materialized and its dead ones dropped, so a branch entry never
carries one and joins, loop heads and branch bridges reconcile nothing.
The one entry that does bring objects is a method's start, into its
first block, with the parameters its caller handed over (§6);
`settle_entries` keeps those, and a method whose first block is a loop
head is handed nothing (`caller_slots_ok`). At any other merge
`settle_entries` materializes whatever an entry still holds, on that
entry's bridge, as a safety net; it finds nothing there in practice.

An intra-block dispatch (a class dispatch, a polymorphic call, a generic
binary operator: `declare_merge`) is a merge of its own, and its arms may
emit the `CondBr` that follows it (`CondBrSink`), which makes the
instruction the block's last. So `declare_merge` materializes an elided
object among the operands (no arm takes one over), and, under a sink,
settles every live object of the frame as the block's end would
(`settle_virtuals_before_branch`).

A producer lowered with a `CondBr` as its sink without a dispatch — a
fused comparison, a predicate call — may leave its answer in the
condition flags, and nothing may be emitted between it and the branch.
The block-end materialization the `CondBr` would perform is an
allocation call that clobbers them (issue #1837: `[[1], c ? [7] : [6]]`
branched on whatever the allocator left), so it is done before the
producer instead (`settle_virtuals_for_sink`): every object the
producer does not read is dropped if dead after the branch and
materialized otherwise. `materialize_virtual` asserts (debug) that no
condition is pending.

A parameter holding a handed-over object (§6) follows the same rules but
one: it survives its frame's calls. Its elements are in the caller's
frame (or are a template), which no callee can reach, and the callee gate
guarantees that nothing in the body can observe the parameter slot
behind the compiler's back. It is dropped once nothing can read it any
more (`local_live_after`, §10).

The consequences are what keep the mechanism small:

- **A merge never joins an elided object.**
- **A suspended frame owes only objects whose elements are safe.** A
  temporary's object never lives across a call; a handed-over object's
  elements are in the caller's frame, so the chain-deopt replay stub
  (`gen_chain_replay_stub`) materializes `CallerSlots` (through the
  frame's saved bp or its window offset) and `Literal` entries.
- **Liveness comes from the stack top.** The bytecode's own per-instruction
  stack top says when a temporary's object is dead and when its elements
  can be clobbered; a parameter's reads are counted off the basic-block
  graph (`local_live_after`). No separate escape pass over the bytecode is
  needed.

## 4. Side exits and GC

A side exit while an object is pending performs the allocation in its
write back: `WriteBack::virtual_obj` holds `(dst, VirtualSrc)` and runs
last, after every other slot is homed, so the frame is GC-consistent across
the allocating call (`create_array` over the element slots or the caller's
window, `value_deep_copy` of the template, or `create_range` from the
endpoints). Both
`gen_write_back_for_deopt` (x86-64) and `a64_gen_write_back_for_deopt` do
this, and the chain-deopt conversion of a frame suspended in a call does
the same for its handed-over objects.

At a GC safepoint the object's slot is `V`, so the GC write back stores
`nil` into it; the pinned element slots hold their (boxed, homed) values
and are scanned like any frame slot. A handed-over object's elements are
in the caller's frame, which the caller spilled before the call
(`write_back_range`) and which is on the control-frame chain.

## 5. What is not covered

- **Locals.** `x = [a, b]; … x.max` builds the Array: only a temporary
  (or a parameter handed one, §6) holds an elided object. The stage that
  kept objects in locals was removed in 2026-11 (see the introduction).
- **Block boundaries.** An object is never carried into a merge or a
  loop head (§3), so a parameter's object live past a branch, a guard
  clause or an optional parameter's prologue is built at the end of the
  block.
- **Elements overwritten before the use.** `[a, b].include?(c + 1)`
  evaluates `c + 1` into the first element's slot, so the object is
  materialized there. A local or literal argument (`include?(x)`) is passed
  in its own slot and does not clobber. The same happens to a literal
  followed by another argument: in `size([i, 2], 1)` the `1` is evaluated
  into the literal's first element slot, so only a literal in the last
  positional position (or alone) reaches the callee elided; a literal of
  immediates (`Literal`) has no element slots and is not affected.
- **`Slots` objects and transparent callees.** Without a template there is
  no Array to hand a transparent callee; only the `elements` lowerings
  apply. `[a, b][i]` and `args[i]` build the Array.
- **Return values.** A specialized callee's returned object is built at
  its `ret`, unless the call site discards the result (§8).
- **Hash literals and `**kwrest`.** Both are built as they are written.
- **Range endpoints of unknown class.** `(a..b.size).sum` builds the Range:
  the end is a call's result, not proven Fixnum at the `Range`
  instruction. A Float, Bignum or String endpoint builds it too. The
  endpoint temporary of `(a..f(b)).include?(x)` is clobbered by the
  argument push, as an Array's elements are.

## 6. Into callees: `VirtualSrc::CallerSlots`

A specialized callee (`compile_specialized_func`, `LinkMode::from_caller`)
is compiled for one call site, with the caller's abstract state at the
call in hand. Two kinds of parameter can then start as an elided object:

- the **`*rest` parameter**, over the caller's argument window that the
  callee would bind into a new `Array` — `def f(*args) = args.size`,
  and the `...` of a forwarding trampoline `def f(...) = g(...)`;
- a **positional parameter** whose argument is itself an elided literal in
  the caller — `size([a, b])`, `t([1, 2], x)`.

Both are `VirtualSrc::CallerSlots { src, len, base }`, the caller's slot
window `src..src+len` read through the base the callee's frame knows:

| `SlotBase` | Callee | Element `i` lives at |
|---|---|---|
| `SavedFp` | framed | `[[rbp] - rbp_local(src + i)]`: the caller's frame pointer, saved at `[rbp]` by the callee's prologue |
| `Window(delta)` | frameless (`frameless_call.rs`) | `[r14 + delta - conv(src + i)]`: the callee runs in a window `delta` below the caller's locals, so the caller's slots are at a constant offset from its own LFP |
| `Own` | — | the frame's own slot `src + i`: what a `Slots` object is, used by the shared lowerings only |

A caller's `Literal` object is handed over as it is (the template is
global).

**The hand-over** (`specialized_iseq`): the caller decides
`handover = Some(base)` when the call is specialized on its arguments and
the callee passes the gate below; `from_caller` then returns, next to the
parameter link modes, the objects to register at the callee's entry
(`SlotState::new_method`). The caller stores `nil` into the parameter's
slot in the callee frame instead of the value (`fetch_arg_for_callee`),
skips `create_array` for the rest window, and spills the window
(`write_back_range`) so that the callee's reads and any side exit's
rebuild find the elements in memory. A literal inside the rest window is
materialized first: it is an element there, not a parameter. The caller's
own entry for the object is taken at the end of the fill: the object now
belongs to the callee.

**The callee gate** (`caller_slots_ok`): an iseq that cannot capture its
locals without a block (`possibly_capture_without_block`: no `binding`,
`eval`, `local_variables`, …), has no block literal, `super` or `yield`
(`ISeqInfo::forwarding_no_escape`), and rescues nothing. Then a parameter
whose reads are all behind it is dead for good, and nothing in the body can
observe the slot except through the compiler. The gate reads the callee's
own iseq; a call *it* makes to a capturing method (`binding`, `eval`)
resolves through the inline cache and counts as a read of every object of
the frame in `settle_virtuals` (`captures`), so the parameter is
materialized before the call.

**Lifetime in the callee.** A parameter's object follows §3 with one
difference: it survives the frame's own calls, since its elements are not
in this frame. It is materialized when read (a `Mov`, an argument to a
non-specialized call, a `ret`, an index base), at the end of its basic
block while still live, or when read by an instruction that cannot
consume it; dropped when `local_live_after` says nothing reads it any
more. It is *not* handed over again: a `CallerSlots` object passed to a
further specialized callee is materialized, except for the forwarding
consume below.

**The forwarding consume.** The rest of `def f(...) = g(...)` is consumed
by the `g(*rest, **kwrest, &blk)` call itself: `set_arguments`' forwarding
arm reads the object (`SetArgumentsForwarded::deferred_src`) and copies the
caller's window straight into `g`'s frame, or builds `g`'s own `*rest`
from the window's tail (`forwarded_deferred_layout`), whenever `g`'s bind
is a compile-time constant. The rest slot stays elided through the call
(`arg_forwarding_jit.md` §3.4). The forwarding consume works on a generic
call too — it is the only consume of a `CallerSlots` object that does not
need the callee specialized — so `settle_virtual_args` leaves the trailing
`...` splat to it.

**Reads that consume.** `ElementsOp` lowerings over a `CallerSlots` object
read the caller's window: `ArrayMinMax { base, args, len }` for `min` /
`max`, `LoadCallerSlot { base, slot }` for `first` / `last`, constants for
`size` / `empty?`. Materialization is `CreateArrayFrom { base, src, len }`
(`runtime::create_array` over the window); the same addressing is used by
the side-exit write-back, by the chain-deopt replay stub of a frame
suspended in a call (through `[rsi]` / `[x24]` for the saved bp, `rdx` /
`x25` for the window), and by the frameless callee's exit write-back.

**What it buys.** `def f(*args)`-style helpers (`args.size`, `args.first`,
`args.last`, `args.max`) and `...` trampolines (`Class#new`'s forward to
`initialize` included) run without the rest `Array`; literal arguments into
`def size(a) = a.size`-style callees without the literal. The mechanism
replaced the former deferred-rest annotation (D1): the kw half (K1) keeps
its annotation (`DeferredForward { kw }`), the rest half is this.

## 7. Range literals: `VirtualSrc::Range`

`(lo..hi).include?(v)`, `(0...n).sum`, `case v when lo..hi` — the
`Range` instruction builds a Range from two slots (`gen_range`), which
compares the endpoints with `<=>` (a dispatch, which may raise) unless
one is `nil`. The object is elided only where that compare is known to be
free: each endpoint is a compile-time Fixnum or `nil` (`RangeEnd::Const`),
or a slot the abstract state has proven Fixnum (`RangeEnd::Slot`, after
`write_back_slots`, so the frame holds it). The destination must be a
temporary, as for an Array literal.

An endpoint slot is pinned like an Array's element slots, with one twist:
the Range is usually built *into* its first endpoint's slot (the
bytecode evaluates `lo` into the temporary the Range then occupies), and
that slot is never written while the object is elided — `V` slots are not
written — so it is excluded from the overwrite check. An endpoint that is
a local (`a..b` with locals, the common shape) is not pinned; the object is
materialized before any survivable instruction that writes the local
(`written_slot`).

Materialization is `CreateRange` (`runtime::create_range`: `Value::range`
with no compare) in the body, in an argument fill, and at a side exit
(`gen_virtual_materialize` / `a64_gen_virtual_materialize` load each
endpoint as a constant or from its slot). A Range is never handed into a
callee (its endpoints are this frame's slots) and lives only in a
temporary, so no suspended frame owes one and the chain replay asserts the
kind away.

A `Range` literal with constant endpoints (`1..5`) is a template
(`VirtualSrc::Literal`), as an Array of immediates is, and its
element ops are the same lowerings over `Const` endpoints: `(1..3).sum` is
folded at compile time, `when 1..5` is the inline compare against two
immediates. A Range is always frozen, so the bytecode loads such a literal
with `FrozenLiteral` (no per-execution copy), and the `FrozenLiteral`
lowering routes a Range through `try_elide_literal` like a `Literal`;
materializing the template — in the body, handed into a callee, at a side
exit — yields the literal object itself (`value_deep_copy` returns a value
of an always-frozen class unchanged), as the instruction would have. The fold and the inline compare stand in for `Integer#<=>`
(and `Float#<=>`), so they are taken under the basic-op license
(`basic_op_assumable`, `record_bop_dep`) and the unit is evicted by a
redefinition; the helpers re-check `cmp_redefined` at run time and
dispatch the real method then.

## 8. A discarded result

`def set(x, y) = @p = [x, y]` called as a statement, a setter body's
last expression, `pair(x, y)` whose value nobody reads: a specialized
callee returns an object it never let out to a call site with no
destination. The `Ret` of a specialized callee is one edge of the
caller's continuation merge (`record_return_edge`,
`build_return_segments`); when the call site discards the result
(`return_discarded`), `may_consume_virtual`'s `Ret` arm lets the object
through, the edge keeps it, and its segment drops it: it loads `nil`
into the return register (which must carry a non-zero value, zero being
the error return) and the allocation never happens.

**The gate** (`return_discarded_of`): the callee is a method (no
`outer`; a block's return resumes a `yield`), has no exception handler
and never captures its frame without a block
(`possibly_capture_without_block`); the call site passes no block and
has no destination. Any other `Ret` reads the object, so it is built
before the `ret`, as for any escape.

## 9. The def-use and liveness tables (`jitgen/slot_analysis.rs`)

The questions §3 and §10 ask of the bytecode — is this parameter read
again after this instruction, may a block of the frame touch it, is the
next instruction the one call a temporary's view may be consumed by —
are answered from two tables rather than forward scans that re-decode
the bytecode on every call.

**Layer A, `SlotAnalysis`** — what the bytecode alone decides. Per
instruction, the slots it reads (`TraceIr::read_slots`) and the one it
defines without reading (`TraceIr::defined_slot`; an instruction whose
destination is not plain is neither a read nor a kill). Per basic block,
the use and def sets. And what the frame's
block literals (and blocks nested in them) do to its locals: the slots
they reach through the outer chain (`LoadDynVar` / `StoreDynVar` at the
block's depth), whether one of them is opaque (a handler, whose
instructions the JIT cannot decode, or a lambda literal, whose body is
not reachable from here), and the positions of the calls inside them.
It is built once per iseq, the first time a compilation needs it, and
kept in `ISeqInfo::slot_analysis` for good: the bytecode is never
rewritten after it is installed (the inline caches patch operand words,
never the shape of an instruction), so nothing invalidates it. An iseq
with an exception handler is marked `all_live` (a handler block could
read anything); its tables are still built, with the handler-only
instructions (the rescue-clause matches, which the JIT never visits and
`TraceIr::from_pc` refuses) as empty summaries, for the questions that
stay inside a basic block. The tables are kept small, since they live
as long as the iseq: a 16-byte summary per instruction, the reads of
every instruction in one shared vector, the per-block sets in one bit
matrix.

**Layer B, `UnitLiveness`** — what the inline caches decide, which may
differ from one compilation to the next. Whether a call is a capture
point (`capture_point`: `Frame` for a callee that may `binding` / `eval`
or a call the compiler cannot name, `Block` for a call passed a block
literal) is a `MethodCache` verdict, and so is whether every call inside
the frame's blocks is resolved to a callee that cannot capture its
caller. Per compilation unit and per iseq, Layer B reads those verdicts
once, forms the set of locals the blocks may touch (Layer A's set, or
every slot when a block is opaque or holds an unresolved call), adds a
capture point's reads (every slot for `Frame`, the touched set for
`Block`) to the use sets, and runs a backward liveness dataflow over
`BasicBlockInfo::succ` for the blocks' live-out sets. The compilation
sees one state of the caches throughout (it runs no Ruby code), so the
table is built when first asked for and shared with the analysis clones
(`JitContext::unit_liveness`). Its query, `table_observable_after`,
serves the loop-entry seeding in `merge.rs`.

The life of a parameter's object is the *frame liveness* of §10
(`table_frame_live_after`), which depends on no inline cache and so
lives in Layer A. The one instruction a temporary's view may be
consumed by is `SlotAnalysis::alias_stop`; the call-site and
receiver-class conditions of `alias_consumed_by_call` are checked on it.

## 10. How long a parameter's object lives: the frame liveness

A parameter's elided object has to exist whenever something could
observe it, and what can observe a local is not only the compiled code.
A side exit hands the frame to the interpreter, which honours none of
the compilation's inline caches: the call the compiler resolved to a
callee that captures nothing is, there, a call to whatever the receiver
answers — `binding` or `eval` under another name included (issue #1910):

```ruby
class A; def get = nil; end
class B; alias_method :get, :binding; public :get; end

def f(o, *x)
  s = x.first + x.last       # the last read the compiled code can see
  b = o.get                  # an `A` so far: resolved, captures nothing
  [s, b && b.local_variable_get(:x)]
end
```

Compiled for `A`, the call is not a capture point, and a liveness that
counts only what the compiled code reads says `x` is dead after
`x.last`. Dropping the object there is wrong: called with a `B`, the
class guard of `o.get` exits to the interpreter with `x` written back as
`nil`, and the Binding the interpreter makes shows `nil` where CRuby
shows the rest Array. The compiled code never calls a capturing callee
itself (`capture_point` builds every object first and the call is left
to the interpreter), so the capture is always behind a side exit, where
the write-back can still build an object that is tracked — but not one
that was dropped.

**The frame liveness** (`SlotAnalysis::frame_live_out`,
`table_frame_live_after`) is therefore liveness as the interpreter sees
the frame: every instruction that may send a message
(`TraceIr::may_dispatch`) reads every local. That is every method call
and `yield`, the operators and index instructions (`alias_method :[],
:eval` reaches `eval` from `o["x"]`), and whatever else runs a method of
the program: string interpolation (`to_s`), a splat (`to_a`), a multiple
assignment (`to_ary`), a Hash or Range literal (`hash`, `<=>`), a
constant load (`const_missing`), a definition (`method_added`). What
does not: control flow, moves, literals of immediates and Arrays, and
the loads and stores of locals, instance and class variables. The table
is exhaustive over `TraceIr`, so a new instruction has to say which it
is. It depends on no inline cache — that is the point — and so it is a
Layer A fact (§9): one bit per instruction and one more live-out
matrix, built the first time a compilation of the iseq asks. A frame
with a lambda literal answers "live" throughout, as one that rescues
does.

`local_live_after` is this liveness, and it is the only question
`settle_virtuals` asks about a parameter's object: the object lives
until nothing can read the parameter before it is written again, with
every sending instruction counted as a read.

**A read that builds nothing.** A sending instruction is not an escape
point. The object stays elided across it, as it stays across the
frame's calls (§3), and the side exits of the instruction build it. What
changes is only when the object stops being tracked:

- an object nothing can observe *after* a sending instruction is kept
  *through* it (`settle_virtuals`, `kept`) and dropped by the next
  instruction, or by the block's end (`settle_virtuals_at_bb_end`). So
  the last call of a method keeps the object for its own side exit and
  costs nothing;
- a sending instruction that redefines the parameter (`x = f(y)`) keeps
  the old object until its own definition of the slot removes it;
- a comparison or call whose answer goes straight to a branch
  (`CondBrSink`) settles the block's end ahead of itself (§3). A dead
  parameter's object is left alone there (`DeadLocal::Keep`) and
  dropped where the branch is settled, which emits nothing; in front of
  a dispatch that branches in every arm it is built (`DeadLocal::Build`),
  since no arm takes an elided object over.

**Capture points.** A call that could read the frame's locals behind
the compiler's back (`capture_point`) builds the objects first: one
resolved to a callee that captures its caller's frame (`binding`,
`eval`, `local_variables`) or to nothing the compiler can name (a
generic `send`) builds every object; one passing a block literal builds
every parameter's object, a dead one included. The block's body reads
the frame's locals through the outer chain, a `binding` or `eval` it
reaches sees the whole frame, and so does the callee, by asking the
block it was handed for its binding, whatever the body holds (issue
#2058). (The callee gate of §6 already keeps block literals out of a
body that is handed objects, so this is a safety net there.)

**What it does not cover.** The frame liveness makes the object survive
until a *side exit* can build it. A capture that reaches the frame with
no side exit in between still finds the object elided: `binding` or
`eval` reached *in place* from compiled code, through a native method
that calls back (`send(:binding)`, `[o].include?("x")` with `==`
aliased to `eval`), through an operator lowered to a generic helper, or
through `yield` to `&:name` (issue #2069). Here the object is live and
tracked; it is the capture that would have to convert the frame.
