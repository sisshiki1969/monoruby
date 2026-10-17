# Escape analysis: eliding Array, Hash and Range literals, `*rest` and `**kwrest`

`[a, b].max`, `[:get, :head].include?(verb)`, `[x, y].first`,
`{a: 1, b: 2}[k]`, `(lo..hi).include?(v)`, `case v when 1..5`: an Array,
Hash or Range literal that is built only to be the receiver of one call,
and is dead right after it. The JIT does not
allocate such a literal when it is compiled. It records it in the abstract
state as an **elided object** and decides at its use whether the object is
needed at all. The same object can be handed, still elided, into a
specialized callee's parameter — as the literal argument of `size([a, b])`,
as the `*rest` parameter the callee binds `f(a, b)` into (§6), or as the
`**opts` parameter the callee binds `f(x: 1, y: 2)` into (§7).

This replaced `try_fuse_array_minmax`, a peephole that recognized exactly
`array` + `min`/`max` on the next instruction. The pattern-specific part is
now data: what a callee may be handed instead of a real Array is its
**receiver escape summary**, declared next to the builtin.

Source: `codegen/jitgen/compile/escape.rs`; the state half in
`state/slot.rs` (`SlotState::virtuals`, `LinkMode::from_caller`) and
`state.rs`; the side-exit half in `WriteBack::virtual_obj`
(`codegen/jitgen.rs`, and `a64_gen_virtual_materialize` on aarch64); the
summaries in `builtins/array.rs`, `builtins/hash.rs` and
`builtins/range.rs` (`init_recv_escape`, `init_recv_escape_ruby`) and
`executor/inline.rs` (`RecvEscape`, `ElementsOp`); the Range runtime
helpers in `codegen/runtime.rs` (`create_range`, `range_cover`,
`range_fold`).

## 1. The elided object

Three instructions and a method entry can produce one:

| Where | `VirtualSrc` | Condition |
|---|---|---|
| `%d = array[%s..%s+n]` | `Slots { src, len, callid }` | no splat; `%d` and the elements are temporaries, the elements dead once the literal is built (at or above the stack top) |
| `%d = literal[...]` | `Literal(template)` | the template is a plain `Array` whose elements are all immediates, a plain `Hash` whose keys and values are all immediates, or a `Range` whose endpoints are immediates |
| `%d = %s..%e` / `%s...%e` | `Range { start, end, exclude_end }` | `%d` is a temporary; each endpoint is a compile-time Fixnum or `nil` (`RangeEnd::Const`) or a slot proven to hold a Fixnum (`RangeEnd::Slot`), so building the Range later can neither dispatch `<=>` nor fail (§8) |
| a specialized callee's parameter | `CallerSlots { src, len, base }`, or the caller's `Literal` | the caller handed the object over (§6) |
| a specialized callee's `**kwrest` parameter | `CallerKw { callid, base }` | the caller handed its leftover keyword arguments over (§7) |

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
  being read; or it is a parameter nothing reads any more. Nothing ever saw
  it.
- **Materialized**: allocated after all, by exactly the code the literal
  would have run (`NewArray` / `DeepCopyLit`, `CreateArrayFrom` over the
  caller's window, `RestKw` over the caller's keyword slots, or
  `CreateRange` from the endpoints), at the point where something could
  first observe it.

## 2. Consuming uses

A use that may consume the object is a method call with it as the
receiver, and nowhere else in the call's operands
(`may_consume_virtual`); for a Hash also the `%d = %base[%idx]` index
instruction (`consume_virtual_index`), which is `Hash#[]` resolved the same
way; for a Range also the `===` comparison with it on the left
(`consume_virtual_teq`: the `case … when lo..hi` opcode and the explicit
`(lo..hi) === v`), which is `Range#===` resolved the same way. The call resolves the method on the object's class
(`VirtualSrc::class_id`) under the class-version guard and reads the
callee's `RecvEscape`:

- `elements: Some(op)` — an allocation-free lowering over the object's
  elements, for a `Slots` or `CallerSlots` object and an argument-less,
  block-less call. `Min` / `Max` compare the elements in their slots
  (`ArrayMinMax`, over the frame's own slots or the caller's window);
  `Size`, `Empty` are answered at compile time; `First`, `Last` are a copy
  of one element slot (`LoadCallerSlot` for a handed-over object). Over
  the pairs of an elided Hash (a template's, or a `CallerKw` object's
  names and caller slots), `Size` / `Empty` are constants, and
  `HashIndex` (`[]`), `HashFetch` (`fetch`) and `HashHasKey` (`include?`
  and its aliases) are folded when the key is a compile-time packed
  immediate — a symbol or a fixnum, for which `eql?` is bit equality and
  no `hash` is dispatched — and, for `[]` / `fetch` with one argument,
  present (a miss would dispatch `default` or raise). `fetch(k, d)` on a
  miss is `d`'s own slot. A found value is the template's constant or a
  `LoadCallerSlot` of the caller's keyword slot (`plan_hash_op`,
  `emit_hash_fold`). Over the endpoints of an elided Range (`Const` or
  `Slot`), `RangeCover` (`include?`, `member?`, `cover?`, `===`) is two
  tagged compares inline for a Fixnum argument — the tag bit is the same
  on both sides, so Fixnums order as tagged words, and a `nil` endpoint
  bounds nothing — with `runtime::range_cover` behind it for any other
  argument; `RangeSize`, `RangeCount`, `RangeSum`, `Min`, `Max`, `First`,
  `Last` are folded at compile time for constant endpoints
  (`runtime::range_fold`) and go through a helper otherwise; `RangeBegin`,
  `RangeEndpoint`, `RangeExcludeEnd` are the endpoint itself
  (`plan_range_op`, `emit_range_plan`). Where the in-place answer does
  not apply — a non-numeric argument, a `nil` endpoint the method rejects,
  a redefined `Integer#<=>` — the helper builds the Range and dispatches
  the resolved method on it, exactly the call that would have been made.
  A parameter stays elided after such a use if it is still live (the next
  `args.size` or `opts[:y]` is free too); a temporary is done.
- `transparent: true` — the callee never mutates, stores or returns its
  receiver, nor returns an Enumerator over it, in any argument form. A
  `Literal` object's copy is then indistinguishable from the template to
  the callee, so the call is compiled as usual with the receiver slot bound
  to the template (`LinkMode::C`). Arguments and a block literal are fine:
  only the receiver's identity is at stake. The receiver slot must be dead
  after the call, so the template never becomes visible to later code.

Anything else materializes the object and compiles the call as before.

The class-version guard is emitted while the object is still registered,
so a redefinition (`class Array; def max = self; end`) deopts with the
object materialized and the interpreter calls the new method on a fresh
Array.

Adding a consumer is a summary entry. An entry is a promise about every
argument form of the method; `take` qualifies (`take(n)` larger than the
size returns a `dup`), `each` does not (it returns `self`), `index` does
not (blockless, argument-less it returns an Enumerator over `self`). The
Hash table is in two halves: the builtins in `builtins/hash.rs`
(`init_recv_escape`, with `[]`, `fetch`, `include?`, `size`, `empty?` as
elements lowerings and `keys`, `values`, `merge`, `==`, `inspect`, … as
transparent) and the methods `builtins/hash.rb` defines in Ruby (`dig`,
`slice`, `except`, `any?`, `count`, …), registered after that file is
loaded (`init_recv_escape_ruby`, from `Executor::new`). The Range table
(`builtins/range.rs`, `init_recv_escape_ruby`) is registered the same way,
on the resolved methods, since `builtins/range.rb` defines most of them
(`===`, `include?`, `cover?`, `size`, `sum`, `min`, `max`, …) over the
builtins; a Range is immutable, so every method of its own that returns
neither `self` nor an object holding it (`each`, `step`, `%` do) is
transparent.

## 3. When an object is materialized

`settle_virtuals` runs before every instruction. An object not consumed
by the instruction is

- dropped if the instruction does not read it and it is dead after it;
- otherwise materialized if any of these hold:
  - the instruction reads it (a `Mov`, an argument, a `ret`, an index base);
  - the instruction could overwrite an element slot (its stack top reaches
    the elements: bytecode temporaries are only written by pushes), or
    writes a local that is an elided Range's endpoint (`written_slot`);
  - the instruction ends the basic block (a parameter's object may be
    carried into a forward merge instead, below);
  - the instruction can run Ruby code or reach a call — only `Mov`,
    `Literal`, `FrozenLiteral`, `Array` and the `InlineCache` word are
    survivable (`virtual_survives`).

`settle_virtuals_at_bb_end` does the same after an instruction that ends
its block by falling through.

An intra-block dispatch (a class dispatch, a polymorphic call, a generic
binary operator: `declare_merge`) is a merge of its own, and its arms may
emit the `CondBr` that follows it (`CondBrSink`), which makes the
instruction the block's last. So `declare_merge` materializes an elided
object among the operands (no arm takes one over), and, under a sink,
settles every live object of the frame as the block's end would
(`settle_virtuals_before_branch`): a parameter's object would otherwise
reach the branch target through an arm's side-branch state while the
fall-through materializes it, and the join of the two would not keep
both.

A parameter holding a handed-over object (§6) follows the same rules but
two: it survives its frame's calls, and it survives a forward block
boundary. Its elements are in the caller's frame (or are a template),
which no callee can reach, and the callee gate guarantees that nothing in
the body can observe the parameter slot behind the compiler's back. It is
dropped once nothing can read it any more (`local_live_after`: the rest
of its block and every block reachable from there).

**Merges.** A block whose successors are all forward merges — none is a
loop head, and the block is not a dispatch arm (`merge_carries_virtuals`)
— leaves a live parameter's object elided in the state its branch entry
carries. When the merge is compiled (`incoming_context`), the entries are
reconciled first (`settle_entries`): an object every entry brings
identically (same slot, same `VirtualSrc`) stays elided through the
join, and any other is materialized on its entry's bridge, in a per-entry
`AsmIr` the bridge is prepended with (`gen_bridges_for_branches`). The
join itself then only ever meets equal tables (it asserts so, debug), and
the merged state inherits them. This is what lets an optional parameter's
prologue (`def f(a, b = 0) = a.max + b`, `def g(x: 1, **opts) = opts[:y]`),
a guard clause (`return 0 if a.empty?`) or a `?:` before the use keep the
object elided. A loop head is not such a merge: its back edge is a
fixpoint over the body and the body is analysed from a head without
objects, so every entry of a loop — the fall-through into it and its back
edges — materializes before branching (`backedge_branches` asserts none
arrives). A temporary's object never crosses a block boundary: it is
materialized or dropped where its block ends, as before.

The consequences are what keep the mechanism small:

- **A merge only ever sees equal elided objects.** A temporary's object
  never reaches one, and a parameter's reaches it on every path or on
  none, so joins and the branch bridges reconcile nothing; loop-head
  fixpoints see no object at all.
- **A suspended frame owes only objects whose elements are safe.** A
  temporary's object never lives across a call; a handed-over object's
  elements are in the caller's frame, and a local's (§10) in the frame's
  own scratch slots, so the chain-deopt replay stub
  (`gen_chain_replay_stub`) materializes `CallerSlots` (through the
  frame's saved bp or its window offset), `Literal`, and `Slots` /
  `Range` entries (through the frame's lfp).
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

## 5. What is not covered (yet)

- **Locals** hold an object under the rules of §10: not in a body that
  rescues or runs frameless, not while the frame may be captured, not
  through a loop head, and an alias (`y = x` with `x` read again) builds
  it.
- **Elements overwritten before the use.** `[a, b].include?(c + 1)`
  evaluates `c + 1` into the first element's slot, so the object is
  materialized there. A local or literal argument (`include?(x)`) is passed
  in its own slot and does not clobber. The same happens to a literal
  followed by another argument: in `size([i, 2], 1)` the `1` is evaluated
  into the literal's first element slot, so only a literal in the last
  positional position (or alone) reaches the callee elided; a literal of
  immediates (`Literal`) has no element slots and is not affected.
- **`Slots` objects and transparent callees.** Without a template there is
  no Array to hand a transparent callee; only the `elements` lowerings apply.
- **Loops.** A handed-over object is materialized before a loop head
  (`def f(*a); 3.times { }; a.max; end`), even when the loop body cannot
  touch it; carrying one through a loop's fixpoint would need the body
  analysed with it in place.
- **Return values that escape the callee's frame first.** A plain `return`
  of an elided object from a specialized callee hands it over elided (§9);
  a `return` from inside a block, from a callee with a handler, with a
  block or a binding in flight, or of a `**kwrest`, builds it at the `ret`.
- **Hash lookups by a non-constant key, or by a missing one.** `h[k]`
  with `k` a variable needs `k.hash` and `eql?`, and `h[:zz]` on a
  template without `:zz` needs `default`; both run on the template (a
  `Literal`) or materialize (a `CallerKw`). A Hash literal with a dynamic
  value (`{a: x}`) is not elided at all: its value is evaluated into the
  slot a later literal key argument lands on, so the lookup would
  materialize it anyway.
- **Range endpoints of unknown class.** `(a..b.size).sum` builds the Range:
  the end is a call's result, not proven Fixnum at the `Range`
  instruction. A Float, Bignum or String endpoint builds it too, and so does
  a Range into a local. The endpoint temporary of `(a..f(b)).include?(x)`
  is clobbered by the argument push, as an Array's elements are.
- **Other classes.** Objects whose `initialize` the JIT inlines are the
  natural next `VirtualSrc` kind.

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

## 7. Hash literals and `**kwrest`: `VirtualSrc::CallerKw`

A Hash is an elided object in two forms.

**A literal of immediates** (`{a: 1, b: 2}`, `{200 => :ok}`) is a
`Literal(template)` like an Array's, and ends the same ways: consumed by a
folded lookup (`[]`, `fetch`, `include?`, `size`, `empty?` with a constant
key, §2), handed to a transparent callee as the template, dropped, or
copied by `DeepCopyLit`. A specialized callee receives it in a parameter
as it is. There is no `Slots` form: a Hash literal with a non-immediate
value is built as before (§5).

**A `**kwrest` parameter.** The kwrest of a specialized callee —
`def f(a, **opts)` called as `f(x, y: 1, z: 2)` — would be a fresh Hash of
the caller's keyword arguments that the callee's own parameters do not
name. Instead it starts as `VirtualSrc::CallerKw { callid, base }`: the
caller's call site, and the base through which the caller's frame is
reached (`SavedFp` only; a frameless callee is never specialized with a
kwrest). Its pairs are a compile-time table, `AbstractFrame::caller_kw`
(`JitContext::caller_kw_table`): the call site's keyword names, minus the
callee's `kw_names`, each with the caller slot its value was evaluated
into (`kw_pos + offset`). Keys are symbols, so every lookup by a constant
symbol is folded to a `LoadCallerSlot` of that slot, or to a miss.

**The hand-over** (`specialized_iseq`, `set_arguments`'s kwrest fill): the
caller elides the kwrest when the call is specialized on its arguments,
the callee's body passes §6's gate, the call site has no `**splat`, the
callee is not a forwarding trampoline (its kw half keeps `DeferredForward
{ kw }`), and the bind is a simple call. Then the caller spills the
leftover keyword slots (`use_as_value` + `write_back`) so the callee and
any side exit find them in memory, stores `nil` into the kwrest slot of
the callee frame, and `from_caller` registers the `CallerKw` object with
the parameter in `LinkMode::V`. The callee skips its `CheckKwRest`
(nothing to check: the object has no duplicate keys by construction — a
duplicate at the call site is already resolved by the caller's layout).
A call site to a `**kwrest` method is now a reason to specialize at all
(`specializable`), as a `*rest` method is.

**Materialization** is `AsmInst::RestKw { rest_kw, table, base }`
(`AsmIr::kw_rest_from`), the same `correct_rest_kw` runtime call the
callee's prologue uses, pointed at the caller's frame through `base`
(`emit_rest_kw` on both arches: `[rbp]` / `[x29]` minus
`RBP_LOCAL_FRAME` for `SavedFp`). A side exit while the object is pending
writes it back as `WriteBack::forward_kwrest` (`wb_forward_kwrest`), the
entry that already rebuilt a deferred forwarding kwrest, so the deopt
exits and the chain-deopt stub need no new case; `WriteBack::virtual_obj`
never carries a `CallerKw`.

**What it buys.** `def f(**opts)`-style helpers that only read their
options (`opts[:x]`, `opts.fetch(:x, 0)`, `opts.key?(:x)`, `opts.empty?`,
`opts.size`) run without the kwrest Hash; `{a: 1, b: 2}[k]`-style literal
tables and `{...}.include?(k)` without the literal's copy. An optional
keyword parameter next to the kwrest (`def f(x: 1, **opts)`) still
allocates: its prologue branch ends the basic block before the use (§5).

## 8. Range literals: `VirtualSrc::Range`

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
(`VirtualSrc::Literal`), as an Array or Hash of immediates is, and its
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


## 9. Return values: an elided object handed back to the call site

`def pair(a, b) = [a, b]` followed by `pair(x, y).max`, `a, b = pair(x, y)`,
`def rest(*a) = a` / `rest(x, y).min`, `def rng(a, b) = (a..b)` /
`rng(x, x + 10).include?(5)`: the callee's last act is to return an
object it never let out. The `Ret` of a specialized callee is one edge of
the caller's continuation merge (`record_return_edge`,
`build_return_segments`); an elided object still in the returned slot
there is left elided on the edge (`may_consume_virtual`'s `Ret` arm, when
`return_virtual_in_caller` accepts it) instead of being built, and the
decision is made once every return path is known.

**The gate** (`return_virtual_in_caller_of`): the callee is a method (no
`outer`), has no exception handler and never captures its frame without
a block (`possibly_capture_without_block`); the call site passes no
block, has a destination above the caller's locals, and the caller's
frame is proven on the stack (`no_capture_guard`); and the object maps
into the caller: a `Literal` as it is, an Array (`Slots`, or a
`CallerSlots` handed *in*) as `CallerSlots { src: dst, len, base: Own }`,
a `Range` with its slot endpoints relocated to `dst, dst + 1` — all
provided the slots fit in the caller's frame (`dst + n <= total_reg_num`).
A `CallerKw` is not handed back (its table belongs to the callee).

**The decision** (`return_virtual_ok`, in `build_return_segments`): every
edge carries an object, they all map to the same caller-side
`VirtualSrc`, and the callee has no boxed return (a non-local `return`
from a block, a `break`). `JitStackFrame::return_virtual` records it,
next to `float_return`, and travels with the compile result
(`SpecializedCompileResult::return_virtual`, memoized).

**The copies** (`emit_return_virtual_copies`): on each edge, after the
chain bridge, the elements are stored into the caller's slots
`dst..dst+len` through `AsmInst::StoreCallerSlot` (the twin of
`LoadCallerSlot`: `SavedFp` from a framed callee, `Window(delta)` from a
frameless one — `DynVarOffset::Hint` chain addressing is not usable from
a frameless callee, whose frame totals include the window). A `Slots`
object's elements are loaded from the callee's state (a `V` element —
the object's own slot doubling as its first element, see below — from
the stack); a `CallerSlots` handed in from the same caller is moved
slot by slot, far end first where the windows overlap. No value is
loaded into the return register: the caller does not read it. An edge
whose object did not pass (another path returned a bare value) is
materialized on its own segment, as before.

**The call site** (`def_specialized_result` → `def_virtual_return`): the
destination becomes the object's slot (`def_virtual`, `V`), the slots
above it `S` (proven `Fixnum` for a Range's endpoints, `Value`
otherwise). The destination itself is **element 0**: for a self-call the
result slot is the first argument slot and the frame ends right after the
arguments, so there is no room above; the element's value sits on the
stack under a `V` link, which the write-backs no longer `nil`-fill
(`SlotState::wb_void` skips the own slot of an elided object), the
element lowerings read from memory (`copy_element`,
`write_back_elements`), and `discard_dead_elements` leaves alone when the
object is gone, since the slot then holds whatever replaced it. The
caller's `InlineCache` pseudo-instruction after the call writes nothing,
so it no longer counts as overwriting element 1 (`settle_virtuals` asks
for the slot an instruction actually writes).

**Destructuring** (`consume_virtual_expand`): `a, b = f(x, y)` expands
the object straight into its targets — a template's constants, copies of
the element slots, `nil` past the end — with the object gone afterwards.
The targets are the temporaries right above the result, overlapping the
element slots, so the copies run far end first (a `memmove`) when the
targets lie above the elements.

**A discarded result** (`return_discarded`): a call site with no
destination (`pair(x, y)` as a statement, a setter body's last
expression) has nothing to resume with, so the same gate minus the
destination (`handover_site_of`) lets the `Ret` keep the object, and the
segment drops it: it loads `nil` into the return register (which must
carry a non-zero value, zero being the error return) and the allocation
never happens. The decision is per edge — a path returning a bare value
loads it as before, and `return_virtual` stays `None`.

**Not carried**: a `return` reached through a block (`new_method_return`
boxes it), the hand-over of an object the caller then returns again to a
non-specialized frame (materialized at that `Ret`, as any escape: the
unit's callers are unknown code that needs a real `Value` in the return
register), and a Range whose consuming use evaluates an argument into its
slot after the return (`rng(x, x + 10).include?(x + 3)`, the §5
clobbering).

## 10. Locals: an object kept in a local variable, across calls

`x = [a, b]; … x.max`, `r = (lo..hi); … r.include?(v)`, `opts = {a: 1};
… opts[:a]`: the literal is assigned to a local, used later — after
other statements, after calls — and never let out of the frame. The
object is elided into the local and stays elided until something could
observe it; the rules of §3 apply, with a local's lifetime (it survives
the frame's calls, is dropped when nothing reads it any more, and
crosses a forward merge) and the escape points below.

**Scratch windows.** A temporary's elements stay in the temporaries the
literal was built from, which the next push overwrites — so a local's
object cannot keep its elements there across the statements that follow.
The bytecode compiler reserves, for every local an Array literal (no
splat, at most 8 elements) or a Range literal is assigned to, a *scratch
window* as wide as the widest such literal: hidden temporaries above
every temporary the bytecode uses (`BytecodeGen::local_scratch`,
`ISeqInfo::local_scratch`, counted in `temp_num` and so nil-filled at
entry like every register, at most 16 slots per iseq). The interpreter
never reads them. When the JIT elides `x = [a, b]`, it copies the
elements into `x`'s window and homes them there (`copy_to_scratch`); the
object is `Slots { src: window, len }`, and every consuming lowering,
side-exit write back and GC safepoint reads the window exactly as it
would read a temporary's pinned elements. A Range's slot endpoints move
into the window the same way (`RangeEnd::Slot(window + i)`), so a later
write to the local the endpoint came from (`a = f(a)`, which
`written_slot` would not see in a call) cannot change the Range. A
literal of immediates needs no window.

**The local gate** (`local_elision_ok`): the frame is proven on the stack
and uncaptured (`no_capture_guard`) and rescues nothing (a handler could
read any local). A frameless callee (`frameless_call.rs`) passes it: its
window is its whole frame as its own prologue would have laid it out,
scratch windows included, and its body addresses them through the
shifted LFP like any slot of its own; a redo exit hands the whole call
back to the caller, whose state never saw the callee's object, and a
materializing exit builds the object from the window into the frame it
makes (`get_write_back_with_void`: the `nil` fill first, the build
last), as a framed side exit does. So `x = [a, b]; x.size` as the whole
of a tiny method, inlined frameless, allocates nothing, and the object
returned from such a callee is copied out of the window through
`Window(delta)` (§9). Once a capture drops the invariant, no further object
goes into a local until a capture guard re-proves it.

**Escape points.** `settle_virtuals` materializes a local's object
before

- any instruction that reads the local without consuming it: a store
  into an ivar, gvar, cvar or constant, an index store, an argument to a
  call that is not specialized with the object handed over, a `ret` of
  the root unit, a `Mov` that aliases it (`y = x` with `x` read again);
- a call that could read the frame's locals behind the compiler's back
  (`capture_point`): one resolved to a callee that captures its caller's
  frame (`binding`, `eval`, `local_variables`) or resolved to nothing
  the compiler can name (a generic `send`, which could be any of these)
  materializes every object; one passing a block literal materializes
  the objects of the locals the block, or a block nested in it, touches
  (`touched_by_blocks`: a `LoadDynVar` / `StoreDynVar` of the local at
  the block's depth — a store replaces the object behind the compiler's
  back — a lambda literal, or a capturing or unresolved call inside the
  block, which could reach the whole frame). The same check covers a
  parameter's handed-over object;
- a block boundary into a loop head, as any object (a forward merge
  carries it, §3).

Everything else — the frame's own calls to known callees, operators,
instance-variable loads, loads of other locals — leaves the object in
place: its elements are in the window, which nothing but the next
literal into the same local writes. That write is the one way a window
is refilled, and an object that moved out of the local into a temporary
(`t = x` with `x` dead) still has its elements there, so
`settle_virtuals` treats the window of a `Mov` / `Array` / `Range`
destination as written and materializes any *other* object whose
elements lie in it.

**Moves** (`consume_virtual_mov`): `y = x` with `x` dead after the copy
hands the object to `y` instead of building it — into a temporary as it
is, into a local by copying its own-frame elements into `y`'s window
(`mov_target_ok`: the window must be wide enough; a template or a
handed-over object needs none; a `**kwrest` object is tied to its
parameter's table and is built instead). The bytecode compiler gives a
local assigned from another local a window as wide as that local's, so
the move has somewhere to go.

**A temporary's view** (`alias_consumed_by_call`): `x.include?(a + 1)`
evaluates its receiver into a temporary *before* its argument, so the
`Mov` copies a still-live local. When the first reader of that
temporary in the block is a call that only reads the elements of its
receiver (a method with a `RecvEscape::elements` summary, block-less,
with plain arguments) and the instructions between write nothing but a
plain destination, the temporary takes a second view of the same
object — the same `VirtualSrc`, sharing the window — which the call
consumes while the local keeps its own. The two views are never told
apart: such a method neither mutates nor lets out its receiver, so
even a lowering that builds the temporary's copy after all answers the
same. An object in a temporary likewise outlives the instructions that
write a plain destination (`defined_slot`: the operator computing the
argument), not only the `Mov` / literal family; a call is still a
boundary for it. Liveness is kill-aware
(`local_live_after`, `defined_slot`): a write to the local before any
read ends its life on that path, so `x = [a, b]; n = x.max; x = 1` and a
literal rebuilt at the top of a loop body do not keep the previous
object alive. An instruction that redefines the local without reading
it (`x = 1..n`, `x = f(y)` over an elided `x`) drops the object the same
way (`settle_virtuals`, `redefined`) instead of building it first.

**Across calls.** The local's slot is `V` (the GC write back and any
side exit `nil`-fill or build it), the window is homed, and the frame is
on the chain. A chain deopt that converts the frame while it is
suspended in a call builds the object from the window through the
frame's lfp (`gen_chain_replay_stub`, `a64_gen_chain_replay_stub`:
`Slots` and `Range` entries next to `CallerSlots` and `Literal`); a
side exit in the frame itself does what it did for a temporary. An
in-line build of a window-based `Slots` object uses `CreateArrayFrom`
rather than the call site's `NewArray`, whose cold path reads the
literal's original argument temporaries, long since overwritten. A
specialized callee's `return x` hands the local's object back as §9
describes, its elements copied out of the window.

**Not carried**: through a loop head (the body is analysed from a head
without objects); an alias that keeps both names live beyond a
temporary's view; a Range whose endpoints are not proven Fixnum or
`nil` where it is built (as for a temporary's Range); an object
returned into a local of the caller (`x = pair(a, b)` needs the slots
above the destination, §9); a transparent callee on a literal of
immediates that stays live (`opts.key?(:a)` twice: the first call
builds the Hash); an Array index on a local's object (`x[0]`: only a
Hash literal's `[]` is folded, `consume_virtual_index`).

## 11. The def-use and liveness tables (`jitgen/slot_analysis.rs`)

The questions §3 and §10 ask of the bytecode — is this local read again
after this instruction, may a block of the frame touch it, is the next
instruction the one call a temporary's view may be consumed by — were
answered by forward scans that re-decode the bytecode on every call
(`local_live_after`, `touched_by_blocks`, `alias_consumed_by_call`).
`slot_analysis.rs` answers them from two tables instead.

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
with an exception handler is marked `all_live`, as the scans treat it (a
handler block could read anything); its tables are still built, with
the handler-only instructions (the rescue-clause matches, which the JIT
never visits and `TraceIr::from_pc` refuses) as empty summaries, for
the questions that stay inside a basic block. The tables are kept
small, since they live as long as the iseq: a 16-byte summary per
instruction, the reads of every instruction in one shared vector, the
per-block sets in one bit matrix.

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
`BasicBlockInfo::succ` for the blocks' live-out sets. The
compilation sees one state of the caches throughout (it runs no Ruby
code), so the table is built when first asked for and shared with the
analysis clones (`JitContext::unit_liveness`).

A query is then a scan of the rest of the instruction's basic block
over Layer A's summaries, with Layer B's capture verdicts, and the
block's live-out set past it (`table_live_after`). The one instruction a
temporary's view may be consumed by is `SlotAnalysis::alias_stop`; the
call-site and receiver-class conditions of `alias_consumed_by_call` are
checked on it as before.

`local_live_after`, `touched_by_blocks` and `alias_consumed_by_call`
are table lookups; the forward scans they replaced were checked against
the tables, call for call, on the whole test suite, ruby/spec core and
optcarrot before being removed.
