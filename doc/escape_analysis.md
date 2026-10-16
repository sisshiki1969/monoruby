# Escape analysis: eliding Array and Hash literals, `*rest` and `**kwrest`

`[a, b].max`, `[:get, :head].include?(verb)`, `[x, y].first`,
`{a: 1, b: 2}[k]`: an Array or Hash literal that is built only to be the
receiver of one call, and is dead right after it. The JIT does not
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
summaries in `builtins/array.rs` and `builtins/hash.rs`
(`init_recv_escape`, `init_recv_escape_ruby`) and `executor/inline.rs`
(`RecvEscape`, `ElementsOp`).

## 1. The elided object

Two instructions and a method entry can produce one:

| Where | `VirtualSrc` | Condition |
|---|---|---|
| `%d = array[%s..%s+n]` | `Slots { src, len, callid }` | no splat; `%d` and the elements are temporaries, the elements dead once the literal is built (at or above the stack top) |
| `%d = literal[...]` | `Literal(template)` | the template is a plain `Array` whose elements are all immediates, or a plain `Hash` whose keys and values are all immediates |
| a specialized callee's parameter | `CallerSlots { src, len, base }`, or the caller's `Literal` | the caller handed the object over (§6) |
| a specialized callee's `**kwrest` parameter | `CallerKw { callid, base }` | the caller handed its leftover keyword arguments over (§7) |

The slot `%d` becomes `LinkMode::V` and the object lives only in
`SlotState::virtuals`. A `Slots` object's element slots are *pinned*:
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
  caller's window, or `RestKw` over the caller's keyword slots), at the
  point where something could first observe it.

## 2. Consuming uses

A use that may consume the object is a method call with it as the
receiver, and nowhere else in the call's operands
(`may_consume_virtual`); for a Hash also the `%d = %base[%idx]` index
instruction (`consume_virtual_index`), which is `Hash#[]` resolved the same
way. The call resolves the method on the object's class
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
  `emit_hash_fold`). A parameter stays elided after such a use if it is
  still live (the next `args.size` or `opts[:y]` is free too); a temporary
  is done.
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
loaded (`init_recv_escape_ruby`, from `Executor::new`).

## 3. When an object is materialized

`settle_virtuals` runs before every instruction. An object not consumed
by the instruction is

- dropped if the instruction does not read it and it is dead after it;
- otherwise materialized if any of these hold:
  - the instruction reads it (a `Mov`, an argument, a `ret`, an index base);
  - the instruction could overwrite an element slot (its stack top reaches
    the elements: bytecode temporaries are only written by pushes);
  - the instruction ends the basic block;
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
fall-through materializes it, and the join of the two would drop it. The
join asserts (debug) that the innermost frame carries none.

A parameter holding a handed-over object (§6) follows the same rules but
one: it survives its frame's calls. Its elements are in the caller's frame
(or are a template), which no callee can reach, and the callee gate
guarantees that nothing in the body can observe the parameter slot behind
the compiler's back. It is dropped once nothing can read it any more
(`local_live_after`: the rest of its block and every block reachable from
there).

The consequences are what keep the mechanism small:

- **No merge sees an elided object.** Every block ends with none, so joins,
  loop-head fixpoints and the branch bridges never have to reconcile one.
- **A suspended frame owes only handed-over objects.** An object with
  elements in the frame itself never lives across a call, so the
  chain-deopt replay stub (`gen_chain_replay_stub`) materializes exactly
  `CallerSlots` (through the frame's saved bp or its window offset) and
  `Literal` entries, and asserts the rest.
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
window, or `value_deep_copy` of the template). Both
`gen_write_back_for_deopt` (x86-64) and `a64_gen_write_back_for_deopt` do
this, and the chain-deopt conversion of a frame suspended in a call does
the same for its handed-over objects.

At a GC safepoint the object's slot is `V`, so the GC write back stores
`nil` into it; the pinned element slots hold their (boxed, homed) values
and are scanned like any frame slot. A handed-over object's elements are
in the caller's frame, which the caller spilled before the call
(`write_back_range`) and which is on the control-frame chain.

## 5. What is not covered (yet)

- **Locals.** `x = [a, b]; x.max` is not elided: a local is readable from a
  block or a binding, and lives across calls and blocks. (A parameter
  holding a handed-over object is the exception, under the callee gate of
  §6.)
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
- **Merges.** A handed-over object is materialized when its block ends
  while it is still live, so a callee whose prologue branches before the
  use — an optional parameter (`def f(a, b = 0) = a.max + b`,
  `def g(x: 1, **opts) = opts[:y]`), a guard clause — allocates it after
  all. Letting identical objects survive a join is the natural next step.
- **Return values.** An elided object returned from a callee (`[x, y]`
  destructured by the caller) is materialized at the `ret`.
- **Hash lookups by a non-constant key, or by a missing one.** `h[k]`
  with `k` a variable needs `k.hash` and `eql?`, and `h[:zz]` on a
  template without `:zz` needs `default`; both run on the template (a
  `Literal`) or materialize (a `CallerKw`). A Hash literal with a dynamic
  value (`{a: x}`) is not elided at all: its value is evaluated into the
  slot a later literal key argument lands on, so the lookup would
  materialize it anyway.
- **Other classes.** `Range`, and objects whose `initialize` the JIT
  inlines are the natural next `VirtualSrc` kinds.

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
