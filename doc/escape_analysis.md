# Escape analysis: eliding Array literals used as a receiver

`[a, b].max`, `[:get, :head].include?(verb)`, `[x, y].first`: an Array
literal that is built only to be the receiver of one call, and is dead
right after it. The JIT does not allocate such a literal when it is
compiled. It records it in the abstract state as an **elided object** and
decides at its use whether the Array is needed at all.

This replaced `try_fuse_array_minmax`, a peephole that recognized exactly
`array` + `min`/`max` on the next instruction. The pattern-specific part is
now data: what a callee may be handed instead of a real Array is its
**receiver escape summary**, declared next to the builtin.

Source: `codegen/jitgen/compile/escape.rs`; the state half in
`state/slot.rs` (`SlotState::virtuals`) and `state.rs`; the side-exit half
in `WriteBack::virtual_obj` (`codegen/jitgen.rs`, and
`a64_gen_virtual_materialize` on aarch64); the summaries in
`builtins/array.rs` (`init_recv_escape`) and `executor/inline.rs`
(`RecvEscape`, `ElementsOp`).

## 1. The elided object

Two instructions can produce one:

| Instruction | `VirtualSrc` | Condition |
|---|---|---|
| `%d = array[%s..%s+n]` | `Slots { src, len, callid }` | no splat; `%d` and the elements are temporaries, the elements dead once the literal is built (at or above the stack top) |
| `%d = literal[...]` | `Literal(template)` | the template is a plain `Array` whose elements are all immediates |

The slot `%d` becomes `LinkMode::V` and the object lives only in
`SlotState::virtuals`. A `Slots` object's element slots are *pinned*:
`clear_above_next_sp` leaves them alone although the bytecode considers them
dead, because they are the object's contents. A `Literal` object needs no
storage: its contents are the template's.

The object ends in one of three ways.

- **Consumed** by its use (§2): the Array is never allocated.
- **Dropped**: its slot is overwritten, or falls above the stack top, without
  being read. Nothing ever saw it.
- **Materialized**: allocated after all, by exactly the code the literal
  would have run (`NewArray` / `DeepCopyLit`), at the point where something
  could first observe it.

## 2. Consuming uses

A use that may consume the object is a method call with it as the
receiver, and nowhere else in the call's operands
(`may_consume_virtual`). The call resolves the method on `Array` under the
class-version guard and reads the callee's `RecvEscape`:

- `elements: Some(op)` — an allocation-free lowering over the literal's
  elements, for a `Slots` object and an argument-less, block-less call.
  `Min` / `Max` compare the elements in their slots
  (`runtime::opt_array_min/max`, the former fusion's helper); `Size`,
  `Empty`, `First`, `Last` are answered at compile time (a constant, or a
  copy of one element slot).
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
not (blockless, argument-less it returns an Enumerator over `self`).

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

The consequences are what keep the mechanism small:

- **No merge sees an elided object.** Every block ends with none, so joins,
  loop-head fixpoints and the branch bridges never have to reconcile one.
- **No suspended frame owes one.** An object never lives across a call, so
  the chain-deopt replay stub needs no materialization (it asserts so), and
  no dispatch arm splits a state carrying one.
- **Liveness comes from the stack top.** The bytecode's own per-instruction
  stack top says when the object's slot is dead and when its elements can
  be clobbered; no separate escape pass over the bytecode is needed.

## 4. Side exits and GC

A side exit while an object is pending performs the allocation in its
write back: `WriteBack::virtual_obj` holds `(dst, VirtualSrc)` and runs
last, after every other slot is homed, so the frame is GC-consistent across
the allocating call (`create_array` over the element slots, or
`value_deep_copy` of the template). Both `gen_write_back_for_deopt`
(x86-64) and `a64_gen_write_back_for_deopt` do this.

At a GC safepoint the object's slot is `V`, so the GC write back stores
`nil` into it; the pinned element slots hold their (boxed, homed) values
and are scanned like any frame slot.

## 5. What is not covered (yet)

- **Locals.** `x = [a, b]; x.max` is not elided: a local is readable from a
  block or a binding, and lives across calls and blocks.
- **Elements overwritten before the use.** `[a, b].include?(c + 1)`
  evaluates `c + 1` into the first element's slot, so the object is
  materialized there. A local or literal argument (`include?(x)`) is passed
  in its own slot and does not clobber.
- **`Slots` objects and transparent callees.** Without a template there is
  no Array to hand a transparent callee; only the `elements` lowerings apply.
- **Uses inside callees.** An elided object passed as an argument, or
  returned, is materialized. Following it into a specialized callee is
  the next step, and would subsume the D1 deferred rest array
  (`arg_forwarding_jit.md` §3.4), which is the same idea specialized to
  `...` forwarding.
- **Other classes.** `Hash` literals, `Range`, and objects whose
  `initialize` the JIT inlines are the natural next `VirtualSrc` kinds.
