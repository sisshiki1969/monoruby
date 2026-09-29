# Class proofs and singleton classes

The JIT's abstract state proves things about the classes of the values it
holds: "the object in slot `a` is an `Array`", "`self` is a `Foo`", "the
constant `FOO` is a `Foo`". Compiled code leans on those proofs without
checking them again — a call site resolves against the proved class with no
receiver guard, `respond_to?` folds to a constant, `a[0]` becomes an inline
`Array#[]`.

A heap object's class is not fixed. Giving it a singleton class — `def
obj.m`, `obj.extend(M)`, `obj.singleton_class`,
`obj.define_singleton_method`, `obj.instance_eval "def m; end"`, `class <<
obj` — switches its class to a fresh `#<Class:obj>` (in monoruby, literally:
`get_singleton` rewrites the object's class field). Any proof about the
object made before that is now about the wrong class.

This document is about when that can happen under a proof, and what keeps
the compiled code right. See [`jit_invalidation.md`](jit_invalidation.md)
for the version words and salvage machinery it builds on.

---

## 1. Where a proof can go stale

Inside one straight-line stretch of compiled code nothing can change an
object's class: no Ruby runs. So the exposure is exactly:

1. **A proof carried across Ruby code** — a call, a `yield`, a generic
   operator, a `send` — made before it and used after it. Anything the
   callee reaches (any object passed to it, any object reachable from
   one, `self`) may come back with a singleton class.
2. **A proof about a folded constant.** `LoadConst` puts the object in
   the state as `LinkMode::C(v)`, and its class is read at *compile*
   time. It can go stale between compilation and a later run, with no
   call in the unit at all.
3. **An in-unit singleton definition** (`SingletonMethodDef`,
   `SingletonClassDef`), which already dropped the proofs of every slot
   in every frame of the tower (`forget_heap_object_classes`).

The class-version machinery does not cover 1 and 2. A singleton
definition poisons the units that resolved *its name*; salvage then
re-asks each recorded `(class, name)` — and for the old class the answer
has not changed, so the code is re-stamped. And many uses of a proof never
reach a version guard at all: a folded `respond_to?`, an inlined
`Array#[]` / `String#+` are guarded by the basic-op licence only.

## 2. The latch

`ClassInfo::instance_singleton` is a one-way flag per class: *an instance
of this class acquired a singleton class while compiled code relied on it
not happening*. Every way of attaching a singleton class goes through one
place, the branch of `ClassInfoTable::get_singleton` that mints a new one
for an object (`clone`'s copy included); it queues the object's old class,
and `Store::note_instance_singletons` (§4) settles the queue. Not queued:

- **a frozen object** — it cannot gain a method (`def` and `extend`
  raise), so it still behaves as an instance of its old class;
- **a module** — its class is not what the JIT reasons about, and it gets
  a singleton class up front anyway.

The flag is set only when the acquisition actually invalidates a unit.
Until some unit relies on a class, an instance leaving it breaks nothing
— and latching unconditionally would latch `Object`, `Array` and `Hash`
at boot, when `main`, `$LOAD_PATH` and `ENV` get their singleton classes,
which cost `nqueens` 7% (its arrays' proofs stopped being carried across
the `Array.new` calls).

`Store::class_proof_may_break` names the classes the scheme is about at
all: not the immediates, not `Range` / `Complex` (they cannot have a
singleton class), not `Class` / `Module` instances, and not a class that
is itself a singleton class (its only instance already has it).

## 3. Compile time: settling the proofs

Every place that lets Ruby run unsets the frame's class-version guard. It
now also marks the frame's class proofs *unsettled* (an `Invariants` bit,
joined by OR). At the head of the next instruction —
`JitContext::settle_class_proofs`, called from `compile_instruction` —
the state is walked (every frame of the specialization chain) and each
proof about an object whose class may break is sorted:

- **Class latched** (some instance already has a singleton class): the
  proof is dropped — `S(Class(C))` becomes `S(Value)`, and a later use
  guards the class again at run time. A latched constant in the innermost
  frame is written to its slot and becomes `S(Value)` too. `self` cannot
  be dropped (the unit is keyed by it), so it is re-checked instead: a
  class guard on `self` that deopts if it has changed.
- **Not latched**: the proof is kept. Its class goes into the unit's
  `singleton_deps`, and a class-version guard is emitted right there,
  before any use.

The guard is the one the next call site would have emitted anyway, moved
forward to the first instruction after the call; what it newly covers is
the guard-free uses in between. Its deopt resumes at that instruction —
the call is done. (Not on a call's trailing `InlineCache` word, where the
VM cannot resume, and not on a `ret`, which uses no proof: the caller
settles what it gets back.)

Only the innermost frame's `self` is settled: an outer frame is suspended
in a call and settles its own when that call returns. The same holds for
an outer frame's latched constant.

`LoadConst` treats its fold the same way
(`JitContext::settle_constant_class`): a constant object whose class may
break is recorded in `singleton_deps` and version-guarded, or, if its
class has latched, loaded into its slot as `S(Value)` instead of folded.

## 4. Run time: invalidation

`Store` keeps the class-first index `jit_singleton_deps` (class → units),
the counterpart of the name-first `jit_method_deps`, filled when a unit's
salvage record is installed and emptied with it. When the
queue is settled — `Store::get_singleton` shadows the `ClassInfoTable`
method, so every `store.get_singleton` passes through it — a class with an
entry is latched and every unit in the entry is poisoned: their version
words go to the sentinel.

- A frame of such a unit that is **suspended in the call that attached the
  singleton class** returns to the settle's version guard, which fails.
- Salvage (`salvage_method_unit` / `salvage_loop_unit`, and through the
  owner the specialized children) now also checks
  `Store::singleton_deps_hold`: with the latch set it refuses, so the frame
  deopts and the unit is recompiled — this time with the class latched, so
  the proof is not carried.
- A unit **not on the stack** fails its next guard the same way and is
  recompiled before it can use the proof.

A latch flips once per class, so each unit pays at most one recompile per
class it depended on, however many instances acquire singleton classes
later.

## 5. What is not covered

- **Implicit calls** — `to_s` in string interpolation, `to_ary` in
  multiple assignment, `hash` / `eql?` in a hash lookup, and inline
  builtins other than `send` / `Method#call` that call back into Ruby —
  do not unset the class-version guard, so they are not settled either.
  The same holes exist for method (re)definition; this scheme has exactly
  the coverage the class-version guard has.
- **Class and module objects.** A class's metaclass is created lazily
  (`get_metaclass`), which changes the class object's class too. Class
  objects are left out here (`class_proof_may_break`).
- **`IO#reopen`** changes its receiver's class to another real class
  outside `get_singleton`.
