# Instance variable type tracking

`src/ivar_ty.rs`. The JIT types an ivar load by what has ever been stored
into that ivar slot of that class, and guards nothing at the load when
that is a single class. This page says what is recorded, where, and what
keeps a compiled load right when the record changes.

## Why

An ivar load is the one place where the JIT knew nothing about a value it
had not computed itself: every use of the value carried a class guard.
On optcarrot about 90% of the type checks executed by compiled code
guarded an ivar-loaded value, and 98% of those values were of a single
class for the whole run.

## The state

One word per (class, `IvarId`) — `IvarTy`:

| tag | meaning |
|---|---|
| `EMPTY` | nothing but (maybe) `nil` stored yet |
| `FIXNUM` | Fixnum (a Bignum goes straight to Top, so `Integer` keeps meaning "Fixnum") |
| `FLOAT` | Float (flonum or heap) |
| `BOOL` | `true` / `false` |
| `CLASS` | an object of exactly one class, its id in bits 32..63 |
| `TOP` | anything |

plus a nil bit. A state only widens: joining a second kind gives Top.

The class is the object's class at the time of the store (`Value::class`),
so the instances of a singleton class have their own states.

## Where stores are observed

Every way a value gets into an ivar slot goes through one of:

- `RValue::set_ivar_by_ivarid` — the interpreter, `instance_variable_set`,
  the generic JIT store, attr writers called out of line, extensions.
  `ivar_ty::observe` decides under a shared borrow that the state does not
  change, which is what nearly every store finds.
- the JIT's inline stores (`store_ivar`, the inlined `attr_writer`):
  `AsmInst::IvarTyCheck` tests the value against the state at compile
  time and calls `jit_ivar_ty_observe` through a stub only on a mismatch.
  A store whose value the abstract state already proves conforming emits
  nothing.
- `RValue::change_class`: an object that changes class re-observes all
  its ivars under the new class.

An unset slot reads as nil but is never stored: a load that finds the
slot empty (`AsmInst::IvarUnset`) records the nil itself before it
deoptimizes.

## Typed loads

`JitContext::ivar_ty_for_load` types a load when the state is one class
(Float excepted: unboxing tests flonum / heap either way, so the type buys
nothing). With the nil bit the load is `NilOr(class)`; without it the
load is the class itself, and an unset slot deoptimizes.

The unit records each state it relied on (`IvarTyDep`). When a store
widens a recorded state, `observe` poisons the class-version word of
every unit that recorded it, so:

- the next class-version guard in a running frame of that unit misses and
  deoptimizes, and
- salvage (`unit_holds`) refuses to restamp the word, so the unit is
  recompiled.

That makes a typed load correct only if a class-version guard has run
since the last point a state could have changed. `Invariants::ivar_ty_guard`
tracks that: it is set by every class-version guard and cleared by a call,
a store check (whose slow path may widen), and a generic ivar store; a
typed load emits a guard when it is clear.

Two places would otherwise re-guard on every loop iteration:

- **The safepoint poll.** It is the one point in a call-free loop body
  where other Ruby code (another thread, a trap handler, a finalizer) can
  run. Instead of clearing the flag there, `execute_gc` compares
  `ivar_ty::poison_epoch` across the poll and returns
  `executor::POLL_DEOPT` when a state some unit relied on changed, so the
  polling frame leaves for the interpreter.
- **The loop head.** A loop unit starts without the guard. When the back
  edge (from the fixpoint) still has it, the guard is checked once on each
  forward entry instead (`JitContext::incoming_context`), and the loop
  head keeps it.

### Classes that escape

A typed load's class is also a proof for dispatch on the value, which an
object changing class (getting a singleton class, `IO#reopen`, Marshal's
`extend`) would break while it sits in an ivar. `note_class_escape`
marks the old class unreliable for typed loads and poisons the units that
relied on it. Only classes some state has recorded count: an object of a
class no state has ever named cannot be in an ivar whose loads are typed
by it, which keeps the startup singletons (`ENV`, `ARGV`, `main`) and
metaclass creation from disabling `Hash`, `Array` and `Object`.

## Stale store checks

A store check compiled against a monomorphic state is still correct after
the state widens; it only takes the slow path more often. So a widening
does not recompile the units whose *stores* checked it (a recompile at a
later point can meet colder, more polymorphic call sites than the code it
replaces). Instead the slow path first reads the live state word
(`ivar_ty::state_word`, a copy of the state at a fixed address) and accepts
the value without the call if the state is now Top, or the value is nil
and the state has the nil bit. Only a check compiled against an empty
state registers a dependency, to be recompiled with a real one.
