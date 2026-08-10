# Refinements — the design, and why it is shaped this way

monoruby implements refinements. `Module#refine`, `Module#using` and
`main.using` are real, `Refinement` is a class with `#target` and
`#import_methods`, and `Module#refinements` / `Module.used_refinements` /
`Module.used_modules` report the truth. The interpreter and the JIT both
resolve through an activated refinement.

This document is the design record: §§1–5 are the problem — what
refinements do to method resolution and where each of monoruby's
resolution paths would break under the naive approach — and §§6–7 are the
implementation that avoids it. Read it together with `doc/cref.md` (the
CREF refinements hang off) and `doc/jit.md`.

The short version of the problem: refinements make the resolved method a
function of the **caller's lexical scope**, and every method-resolution
path in monoruby — the global method cache, the VM inline cache, JIT
compile-time resolution, and the class-version repair path — was keyed
without it. The repair path was the dangerous one: left alone it would
silently re-validate machine code that resolved the wrong method.

The short version of the answer (§6): represent an activated set as an
interned `u32`, treat it as a compile-time constant of each body with
`class_version` as the invalidation channel, and gate everything on "does
this process contain a refinement at all". A program that never calls
`refine` emits byte-identical machine code to the one that predates all of
this — checked against an `emit-asm` baseline, not assumed.

**Known gaps.** One refinement cell per iseq means a *block* that runs
`using` and executes more than once bases on the previous execution's set
(`ISeqInfo::refinements`, §7.2 option C). Refinements of *basic operations*
are honoured in both tiers (§6.7); the JIT keeps its inline path in scopes
that do not activate the refinement, while the VM's assembly guard has no
call site to ask about and gives it up process-wide.

---

## 1. What refinements change about lookup

Without refinements, monoruby's method resolution is a function of

```
(receiver class, method name, class version) -> FuncId
```

and every cache in the system is keyed on exactly that. With refinements
it becomes

```
(receiver class, method name, class version, caller's cref) -> FuncId
```

Four properties of the cref dependency matter for the implementation.
All four were checked against CRuby 4.0.2.

**(a) Activation is a runtime event at a lexical position.** `using` is a
statement, not a declaration. The same source text resolves differently
before and after it runs:

```ruby
module R
  refine Integer do
    def +(o) = "refined+"
  end
end

p (1 + 1)     # => 2
using R
p (1 + 1)     # => "refined+"
```

**(b) The activated set is captured per method body, at definition
time.** A method defined before the `using` is not refined even when it is
*called* afterwards:

```ruby
def unrefined = 1 + 1
using R
p unrefined   # => 2   (not "refined+")
```

So this is not a global mode that can be flipped, and it is not a property
of the receiver either.

**(c) Basic operations are refinable.** `Integer#+` above is a monoruby
*basic op* and a JIT inline-generator target — the two paths that never
dispatch at all.

**(d) The reflective entry points see refinements too.** In Ruby 4.0
`send`, `public_send`, `respond_to?`, `Object#method` and blocks / `eval`
written in the activated scope all resolve through the refinement:

```ruby
class C
  using R
  def direct      = "x".foo
  def via_send    = "x".send(:foo)
  def via_respond = "x".respond_to?(:foo)
  def via_method  = "x".method(:foo).call
  def in_block    = [1].map { "x".foo }.first
  def in_eval     = eval('"x".foo')
end
# => ["refined", "refined", true, "refined", "refined", "refined"]
```

There is no "reflection is exempt" shortcut to lean on.

---

## 2. The prerequisite: per-scope cref state

`doc/cref.md` covers this in full; the part that blocked refinements:

```rust
// Executor
lexical_class: Vec<Vec<Cref>>,
```

This is **one VM-wide stack**, whose outer level is a `require` / `load`
boundary, not a call frame. Method calls push nothing. In CRuby the CREF
chain is per-frame, hung off the environment pointer, and `using`
populates the *current* frame's chain.

The consequence has already been observed. `Module.nesting` used to read
this stack and therefore reported whatever class body happened to be on
it — a method called from inside an unrelated class body saw that body's
scope. That was fixed (`current_class_nesting`) by reading the *static*
`ISeqInfo::lexical_context` instead, because the lexical nesting of an
iseq never changes after compilation.

**A refinement set cannot be recovered the same way.** Property (a) above
says it is mutated at runtime; property (b) says the mutation is visible
only to bodies compiled under that cref. A static per-iseq field cannot
express "the set as it stood when this body was defined" unless the field
is snapshotted per definition — which is exactly a per-frame cref by
another name.

This was the first step. It is a change to where scope state is *stored*,
not to how lookup works — and, as §7 works out, it did not have to be a
change to the frame layout: the cell lives on `ISeqInfo`, resolved through
the lexical-parent chain.

---

## 3. Where each resolution layer would have broken

### 3.1 Global method cache

`Store::check_method_for_class_with_version` (`globals/store.rs`)

```rust
let mut cache = self.method_cache.borrow_mut();
if let Some(entry) = cache.get(class_id, name, class_version) { … }
```

Key: `(name, class_id)` + `class_version`. Every consumer in the tree goes
through here or through `search_method_by_class_id` beneath it. Under
refinements this key no longer identifies an answer.

The per-class memoized predicates built on the same key (`no_to_str`,
`neq_basic_at`, `match_method_at`, `default_copy_at` — all `Cell<Option<u32>>`
stamps on `ClassInfo`) inherit the problem: a refinement of `#to_str` on a
class whose `no_to_str` memo says "no such method" would be invisible to
every coercion site.

### 3.2 VM inline cache

`vm_send` (`codegen/arch/{x86_64,aarch64}/vmgen/method_call.rs`) caches
into the bytecode operand words:

```
CACHED_FUNCID  (u32)   CACHED_CLASS (ClassId)   CACHED_VERSION (u32)
```

and guards with two compares:

```asm
cmpl r15, [r13 + CACHED_CLASS]     ; receiver class
jne  slow_path1
movl rdi, [r13 + CACHED_VERSION]
cmpl rdi, [rip + class_version]    ; global class version
jne  slow_path2
```

This cache is *per call site*, so in principle it can hold a cref-dependent
answer — a given call site has exactly one cref. The problem is
invalidation: nothing in these two guards notices that `using` ran. Making
`using` bump the global `class_version` works but is blunt — it invalidates
every inline cache in the program. §6.1 argues that is the right trade
anyway, because `using` is a load-time event, not a hot-path one.

### 3.3 JIT compile-time resolution

`JitContext::jit_check_method` (`codegen/jitgen/compile.rs`)

```rust
fn jit_check_method(&self, class_id: ClassId, name: IdentId) -> Option<(FuncId, Visibility)> {
    let class_version = self.class_version();
    let entry = self.store
        .check_method_for_class_with_version(class_id, name, class_version)?;
    Some((entry.func_id()?, entry.visibility()))
}
```

The JIT resolves to a concrete `FuncId` at compile time and bakes it into
machine code (and, on the specialized path, inlines the callee body). The
inputs are receiver class, name and version — the compiling iseq's cref is
not among them and is not even reachable from `JitContext`.

### 3.4 Class-version repair — the silent-wrong-answer path

This is the one that makes a half-measure unsafe.

Every JIT compilation records what it assumed:

```rust
// JitContext, per compiled method call
self.inline_method_cache.push((recv_class, callsite.name, func_id));
// stored on the iseq as `inline_cache_map`
```

When the global class version moves, compiled code deopts — and
`Store::update_inline_cache` (`globals/store/class.rs`) tries to *repair*
rather than recompile:

```rust
for (recv_class, name, comptime_fid) in cache_map {
    let func_id = self.check_method_for_name(lfp, *recv_class, *name);
    if func_id != Some(*comptime_fid) {
        return false;              // resolution moved -> recompile
    }
}
// nothing moved: stamp the new version into the compiled code and keep running
codegen.set_class_version(class_version, &version_label);
```

`check_method_for_name` is a cref-free lookup. If `using` only bumped the
class version (§3.2's blunt option), this loop would re-ask the *unrefined*
question, get the same unrefined answer it got at compile time, conclude
nothing moved, and re-validate machine code that must now dispatch into the
refinement. No error, no deopt — just the wrong method, indefinitely.

Any refinement implementation has to record the cref alongside each
`inline_cache_map` entry so the repair re-asks the same question the
compiler asked.

### 3.5 Specialized inlining

`JitType::Specialized` inlines a callee's body into the caller's frame, and
`gen_machine_code` recurses through `SpecializeInfo` to do it for a whole
tree of callees. A refined method body carries its *own* cref — calls
inside it resolve under the refinement module's scope, not the caller's.
Inlining across that boundary needs a per-inlined-frame cref threaded
through `JitContext`, in the same place the specialized frame sizes and
argument info are threaded today.

### 3.6 Inlined builtins and basic ops

Two mechanisms bypass dispatch entirely:

- `InlineTable: HashMap<FuncId, InlineFuncInfo>` — an inline generator
  emits machine code for `Integer#+`, `Array#[]`, `Math.sqrt`, … in place
  of a call.
- Basic ops additionally consult a single global word,
  `bop_redefined_flags`; `Codegen::set_bop_redefine` sets it to `!0` and
  calls `remove_vm_bop_optimization()` — a process-wide, one-way
  de-optimisation.

Property (c) says `Integer#+` is refinable, so a refinement must reach both.
Framed as an either/or — thread the cref into every `InlineGen`, or take
the process-wide `set_bop_redefine` cliff — both options are bad. §6.7
splits it instead: the JIT's inline generators gate per call site on a
`refined_names` set, and only the VM's dispatch-table basic ops take the
global hit.

### 3.7 super

`find_super` / `jit_check_super` resolve `super` by finding the *position*
of the running body in the receiver's ancestor chain
(`body_dispatched_by`, occurrence counting) and continuing from the next
one. The chain is a property of the receiver class.

A refinement is, semantically, an entry that exists in the chain only
relative to a cref, and `super` inside a refined method means "the method
this refinement shadows". Neither is expressible in a position-in-the-real-
chain model; `super` from a refinement needs its own resolution rule.

---

## 4. Why the cheap version would have been worse than nothing

A stub `refine` that creates an anonymous module, evaluates the block in
it and returns it — no activation — was prototyped and measured. It
unblocks 55 of the 59 `core/binding` examples (see §5) because the blocker
there is a fixture that merely *calls* `refine` at load time.

It was not kept, and the real thing was implemented instead. Without
activation, `refine` turns every refinement-using
program from a clear `NoMethodError` at the `refine` call into a silently
wrong result at every refined call site. The same trade-off applies to
`using`, which is why the existing `Module#using` is documented as
"activates the (necessarily empty) set of refinements" — it is only honest
because `refine` cannot produce a non-empty one.

---

## 5. What it is worth, in ruby/spec

Measured against CRuby 4.0.2, current tree:

| category | monoruby | note |
|---|---|---|
| `core/refinement` | 25 examples, **25 errors** | nothing implemented |
| `core/module` | 11 F / 66 E, of which **62** mention refine | `refine_spec`, `using_spec`, `module_eval` refinement scope, … |
| `core/main` | 3 F / 6 E, of which **5** | `main.using` |
| `core/kernel` | 4 F / 11 E, of which **2** | `Kernel#eval` refinement scope |
| `language/pattern_matching` | **2** E | `#deconstruct` via refinement |
| `core/binding` | 0 F / **7** E | see below |

`core/binding` is a special case worth separating. Its 7 errors are not 7
failing examples — they are 7 spec *files* that fail to load, because
`core/binding/fixtures/classes.rb:54` calls `refine`. That takes 59
examples out of the run (monoruby executes 39 of CRuby's 98). **Exactly one
of those 59 needs refinements to work** (`Binding#eval reflects refinements
activated in the binding scope`); the other 58 only need `refine` to
exist.

So the ledger is roughly:

- ~96 examples need real refinements,
- ~58 examples are collateral damage from a fixture that only needs the
  method to be defined.

That asymmetry is the argument for eventually implementing refinements
properly rather than stubbing them: the stub buys the 58 at the cost of
making the 96 fail silently instead of loudly.

---

## 6. An implementation strategy that keeps the performance

The naive framing of §3 — "every cache key has to grow a cref" — is what
makes refinements look prohibitive. It is avoidable. The design below rests
on one representation choice and two observations, and its acceptance
criterion is that a program which never calls `refine` executes *the same
machine code it does today*.

### 6.0 The representation: an interned set id

Represent an activated refinement set as an interned `RefinementSetId(u32)`:

```rust
RefinementSetId::EMPTY == 0                  // no refinements activated
using M   on a scope holding S   =>   intern(S ∪ refinements_of(M))
```

Interning is hash-consing over a table of small sets; equal sets get equal
ids. Real programs have a handful of distinct activations, so the table
stays tiny.

This is the move that makes everything else cheap. The cref's refinement
state stops being a `Hash[refined_class => module]` that has to be walked
and starts being **a `u32` that can be compared, stored in a cache entry,
and baked into compiled code as a constant**.

### 6.1 Observation 1 — a call site's set is a compile-time constant

`using` is illegal in a method body (§7.1), so:

- A **method body** snapshots its scope's set at `def`. Every invocation of
  that method resolves under the same id.
- A **block** reads its home scope's live cell — but that cell changes only
  when `using` runs in that scope, and `using` is a load-time event.

So for any iseq, the set is constant except across a `using` in its home
scope. That is precisely the shape a JIT speculates on: **treat the set id
as a compile-time constant, and let `using` bump `class_version`.**

`using` bumping the global class version invalidates every inline cache and
every JIT entry in the program. That sounds violent until you notice it is
exactly what a `def` at load time already does, and `using` happens once
per scope during startup, never in a loop.

### 6.2 Observation 2 — the cost should scale with *refined names*, not with *using refinements at all*

Maintain

```rust
refined_names: HashSet<IdentId>   // union of names any refinement defines
```

This is typically a handful of symbols. Then:

- the global method cache and the per-class memoized predicates
  (`no_to_str`, `neq_basic_at`, …) **keep their existing key**, and simply
  refuse to serve a name in `refined_names`;
- those names — and only those — take
  `search_method_with_refinements(recv_class, name, set_id)`.

A program that refines `String#blank?` pays nothing on `Array#each`,
`Integer#+`, or any of the other tens of thousands of call sites. The tax
is proportional to how much is refined, which is the right shape and is
what keeps a refinement-using program fast, not just a refinement-free one.

### 6.3 Where the mutable cell lives

Only toplevel / class-module bodies / eval-at-toplevel own one (§7.1), and
now it holds a `u32`. §7.2 option **(B)** — a side table on the `Executor`
keyed by LEP, alongside the existing `deferred_unwind` — stays the
recommendation: no frame-layout change, correct per execution, and
allocated only once a refinement exists.

A block finds its own set by the same outer-chain walk to the LEP that
`$~` already does. A method finds its own on its `FuncInfo`, written by
`def`. (`def` re-executing under a *different* set writes a different id;
since `using` already bumped the version, that is self-correcting rather
than stale — unlike `lexical_context`, which has no such guard.)

### 6.4 The zero-cost gate

A process-wide flag, false until the first `refine` call:

| path | flag false | flag true |
|---|---|---|
| `search_method` | today's code | + `refined_names` check |
| global method cache | today's key | today's key; skipped for refined names |
| VM inline cache | today's two guards | unchanged (see 6.5) |
| `jit_check_method` | today's lookup | takes `set_id` |
| `update_inline_cache` | today's loop | + id comparison (see 6.6) |
| inline generators | fire as today | gated per call site (see 6.7) |

The acceptance criterion is stronger than "fast": with the flag false the
emitted machine code must be **identical**, which `--features emit-asm`
makes directly checkable against a baseline. Benchmarks (optcarrot and
`benchmark/`) then cannot regress, by construction rather than by
measurement.

### 6.5 VM inline cache — no format change

The cached triple lives in the bytecode operand words
(`CACHED_FUNCID` / `CACHED_CLASS` / `CACHED_VERSION`) and has no room for a
fourth. It does not need one: a call site has exactly one set id, so the
cached `FuncId` is already the right answer *for that site*. The warm path
(`runtime::find_method`) has `vm`, hence the current frame, hence the set —
it resolves with it and caches the result. `using`'s version bump forces a
re-warm. Nothing in the guard sequence changes.

### 6.6 JIT — closing the repair hole for 4 bytes

`inline_cache_map` entries grow from

```rust
(ClassId, Option<IdentId>, FuncId)
```

to

```rust
(ClassId, Option<IdentId>, RefinementSetId, FuncId)
```

and `update_inline_cache`'s re-check calls the set-aware lookup. That is
the whole fix for §3.4's silent-wrong-answer path: the repair now re-asks
the question the compiler asked. Four bytes per recorded call site, no
runtime cost, and with the gate off every recorded id is `EMPTY` and the
comparison is a constant-folded no-op.

`JitContext` gains the set id of the iseq it is compiling — read from the
`FuncInfo` snapshot for a method, or from the live frame for a block/loop,
both at compile time in Rust. **No emitted prologue changes and no machine
code ever loads a cref.**

### 6.7 Inline generators and basic ops — split the two

§3.6 framed this as a choice between threading the cref into `InlineGen`
and taking the global basic-op cliff. With `refined_names` it is neither:

- **Inline generators** (`Array#[]`, `String#size`, …) are consulted at JIT
  compile time, where the set id is known. Gate them on
  `set_id == EMPTY || !refined_names.contains(name)`. A refinement of
  `Array#[]` costs the fast path **only in scopes that activated it**;
  every other scope keeps it.
- **Basic ops in the VM** (`vm_binops`, the comparison dispatch entries)
  are selected by a dispatch table with no call-site context, and
  `remove_vm_bop_optimization` is a one-way process-wide switch. Refining
  one of those does take the global hit. That is acceptable because it is
  the same hit a *global* monkey patch of `Integer#+` takes today — the
  honest comparison — and because the JIT, which is where the time
  actually goes, keeps per-scope precision via the gate above.

> **Update.** The order this called for was followed —
> `doc/bop_redefinition.md` gave basic ops `(op, class)` granularity, full
> coverage and per-iseq JIT invalidation first — and once that was in
> place, honouring a refinement of a basic operation turned out to need
> almost nothing beyond *marking the pair*. `insert_method` /
> `remove_method` ask `refined_class()` for the class the refinement
> refines and mark `(that class, name)`; both tiers then stop answering it
> without a lookup, and the dispatch they fall back to was already
> refinement-aware. `refine Integer { def +(o) = 42 }` now yields 42 in the
> VM and in JIT-compiled code.
>
> Two things that fix did *not* buy:
>
> - **Per-scope precision in the JIT** — since added. `BasicOpTable` keeps
>   the union it always did ("this pair is no longer unconditionally
>   sound", which is what the runtime guards want) but records the reason
>   alongside it, so `assume_basic_op` can ask two questions: a global
>   redefinition binds everywhere and is never inlined, while a refinement
>   is checked against the *compiling scope's* set
>   (`Store::basic_op_refined_in_scope`). `refine`-ing an operator no
>   longer costs anything outside the scopes that `using` it — `fib(29)`
>   0.034 → 0.009, back to baseline. The VM's asm guard is a global word
>   with no call-site context and stays coarse; correctness there rests on
>   the dispatch it falls through to, which is refinement-aware.
> - **The inline-generator half**, which is still ungated.
>
> One boundary had to be drawn to make any of this correct — see
> `Executor::basic_op_refinements`. monoruby writes part of its core
> library in Ruby where CRuby uses C, and those frames are transparent to
> refinements so that `&obj` / interpolation, which monoruby converts in
> the callee and CRuby in the caller, reach the user's scope. An operator
> is never such a conversion: `Array#map`'s own `i += 1` is library code,
> C in CRuby and invisible to any refinement. Resolving it against the
> caller ended the loop after one iteration. So operator names stop the
> walk at the library boundary and everything else still walks out.

### 6.8 The remaining pieces

- **`super` inside a refinement** means "the method this refinement
  shadows". Record `refined_class: Option<ClassId>` on the refinement
  module and special-case `find_super` when the running body belongs to
  one, instead of trying to express it as a position in the real ancestor
  chain (§3.7).
- **Reflection** (`send`, `respond_to?`, `Object#method`) resolves with the
  *caller's* set: the same `nearest_ruby_frame` walk the eval builtins now
  use, then that frame's set id. Only reached with the gate on.

### 6.9 Order of work — as landed

Each step shipped on its own, with the spec ledger and (from step 3) the
`emit-asm` baseline checked at every one.

1. `refine` / `using` build and intern sets; `Module#refinements` and
   `used_refinements` stop being mocks. Lookup still ignores them, so
   nothing can regress yet.
2. `search_method_with_refinements` + `refined_names` + the gate. `using`
   bumps `class_version`. Correct end-to-end through the VM; the JIT still
   refuses to compile any iseq with a non-empty set (deopt to VM), which is
   safe because the gate keeps that path cold.
3. `RefinementSetId` into `JitContext` and `inline_cache_map`; lift the
   step-2 refusal. This is the step that must not move the emit-asm
   baseline for the gate-off case.
4. Inline-generator gate, `super`, reflection.

Steps 1–2 were shippable on their own: refinements worked, refinement-using
code was slower than it needed to be, and nothing else in the system
changed speed. Step 3 is where the §3.4 hazard is actually closed, which
is why the JIT kept refusing until it landed — a refusal is a performance
choice, a wrong `FuncId` is not.

Over core/{refinement,module,main,binding,kernel,proc,class,basicobject}
and language, the four steps took 137 failing examples to 30. What is left
is §6.7's basic ops, the per-iseq cell's re-execution case, and one
`import_methods` example that is not a defect (monoruby implements Zlib in
Ruby, so importing from it legitimately succeeds).

---

## 7. Does this change the method frame layout?

Not for method frames. It does need a mutable cell somewhere for the
scopes that can run `using` — but those are a small, bounded set, and the
JIT never has to read it from machine code.

### 7.1 What each kind of body actually needs

The requirement is bounded by what a scope's *own* refinement state does
when it changes mid-execution. Two `using` calls in one scope, with a
`proc` and a `def` interleaved between them (CRuby 4.0.2):

```ruby
module A; refine(Integer) { def tag;  "A" } end
module B; refine(Integer) { def tag2; "B" } end

p0 = proc { … }   ;   def m0 = …          # before both
using A
p1 = proc { … }   ;   def m1 = …          # between
using B
p2 = proc { … }   ;   def m2 = …          # after both

# procs:   p0 -> [A, B]    p1 -> [A, B]    p2 -> [A, B]
# methods: m0 -> [-, -]    m1 -> [A, -]    m2 -> [A, B]
```

Every proc sees the final state, **including the one created before any
`using` ran**. Every method sees the state as of its own `def`, and the
three methods in one scope carry three different sets.

So the scope's refinement set is genuinely modified at runtime; a block
reads the scope's live state at call time rather than snapshotting it,
while `def` snapshots. The cell must therefore be *mutable, owned by the
scope's environment, and read through* — not a value copied into each
closure. A static per-iseq field cannot express both halves.

Runtime mutation of a CREF is not itself new to monoruby: bare `private` /
`public` / `protected` already write `Cref::visibility` in place
(`set_context_visibility`), `module_function` writes `Cref::module_function`
(`set_module_function` / `clear_module_function`), and class bodies and
evals push and pop entries (`push_class_context`, `push_eval_cref`). What
refinements add is that **method resolution** starts depending on that
mutable state — and that the mutation must be visible to blocks already
created and invisible to methods already defined.

But the mutable half is needed only where `using` is legal, and that is
narrow (verified):

| position | `using` |
|---|---|
| toplevel | `main.using` — ok |
| class / module body, `Class.new { }` | `Module#using` — ok |
| `Kernel#eval` at toplevel | ok |
| **method body** | `RuntimeError: Module#using is not permitted in methods` |
| **outside toplevel via main** | `RuntimeError: main.using is permitted only at toplevel` |

A method body therefore only ever needs a **read-only snapshot taken at
`def`** — which is precisely what `ISeqInfo::lexical_context` and
`ISeqInfo::nested_definee` already are. Method frames need no new slot.

The same applies to the JIT: it compiles method / block / loop bodies and
resolves methods at *compile* time, in Rust, with a live frame in hand. It
never needs to load a cref from machine code, so none of the emitted
prologues change.

### 7.2 Three places the mutable cell could live

**(A) A new `LFP_CREF` word in the local frame.** The faithful option, and
there is a template: `LFP_SVAR` was added for the structurally identical
problem — `$~` owned by the LEP, shared with blocks through the outer
chain, lazily allocated with `0` as the "unset" sentinel, marked in
`Lfp::mark`. A cref slot would copy it line for line.

The cost is the layout shift. Today:

```
LFP_OUTER 0   LFP_META 8   LFP_SVAR 16   LFP_BLOCK 24   LFP_SELF 32   LFP_ARG0 40
RSP_LOCAL_FRAME = 40
```

Inserting a word moves `LFP_ARG0` and `RSP_LOCAL_FRAME` to 48, which
reaches 41 `LFP_ARG0` and 149 `RSP_LOCAL_FRAME` references across 26 files
— both architectures' `vmgen/{init_method,method_call,definition}`, both
JIT `compile/` trees, the invokers and native wrappers, `Lfp::heap_frame` /
`move_frame_to_heap` / `frame_bytes`, and `Lfp::mark`. It also spends 8
bytes on *every* frame for something only non-method frames use.

**(B) A side table on the `Executor`, keyed by LEP.** The codebase already
keys per-frame state this way: `deferred_unwind: Vec<(Lfp, MonorubyErr)>`
and `adapter_blocks: Vec<(Value, ProcData)>`. Resolving "my scope's cref"
is the same outer-chain walk to the LEP that `$~` does, followed by a
lookup. No layout change, per-execution correct, and behind the Stage-2
global gate a non-refining program never touches it.

What it has to handle: `move_frame_to_heap` changes an `Lfp`'s identity
(`deferred_unwind` carries the same exposure), entries must be dropped when
the frame dies so a reused stack address cannot inherit a stale cref, and
the stored crefs must be reachable from the GC.

**(C) Per-iseq storage, no frame involvement at all.** Put the cref next to
`lexical_context` on `ISeqInfo`, last-execution-wins — which is already how
`enter_classdef` stamps `lexical_context` today. `using` writes the running
scope's cell, blocks read their mother's, `def` snapshots.

This inherits the staleness class `ISeqInfo` already documents: one cell
per iseq, so re-entrant execution of a scope that runs `using` (a recursive
or concurrently-loaded `Class.new { using … }`, the same file required on
two threads) shares it. Rare, given §7.1's table — but silently wrong when
it happens, which is the failure mode §4 argues against.

### 7.3 Recommendation

(B). It leaves the frame layout alone, is correct per execution rather than
per iseq, and costs nothing while no refinement has been activated. (A) is
the more faithful model and is known-feasible, but it taxes every call in
every program for a feature most never use; it is the right answer only if
per-frame cref turns out to be wanted for other reasons too — `doc/cref.md`
lists several places where monoruby's single VM-wide `lexical_class` stack
already diverges from CRuby, so that is not far-fetched.
