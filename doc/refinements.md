# Refinements — what stands in the way, and where

monoruby does not implement refinements. `Module#refine` is undefined;
`Module#using` and `main.using` exist but only validate their argument
(there is never a refinement to activate). `Module#used_refinements` and
`Module#refinements` are Ruby-level mocks returning `[]`.

This document records *why* that is, in terms of the resolution machinery
that would have to change. Read it together with `doc/cref.md` (which
describes the CREF that refinements hang off) and `doc/jit.md`.

The short version: refinements make the resolved method a function of the
**caller's lexical scope**, and every method-resolution path in monoruby —
the global method cache, the VM inline cache, JIT compile-time resolution,
and the class-version repair path — is keyed without it. The JIT's repair
path is the dangerous one: it would silently re-validate machine code that
resolved the wrong method.

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

## 2. The prerequisite monoruby does not have: a per-frame cref

`doc/cref.md` covers this in full; the part that blocks refinements:

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

This is Stage 0 of any implementation. It is a change to where scope
state is *stored*, not to how lookup works — and, as §7 works out, it does
not have to be a change to the frame layout.

---

## 3. Where each resolution layer breaks

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
every inline cache in the program, and `using` at the top of a file is
common.

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
The options are (i) teach the inline generators to fire only when the
compiling cref has no refinement for that name — which means giving
`InlineGen` the cref — or (ii) fall back to `set_bop_redefine`, which
permanently disables the operator fast paths for the whole process the
moment any file refines one. (ii) is a large, irreversible performance
cliff for a feature most programs never touch.

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

## 4. Why the cheap version is worse than nothing

A stub `refine` that creates an anonymous module, evaluates the block in
it and returns it — no activation — was prototyped and measured. It
unblocks 55 of the 59 `core/binding` examples (see §5) because the blocker
there is a fixture that merely *calls* `refine` at load time.

It was not kept. Without activation, `refine` turns every refinement-using
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

## 6. A staged implementation, if it is ever done

Ordered so that each stage is independently testable and the
non-refining fast paths stay untouched until the last one.

**Stage 0 — per-frame cref.** See §7 for what this does and does not
require of the frame layout. Independently valuable: it is the same defect
that made `Module.nesting` read another scope's stack.

**Stage 1 — data model, no lookup change.** `Cref` gains
`refinements: Option<Rc<HashMap<ClassId, Module>>>` (shared with the outer
cref until written, mirroring CRuby's `CREF_FL_OMOD_SHARED`). `refine`
builds the anonymous module and records its refined class; `using` merges
a module's set into the current cref. `Module#refinements` /
`used_refinements` stop being mocks. Lookup still ignores all of it.

**Stage 2 — resolution, behind a global gate.** Add
`search_method_with_cref`. Keep a process-wide "any refinement has ever
been activated" flag; while it is false — the overwhelmingly common case —
every path in §3 keeps its current key and its current speed. This gate is
what keeps the feature from costing anything when unused.

**Stage 3 — caches.** Extend the method-cache key and the VM inline-cache
guard for the refining case only. Make `using` bump `class_version`.

**Stage 4 — JIT.** Thread the compiling iseq's cref into `JitContext`;
record it in `inline_cache_map` entries so `update_inline_cache` re-asks
the same question (§3.4); suppress inline generators and basic-op fast
paths for names refined in the compiling cref (§3.6); give `super` its
refinement rule (§3.7).

Stages 0–2 make refinements *correct but slow*; Stage 3–4 make them not
slow down everything else. Stopping after Stage 2 is a defensible
intermediate state. Stopping after Stage 1 is not — that is the trap in §4.

---

## 7. Does this change the method frame layout?

Not for method frames. It does need a mutable cell somewhere for the
scopes that can run `using` — but those are a small, bounded set, and the
JIT never has to read it from machine code.

### 7.1 What each kind of body actually needs

Two measurements bound the requirement (CRuby 4.0.2):

```ruby
pr = proc { 1 + 1 }        # created BEFORE the using
def m_before = 1 + 1       # defined BEFORE the using
using R
p pr.call     # => "refined+"   block sees the later activation
p m_before    # => 2            method does not
```

A block created before the `using` **is** refined; a method defined before
it is **not**. So the cref is a *mutable cell owned by the scope's
environment*, shared by reference with every block that captured that
environment, while `def` snapshots the pointer at definition time. A static
per-iseq field cannot express both halves.

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
