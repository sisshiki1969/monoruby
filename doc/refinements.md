# Refinements — the design, and why it is shaped this way

monoruby implements refinements. `Module#refine`, `Module#using` and
`main.using` are real, `Refinement` is a class with `#target` and
`#import_methods`, and `Module#refinements` / `Module.used_refinements` /
`Module.used_modules` report the truth. The interpreter and the JIT both
resolve through an activated refinement.

This document is the design record: §§1–3 are the problem — what
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

> **As landed (#1973).** The set is a field of each frame on the JIT's
> specialization stack (`JitStackFrame::refinements`, read from
> `Store::iseq_refinements` of the frame's own iseq when the frame is
> pushed), not of the compilation unit, and `JitContext::refinements()`
> answers for the frame being compiled. It works in both directions: a
> helper written before `using` and inlined into a block that activated
> refinements keeps its plain `+`, and a method written under `using` and
> inlined into a caller that activated nothing keeps its refined one. Every
> recorded resolution (`InlineCacheEntry::refinements`, §6.6) carries the
> set of the frame that asked, so the class-version repair re-asks each
> frame's question under that frame's set. A Ruby-written core method
> inlined into a refined caller resolves its own calls under no refinement
> at all, as its stand-alone compilation always did.
>
> The run-time half is the *frameless* callee (`compile/frameless_call.rs`),
> which has no control frame: a helper it calls that reads the scope off
> the nearest control frame (`Executor::site_refinements`,
> `Executor::dispatch_redefined_op`) finds the frame *hosting* the body. The
> only such helpers a frameless body may call are the pure `==` / `!=`
> ones, so the JIT tells them whose scope they answer for
> (`JitContext::pure_eq_scope`, `executor::op::PureScope`): the host's when
> the body was written in the same scope, "user code with no refinement"
> when it was not — a plain callee under a refined caller, a user's block
> run inside `Array#each` — and no helper at all (hence a frame) for a
> body under a set its host does not share. The second case was wrong
> without any refinement in the program: a block compiled into the
> Ruby-written `Array#each` took the core's exemption from a redefined
> `String#==` / `Float#==`.

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
  the refined search (`Store::search_method_refined` and the
  `find_method_for_*_refined` entries in `globals/store/class.rs`).

A program that refines `String#blank?` pays nothing on `Array#each`,
`Integer#+`, or any of the other tens of thousands of call sites. The tax
is proportional to how much is refined, which is the right shape and is
what keeps a refinement-using program fast, not just a refinement-free one.

### 6.3 Where the mutable cell lives

Only toplevel / class-module bodies / eval-at-toplevel own one (§7.1), and
it holds a `u32`. It lives on the body's `ISeqInfo::refinements` (§7.2
option **C**): no frame-layout change, and `None` for every body that
neither defines a method nor runs `using`.

A block without a cell of its own resolves through its lexical parent's,
so it sees a `using` that runs in that parent after the block was
created. A method's cell is the snapshot `def` took. (`def` re-executing under a *different* set writes a different id;
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
> `Executor::site_refinements`. monoruby writes part of its core library
> in Ruby where CRuby uses C: `Array#map`'s own `i += 1` is library code,
> C in CRuby and invisible to any refinement. Resolving it against the
> caller ended the loop after one iteration. This was first drawn for
> operator names only; §6.10 is the rule as it stands, for every name.

### 6.8 The remaining pieces

- **`super` inside a refinement** means "the method this refinement
  shadows". Record `refined_class: Option<ClassId>` on the refinement
  module and special-case `find_super` when the running body belongs to
  one, instead of trying to express it as a position in the real ancestor
  chain (§3.7).
- **Reflection** (`send`, `respond_to?`, `Object#method`) resolves with the
  *caller's* set: the same `nearest_ruby_frame` walk the eval builtins now
  use, then that frame's set id. Only reached with the gate on. (Called by
  the Ruby-written core they see no refinement — §6.10.)

### 6.10 Whose lookup is it — call site, Ruby-level funcall, or funcall

A refinement reaches the call sites *written* in the scope that activated
it. Everything else that looks a method up is a builtin doing so, and
CRuby is specific about which of those see a refinement. monoruby had one
`find_method` for all of them, which asked the nearest Ruby frame — so a
refined `R#<=>` was what `[r, r1].sort` compared with (#1998), and a
refined `Array#size` was what `Array#each`, written in Ruby here, looped
to (#2029). There are three scopes now, and the caller says which it
means:

| Scope | `Executor` | Answers | Used for |
|---|---|---|---|
| **Call site** | `site_refinements` | the running frame's own set; `EMPTY` for a native frame and for a frame of the Ruby-written core | call sites and operators (`find_method_at_site`, `invoke_method_op`), interpolation, `case` / `when` and `rescue`'s `===`, `defined?(recv.m)` |
| **Ruby level** | `ruby_level_refinements` | the nearest *user* Ruby frame's set, walking out through native frames and frames of the Ruby-written core — CRuby's `rb_vm_cref()`, the core being C there | `public_send` and `&:sym` (`CALL_PUBLIC`), `&obj` → `to_proc`, the second try of `rb_funcallv_public`, the C API's `rb_respond_to`, the scope a `def` / `refine` snapshots |
| **Funcall** | — (`RefinementSetId::EMPTY`) | no refinement | everything else a builtin calls: `find_method`, `invoke_method_inner`, `invoke_method_if_exists` — `rb_funcall` / `rb_check_funcall` |

The default is the blind one. `<=>` under `sort`, `coerce` under `1 + x`,
`to_ary` under a destructuring, `to_s` under `puts`, `inspect` under `p`,
`each` under `Enumerable`, `initialize_copy` under `dup`,
`method_missing`, `respond_to_missing?` and the rest are all lookups a
builtin makes with `rb_funcall`, and none of them is refinable.

The reflective builtins (`send`, `__send__`, `respond_to?`, `method`,
`instance_method`, …) resolve under `caller_refinements`: the Ruby-level
scope of whoever called them, except that a call *from the Ruby-written
core* gets no refinement. There `x.respond_to?(:m)` followed by `x.m` is
the core's spelling of `rb_check_funcall` and `x.__send__(:m)` of
`rb_funcall`; the core says `public_send` where CRuby says
`rb_funcallv_public`.

Four places where CRuby's own rule is less obvious, and monoruby follows
it:

- **`rb_funcallv_public`** (`inject(:sym)`, `Random.rand` on a duck)
  asks the class first, through the global call cache, which knows no
  refinement. Only when that finds no *public* method does `rb_call0`
  look again, and for `CALL_PUBLIC` it does so with the Ruby-level
  refinements. So `[r, r].inject(:+)` calls a `+` that only a refinement
  gives `R`, and the class's own `+` if it has one
  (`Executor::public_funcall_refinements`). Under `Enumerable#inject`
  the nearest Ruby frame is the receiver's own `each`.
- **`Foo.new(...)`** at a call site is `opt_new` in CRuby 4.0: an
  allocation, then a call of `initialize` *from that site*. A refined
  `initialize` therefore runs, although `Class#new` the method —
  `Foo.send(:new)`, `Foo.method(:new).call`, a site that passes a block —
  calls it blind. monoruby's `Class#new` is a Ruby method of the core,
  whose `initialize` call is blind like any other; once some refinement
  defines `initialize`, a `new` site in a refining scope dispatches to a
  native twin that asks its caller's scope (`Store::site_target`, applied
  by the VM's lookup, the JIT's, and the class-version repair alike, so
  that they agree on the target).
- **`&:sym`** written where any refinement is active is not
  `Symbol#to_proc`'s proc but one made for that scope
  (`refine_sym_proc_call`), which calls whatever the refined lookup finds
  with no visibility check. `:sym.to_proc.call(x)` keeps the check.
- **The C API**: `rb_check_funcall` and the conversions built on it
  (`rb_check_convert_type`, `rb_check_array_type`, …) probe blind and
  count private methods, as the call that follows does;
  `rb_respond_to` sees the Ruby-level refinements
  (`MR_RESPOND_TO_FUNCALL`).

Two things CRuby does that monoruby does not reproduce, both a
refinement-aware `rb_respond_to` followed by a blind call that then
raises: `Marshal.dump` of an object whose `marshal_dump` exists only in a
refinement, and `$stdout = obj` where only a refinement gives `obj` a
`write`.

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
it happens.

### 7.3 What was chosen

(C), `ISeqInfo::refinements`. It leaves the frame layout alone and costs
nothing while no refinement has been activated. Its one gap is the
staleness above: a block that runs `using` and is executed more than once
keeps the previous execution's set as its base, which shows only when each
execution activates a *different* refinement (`Module.new { using
make_refinement }` in a loop). A class body does not have the problem:
`enter_classdef` re-seeds its cell on every entry. (A) remains the faithful
model if a per-frame cref is ever wanted for other reasons — `doc/cref.md`
lists places where monoruby's VM-wide `lexical_class` stack diverges from
CRuby.
