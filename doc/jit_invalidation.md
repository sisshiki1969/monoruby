# Invalidating compiled code — and repairing it instead

Compiled code bakes in answers that Ruby is allowed to change later: which
`FuncId` a call site dispatches to, what value a folded constant has, which
operator is still the builtin. monoruby guards those answers with two
global version counters, and a guard failure used to mean one thing —
throw the body away and compile it again.

That was far too blunt. The counters are *global*: any `def` anywhere moves
the class version, any `X = 1` anywhere moves the constant version. A
program that keeps defining things (a spec suite, a Rails boot) moved them
constantly, so hot bodies were recompiled over and over even though
nothing they had cached had actually changed.

**Salvage** is the answer: on a guard failure, re-validate what the body
assumed. If every assumption still holds, stamp the current version into
the body and keep the code. Only a genuine change recompiles. On a
ruby/spec `core` run this turns ~107,000 recompiles into ~320.

This document covers what moves the counters, how a unit records what it
assumed, and how each guard failure is repaired. For the stub and
class-guard-chain shapes the guards live in, see [`jit.md`](jit.md); for
basic-operator redefinition — the one invalidation that is *not* repaired
this way — see [`bop_redefinition.md`](bop_redefinition.md).

---

## 1. The two counters, and what moves them

Both live in `Codegen` (`codegen.rs`) and are read by compiled code as a
plain memory word.

**Class version** — `Globals::class_version_inc()`:

| Site | Trigger |
|---|---|
| `Store::insert_method` (`globals/store/class.rs`) | `def`, `define_method`, `attr_*`, `alias_method` |
| `Store::remove_method` | `remove_method`, `undef_method` |
| visibility update (same file) | `public` / `private` / `protected` on an existing entry |
| `include_or_prepend_module` (`value/rvalue/module.rs`) | `include`, `prepend` |
| refinement import (`builtins/module.rs`), `using` (`executor.rs`) | `refine`, `using` — see [`refinements.md`](refinements.md) §6.1 |

**Constant version** — `Globals::const_version_inc()`, and the per-name
epochs described in §4:

| Site | Trigger | Epoch bumped |
|---|---|---|
| `Globals::set_constant` | any constant assignment, *including the first* | that name |
| `Globals::remove_constant` | `remove_const` | that name |
| bare `const_version_inc()` (`value/rvalue/module.rs`) | `include` / `prepend` — resolution may change without any assignment | wildcard |

Note the third row: an event with no single name to attribute the change
to bumps a *wildcard* epoch, which makes every unit's fast path (§4)
conservative until it re-validates.

## 2. One patchable snapshot word per compilation unit

A compilation unit is a root body plus every specialized callee inlined
into it. `jit_compile` (`codegen/jitgen.rs`) creates **one** class-version
word and — when the body folded at least one constant — **one**
const-version word for the whole unit:

```rust
let class_version_label = self.jit.const_i32(class_version as _);
let const_version_label = (!const_folds.is_empty())
    .then(|| self.jit.const_i64(const_version as _));
```

Every guard in the unit, children included, compares the global counter
against that one word. Compilation is atomic at one version, so one word
is enough — and one store re-validates the entire unit. `set_class_version`
/ `set_const_version` (`codegen.rs`) are that store.

> **Both arches read the word.** aarch64 used to bake the snapshot in as an
> immediate, which silently made salvage a no-op there: the word was
> patched and the guard kept comparing against the old immediate. Fixed in
> #1157; `a64_guard_class_version` now takes the unit's label.

The word is reachable afterwards through the unit's salvage record:

| Unit kind | Record | Keyed by |
|---|---|---|
| whole method | `ISeqInfo::jit_entry` → `JitInfo` | `self_class` |
| loop (OSR) | `ISeqInfo::loop_jit_info` → `LoopJitInfo` | `(self_class, LoopStart index)` |
| specialized child | its owner's record, via `SpecializedPatchEntry::owner` | — |

## 3. Class-version salvage

`JitInfo` / `LoopJitInfo` carry an `inline_cache_map: Vec<InlineCacheEntry>`
— every method call the compiler resolved at compile time, as
`(recv_class, name, refinements, func_id)`. `Store::salvage_method_unit` /
`salvage_loop_unit` (`globals/store/class.rs`) re-ask each question with
`check_method_for_name` and compare the answer:

- **all unchanged** → return the unit's version label; the caller patches
  it and the code stands.
- **any changed** → `None`; recompile.

Two details that are easy to get wrong:

- **The refinement set is part of the question.** Re-asking the *unrefined*
  question after a `using` moved the version would confirm an answer the
  code is no longer allowed to give (`refinements.md` §3.4).
- **`super` sites need the executing frame.** `check_method_for_name` takes
  `Option<Lfp>`; a `super` entry (`name: None`) resolves relative to the
  owning frame. A salvage triggered from an inlined child has no such
  frame, passes `None`, and conservatively fails — so a unit containing a
  cached `super` site recompiles rather than guessing.

**Specialized children salvage their owner.** A child's guard reads the
owner's word, and the owner's cache map covers the child's call sites, so
`salvage_specialized` (`codegen/compiler.rs`) validates the *owner* unit.
`SpecializedPatchEntry::owner` is cleared to `None` when a child is
recompiled individually: the fresh body reads its own fresh words, which
the owner's record no longer names, and patching the owner's word would
"heal" words that body never reads — deopting it on every call, forever.

## 4. Const-version salvage — two tiers

A const-version guard failure says only "some constant event happened
somewhere". `ConstSalvageMap` (`globals/store/iseq.rs`) records what the
unit actually folded — for each site, the `ConstCache` tuple the emitted
code relies on and the names whose redefinition could change its
resolution (the final name plus every path qualifier) — plus a snapshot of
those names' epochs at compile time.

`Store::salvage_const_unit` then escalates:

1. **Per-name epochs.** Every event in §1 bumps the epoch of the name it
   touched. If no folded name's epoch moved (and the wildcard didn't), no
   fold can have changed — patch the word, done, without a single lookup.
   This is the common case: a spec file defining unrelated constants.
2. **Value re-check.** For a site whose name *was* touched, compare the
   fold against that site's VM inline cache. A cache already refreshed at
   the current version whose `(value, base_class, self_class)` equals the
   fold proves the assignment wrote the same value — patch and keep.
   A **differing** value recompiles.
   A **stale** cache (the VM has not re-run the site at this version yet)
   returns `ConstSalvage::Defer`: skip the recompile, let this invocation
   deopt so the interpreter refreshes the cache, and retry on the next
   miss. Bounded by `stale_defers` (3) so a fold whose site the deopted
   execution never reaches cannot defer forever.

The recorded fold values are GC roots: `ConstSalvageMap::mark` is reached
from `ISeqInfo::mark`, so a folded `Value` stays alive as long as the code
that baked it in.

## 5. Where a guard miss goes

All misses call into Rust *before* deciding. The entry points live in
`codegen/compiler.rs`.

| Unit | x86-64 | aarch64 |
|---|---|---|
| whole method, class ver | `jit_recompile_method_with_recovery` — on a successful salvage returns 1 and the stub's `jnz recover` **resumes in place**, no deopt | `jit_recompile_method` — salvages, then deopts once |
| whole method, const ver | `jit_recompile_method` → `salvage_method_const` | same |
| loop (OSR) | `jit_recompile_loop` → `salvage_loop` (class or const) | same |
| specialized | `jit_recompile_specialized` → `salvage_specialized` | same |

Except for x86's resume-in-place path, a successful salvage still lets the
current invocation deopt to the VM once; the healed code passes its guards
from the next call or iteration.

**Salvage must run outside the `CODEGEN` borrow.** Validation reads
`Globals::class_version()` / `const_version()`, which borrow the same
thread-local `RefCell` — so the salvage attempt sits in the `extern "C"`
entry point, before `CODEGEN.with(...)`, and the helpers in `Store` return
the `DestLabel` for the caller to patch in its own borrow context rather
than patching it themselves.

**Const guards are not counter-gated.** The generic recompile side exit
(`SideExit::RecompileDeoptimize` → `AsmInst::RecompileDeopt`, gated by
`COUNT_DEOPT_RECOMPILE`) is one-shot: once drained
it never re-arms. That is right for an expensive recompile but wrong for a
cheap salvage — the *second* version move after a successful salvage would
strand the body in the interpreter for the rest of the run. So a
const-version miss calls the salvaging entry on every failure, the same
shape the class-version guard uses. A *block*-style root is the exception
and keeps a plain deopt: the whole-method recompile entry rebuilds
whatever `lfp.func_id()` names as a method, which is the wrong frame shape
for a block body.

## 6. What is not salvaged

| Reason | Behaviour |
|---|---|
| `NotCached`, `MethodNotFound`, `IvarIdNotFound` | the compiler lacked information; recompiling with a warmed VM cache is the point |
| `BecamePolymorphic` | a monomorphic-compiled send saw a second class. Ratcheted: recompile while the PMC holds fewer than two classes, plain deopt afterwards (`RecvMissMode::Learn`) |
| basic-operator redefinition | not a version guard. Bodies that inlined the operator are *evicted* (`ISeqInfo::evict_jit_code`, `bop_deps`) — see [`bop_redefinition.md`](bop_redefinition.md) |
| front-end bail | a *whole-method* bail marks the iseq `jit_invalidated`, and it is never compiled again; a loop bail just leaves the site interpreted |

## 7. The megamorphic gate

Salvage repairs code that is still correct. The complementary question —
*should this receiver class be compiled at all?* — is answered by the
class-guard chain's **miss exit**, and it is worth stating because getting
it wrong is invisible.

A chain miss must leave the chain and run the call in the interpreter
(`jit_class_guard_fail: jmp vm_entry`). The stub samples the receiver
class only when its warm-up counter expires, so a class used for a single
call is sampled at most once, evicted from the small profile
(`profile_self_class`: cap 8, threshold 2), and never compiled for.

When that exit label was left unbound (bound only under
`#[cfg(feature = "profile")]`, so ordinary builds emitted a branch with a
zeroed rel32 — a no-op), every miss fell through into the profiler
instead, sampled the class twice back-to-back, crossed the threshold and
compiled a specialization for **every** class that ever missed. ruby/spec's
concurrent-subclasses example compiled ~15,000 `Object#should` bodies
(~300 MB) and timed the suite out. Fixed in #1158.

## 8. Measuring

Build with `--features jit-log`; `jit_stats::dump()` prints at exit:

```
version / salvage stats:
  class_version incs:                25022
  recovery attempts (class guard):   ...
    salvaged (re-resolution ok):     ...
  recovery attempts (loop guard):    ...
  recovery attempts (spec guard):    ...
  recovery attempts (const guard):   15740
    salvaged:                        10485
    value-compared sites:            13309
    fail: cache stale:               ...
    fail: value changed:             0
  whole recompiles  class-ver:       ...
  ...
```

Read it as: *attempts* are guard failures that reached a salvage entry,
*salvaged* are the ones that kept their code, and the `recompiles` block
is what was left over. `fail: value changed` staying at 0 over a whole
spec run is the empirical case for the whole mechanism — the constant
events were real, but never changed a folded value.

ruby/spec `core` (2,144 files / 23,034 examples), x86-64. Measured in two
series, because the base moved in between — do not read across them:

**Salvage, measured against the pre-#1156 base:**

| | before salvage | class-version salvage (#1151, #1155) | + const-version salvage (#1157) |
|---|---|---|---|
| specialized recompiles (class ver) | 90,593 | 68 | 149 |
| loop recompiles (class ver) | 12,395 | 0 | 0 |
| whole recompiles (const ver) | 4,174 | 4,148 | 155 |
| JIT compile time | 22.4 s | 15.6 s | 12.8 s |
| peak code emission | 350.7 MB | 327.7 MB | 297.9 MB |

Version-guard-driven recompiles across the whole run: **107,220 → 323**.

**The megamorphic gate (§7), measured against current master:**

| | before #1158 | after #1158 |
|---|---|---|
| `Object#should` whole compiles | 14,945 | 30 |
| JIT compile time | 23.1 s | **0.44 s** |
| peak code emission | 318.4 MB | **10.6 MB** |

The gate dominates everything else: it was compiling one body per receiver
class, which is why its emission dwarfs the recompile traffic salvage
removes.

## 9. If you change this code

- A new event that can change method resolution must bump the class
  version; one that can change constant resolution must bump the constant
  version **and** the right epoch (a name, or the wildcard when there is
  no single name).
- A new compile path must register a salvage record, or its guard
  failures will recompile forever. The whole-method and loop paths do this
  in `compile_patch` / `compile_partial_by_id`.
- A new question the compiler bakes in needs a new record and a new tier —
  do not extend an existing map with a differently-shaped invariant.
- Never bind a guard's miss-exit label inside a `cfg` block (§7), and keep
  the snapshot side of a version compare a *word*, never an immediate (§2).
