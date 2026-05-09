# CREF — Class Reference / Constant Reference

CREF is the runtime data structure that records the *lexical* environment
needed by Ruby semantics that aren't local-variable lookup:

| Operation | Reads from CREF |
|---|---|
| `module Foo; end` / `class Foo; end` | parent for the new module/class |
| `def foo; end` | default definee (`cref->klass`) |
| `Foo` (unqualified constant) | lexical scopes to walk in order |
| `Foo = 1` (unqualified assignment) | enclosing class for the new constant |
| `public` / `private` / `protected` / `module_function` | flags toggled on the innermost CREF |
| `Module.nesting` | walk of the lexical chain |
| `using Foo` | refinement set (CRuby only) |

This document describes how CRuby implements CREF and how monoruby's
implementation differs. Read it together with
`monoruby/src/executor.rs` (`Cref`, `lexical_class`,
`push_class_context`, …) and CRuby's `vm.c` / `vm_insnhelper.c` /
`eval_intern.h` (`rb_cref_t`, `vm_cref_push`, …).

---

## CRuby

### Layout

`rb_cref_t` is a heap-allocated `imemo` object (`method.h`):

```c
typedef struct rb_cref_struct {
    VALUE flags;                 // imemo header + CREF flags
    VALUE refinements;           // Hash[refined_class] => refinement_module
    VALUE klass_or_self;         // T_CLASS / T_MODULE / T_OBJECT (singleton)
    struct rb_cref_struct *next; // outer CREF
    const rb_scope_visibility_t scope_visi;  // method_visi (3 bits) + module_func (1 bit)
} rb_cref_t;
```

Three single-bit flags live in `flags`
(`eval_intern.h`):

| Flag | Set by | Means |
|---|---|---|
| `CREF_FL_PUSHED_BY_EVAL` | `class_eval { … }`, `class_exec`, `instance_eval { … }`, `instance_exec`, `Kernel#eval` (no binding) | "this CREF is a runtime override, not a real lexical scope" |
| `CREF_FL_SINGLETON` | `instance_eval` family | "klass_or_self is the *receiver*; promote to its singleton when needed for `def`" |
| `CREF_FL_OMOD_SHARED` | refinement set sharing | refinements hash is borrowed from outer cref |

### Storage

A CREF is linked-list-shaped (`next`) and **owned by the frame's local
environment**. The environment pointer (`ep`) holds the CREF as one of
its slots; `vm_get_cref(ep)` returns it. Each call frame can have its
own CREF chain — there is no VM-wide stack.

### Lifecycle

1. **Toplevel**: `vm_cref_new_toplevel` builds the initial CREF
   `(rb_cObject, METHOD_VISI_PRIVATE, FALSE, NULL, FALSE, FALSE)`. If
   `Kernel#load(path, true)` was used, an additional CREF for the
   wrapper module is pushed on top.

2. **`class Foo … end` / `module Foo … end` / `class << obj … end`**:
   the `defineclass` instruction (`insns.def:802`) pushes a *new* cref
   with `pushed_by_eval = FALSE`, `klass_or_self = Foo`, and chains it
   to the previous CREF. Singleton class form sets `singleton = TRUE`.

3. **`def foo; end`**: reads the current CREF
   (`vm_get_cbase(ep) → CREF_CLASS_FOR_DEFINITION`). The new method
   lands on `cref->klass` (or its singleton if `CREF_FL_SINGLETON`).
   The method's iseq captures the same CREF chain in its environment
   so unqualified constants and `super` resolve relative to where the
   method was *defined*, not where it was *called*.

4. **`module_eval { … }` / `class_eval { … }` / `module_exec` /
   `class_exec`**: `yield_under(self, FALSE, …)` pushes a new CREF
   with `pushed_by_eval = TRUE`, `klass_or_self = self`,
   `singleton = FALSE`. The flag means: `def` inside the block lands
   on `self`, but constant lookup / `module Foo; end` skips this CREF
   (it's not a lexical scope).

5. **`instance_eval { … }` / `instance_exec`**:
   `yield_under(self, TRUE, …)` — same as above but with
   `singleton = TRUE`, so `def` lands on the singleton class of the
   receiver.

6. **`module_eval(string)` / `class_eval(string)`**: `eval_under` —
   `vm_cref_push(self, NULL, FALSE /* pushed_by_eval */, singleton)`.
   Note that the string form sets `pushed_by_eval = FALSE` because the
   parsed string-eval iseq runs in this CREF's lexical scope (whereas
   the block form has its own captured CREF and only needs the
   runtime override).

7. **`Kernel#eval(string)`** (no binding):
   ```c
   /* vm_eval.c:2009 */
   if (!cref && block.as.captured.code.val) {
       rb_cref_t *orig_cref = vm_get_cref(vm_block_ep(&block));
       cref = vm_cref_dup(orig_cref);
   }
   ```
   The caller's CREF is **duplicated** (`vm_cref_dup`) before the eval
   body runs. Setting `module_function` / `private` / `…` inside the
   eval mutates the duplicate's `scope_visi`; the outer CREF is
   untouched.

8. **`Kernel#eval(string, binding)`**: `binding.cref` is used directly.

### Visibility / module_function toggle

`vm_cref_set_visibility` (`vm_method.c:2244`) writes to the *innermost
non-eval* CREF's `scope_visi`:

```c
static void
vm_cref_set_visibility(rb_method_visibility_t method_visi, int module_func)
{
    rb_scope_visibility_t *scope_visi = (rb_scope_visibility_t *)&rb_vm_cref()->scope_visi;
    scope_visi->method_visi = method_visi;
    scope_visi->module_func = module_func;
}
```

`module_function` (no args) sets `(METHOD_VISI_PRIVATE, TRUE)`;
`public`/`private`/`protected` set their visibility *and* clear
`module_func` to `FALSE`. This is why `module_function; def t1; end;
public; def t2; end` produces a regular `t2` — `public` is a state
machine reset, not just a visibility change.

### Constant lookup

`vm_get_ev_const` (`vm_insnhelper.c:1100`) walks the CREF chain, but
**skips eval-pushed entries** when computing the lexical chain:

```c
while (root_cref && CREF_PUSHED_BY_EVAL(root_cref)) {
    root_cref = CREF_NEXT(root_cref);
}
```

This is what makes `module Foo; end` / `X = 1` inside `class_exec`
fall back to the *block's captured* CREF, not the runtime receiver.

### Refinements

Each CREF carries its own `refinements` hash (lazily shared with
parent CREFs via `CREF_FL_OMOD_SHARED`). `using Foo` populates the
*current* CREF's hash; the lookup walks the chain.

monoruby does not implement refinements; the `Cref` struct has no
`refinements` field.

---

## monoruby

### Layout

`Cref` lives in `monoruby/src/executor.rs:2222`:

```rust
struct Cref {
    pub(crate) context: DefinitionContext,
    pub(crate) module_function: bool,
    pub(crate) visibility: Visibility,
    pub(crate) is_lexical: bool,
}

enum DefinitionContext {
    Class(ClassId),     // normal class/module body, class_eval, class_exec
    Receiver(Value),    // instance_eval / instance_exec — singleton lazily on def
}
```

| Field | CRuby analog |
|---|---|
| `context: Class(id)` | `klass_or_self` with `CREF_FL_SINGLETON = 0` |
| `context: Receiver(v)` | `klass_or_self = v` with `CREF_FL_SINGLETON = 1` |
| `module_function` | `scope_visi.module_func` |
| `visibility` | `scope_visi.method_visi` |
| `is_lexical` | **inverse** of `CREF_FL_PUSHED_BY_EVAL` (true when the entry IS a lexical scope) |
| (no field) | `refinements` — refinements aren't implemented |
| (no field) | `flags / OMOD_SHARED` |

### Storage

```rust
// Executor::lexical_class
lexical_class: Vec<Vec<Cref>>,
```

Two-level structure:

- **Outer Vec**: one entry per `require` / `load` boundary
  (`enter_class_context` pushes; `exit_class_context` pops). This
  isolates the requirer's lexical scope from the loaded file.
- **Inner Vec**: the CREF chain for the current require-frame, oldest
  at the bottom, innermost at the top.

In CRuby the chain is per-iseq-frame and stored on the env; in monoruby
it's a single VM-wide stack maintained by the executor.

### Lifecycle

| Event | Helper | Resulting Cref |
|---|---|---|
| Toplevel script start | `Default` for `Executor` initializes `lexical_class = vec![vec![]]` | empty inner vec — `context_class_id()` falls back to `OBJECT_CLASS` |
| `require` / `load` | `enter_class_context` | new empty inner Vec |
| `Kernel#load(path, true)` | `enter_class_context` then `push_class_context(wrap)` | inner Vec is `[wrap-cref(lexical=true)]` |
| `class Foo … end` / `module Foo … end` keyword (`define_class` success) | `push_class_context(class_id)` | `Cref::new(Foo, false, Public)` with `is_lexical=true` |
| `class << obj … end` (singleton class def, `codegen/runtime.rs:850`) | `push_class_context(singleton_class_id)` | same — `is_lexical=true` |
| `Module#class_eval { … }`, `module_eval { … }`, `class_exec`, `module_exec` | `push_runtime_class_context(module.id())` | `Cref::new_runtime(module, Public)` with `is_lexical=false`, `module_function=false` |
| `Module#class_eval(string)`, `module_eval(string)` | `push_runtime_class_context` (note: differs from CRuby — see below) | same as block form |
| `BasicObject#instance_eval { … }`, `instance_exec` | `push_instance_eval_context(self_val)` | `Cref::new_instance_eval(self_val, Public)` — `Receiver` mode, `is_lexical=false` |
| `Kernel#eval(string)` no-binding | `push_eval_cref` (duplicates current top) | a copy of the current innermost Cref; toggles inside the eval don't leak |

### Visibility / module_function

`set_module_function`, `clear_module_function`, `set_context_visibility`
all mutate the **innermost** entry of the current require-frame:

```rust
self.lexical_class.last_mut().unwrap().last_mut().unwrap().module_function = …
```

`Module#public` / `private` / `protected` (no args) call both
`set_context_visibility(visi)` and `clear_module_function()` — matching
CRuby's `vm_cref_set_visibility(visi, FALSE)`.

### "Lexical" vs "definee" — the `is_lexical` distinction

Two queries take the cref:

1. **`context_class_id()`**: the *innermost* Cref's class. Used for
   `def`'s default definee. Walks neither `is_lexical` nor
   require-frames. Direct equivalent of CRuby's
   `vm_get_cbase(ep) → CREF_CLASS_FOR_DEFINITION`.

2. **`lexical_context_class_id(globals)`**: the *innermost lexical*
   class. Walks only the current require-frame top-down, returning the
   first `is_lexical=true` entry. If none, falls back to the iseq's
   captured `lexical_context.last()` (or `OBJECT_CLASS`). Used for
   `module Foo; end` / `class Foo; end` parent and for
   `Module.nesting`. Equivalent to CRuby's
   "`while CREF_PUSHED_BY_EVAL: next`" walk in `vm_get_ev_const`.

The split exists so `class_exec` / `module_eval` / `instance_exec`
push only a method-definee override:

```ruby
class A; end
A.class_exec do
  def foo; end       # → A (definee = receiver)
  module Inner; end  # → toplevel (lexical scope unchanged)
  C = 100            # → toplevel
end
```

### iseq-captured lexical context

Each `ISeqInfo` also stores `lexical_context: Vec<ClassId>` —
populated by `enter_classdef` (`codegen/runtime.rs:43`) when entering
a `class`/`module` keyword body. This is the *static* / parse-time
analog of CRuby attaching the cref to the iseq's environment.

`Executor::definition_func_id` chooses between the current frame's
iseq lexical_context and the enclosing method's, used by:

- unqualified `X = 1` (`set_constant`): the lexical scope's class is
  the assignment target.
- `lexical_context_class_id`'s fallback when no lexical Cref is on
  the runtime stack (e.g. inside a block whose surrounding eval push
  was non-lexical).

### `Kernel#eval` cref isolation (PR #444)

`Kernel#eval(string)` without a binding calls
`Executor::push_eval_cref()` which duplicates the current innermost
Cref onto the same require-frame. Mutations from the eval'd source
(`module_function`, `private`, …) hit the duplicate, and
`pop_eval_cref()` discards it on return. Mirrors CRuby's
`vm_cref_dup(orig_cref)` in `eval_string_with_cref`.

### GC

`Executor`'s `mark` impl (`executor.rs:155`) walks all frames and
marks `DefinitionContext::Receiver(v)` entries — `instance_eval`'s
receiver only lives on the cref stack while the eval body runs, and
`receiver.class()` may be a singleton class held alive only through
this entry. Class IDs (`Class` variant) are interned in the
`ClassInfoTable` and reachable via `Globals`'s root, so they don't
need explicit marking here.

---

## Side-by-side summary

| Concept | CRuby | monoruby |
|---|---|---|
| Type | `rb_cref_t` heap imemo | `Cref` plain `Copy` struct (16 bytes) |
| Linkage | linked list via `next` | Vec inside Vec; index = depth |
| Per-frame storage | yes — `ep[CREF_SLOT]` | no — shared `Executor::lexical_class` |
| Real-vs-runtime split | `CREF_FL_PUSHED_BY_EVAL` flag | `Cref::is_lexical` (inverse polarity) |
| Receiver-mode (`instance_eval`) | `CREF_FL_SINGLETON` + `klass_or_self = receiver` | `DefinitionContext::Receiver(value)` |
| `def` definee | `CREF_CLASS_FOR_DEFINITION` (auto-promotes to singleton when flag set) | `context_class_id()` (handles both variants) |
| Constant chain | walks `cref.next`, skipping `PUSHED_BY_EVAL` | `lexical_context_class_id` walks current frame skipping `!is_lexical`, falls back to iseq's static `lexical_context` |
| Toggle reset (`public` clears `module_function`) | `vm_cref_set_visibility(visi, FALSE)` | `set_context_visibility(visi)` + `clear_module_function()` |
| Eval cref dup | `vm_cref_dup` (deep, dup refinements) | `push_eval_cref` (shallow Copy of innermost Cref) |
| Refinements | first-class field on `rb_cref_t` | not implemented |
| `Kernel#load(path, true)` wrap | extra cref above toplevel | extra `push_class_context(wrap)` after `enter_class_context` |
| Require boundary | implicit (each script gets its own toplevel CREF chain) | explicit outer Vec (`enter_class_context`/`exit_class_context`) |

---

## Known gaps in monoruby's CREF

- **Refinements.** No tracking; `using Foo` is a no-op stub.
  ~83 `core/module` ruby/spec failures are attributable to this.

- **Per-PC cref propagation for `Module.nesting` inside methods.**
  `current_class_nesting` walks the runtime stack, which is accurate
  inside class/module *bodies* but not inside *method* bodies — CRuby
  uses the method's iseq-captured cref chain. monoruby's iseq does
  store `lexical_context: Vec<ClassId>`, but `Module.nesting` doesn't
  consult it yet.

- **`module_function` / `private` toggles inside `Kernel#eval` with a
  `Binding`.** PR #444's `push_eval_cref` only fires for the
  no-binding form; the binding form uses the binding's own cref.
  CRuby has the same shape but its binding-cref is never reused
  across calls — monoruby may need a sweep here too.

- **`pushed_by_eval` propagation through `define_method` proc body.**
  When a `proc` is wrapped into a method via `define_method`, the
  proc's outer cref isn't faithfully reproduced on the wrapper's
  iseq. Currently observable as ~3 failures in `define_method`'s
  "nested method in default definee" / "lambda for break" specs.
