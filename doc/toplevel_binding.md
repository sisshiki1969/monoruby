# `TOPLEVEL_BINDING` and the main script's frame

Why the main script no longer runs inside `TOPLEVEL_BINDING`, what is
built instead and when, and which reads still force the old shape.

---

## 1. The problem: a binding's frame is a captured frame

`TOPLEVEL_BINDING` has to expose exactly the main script's locals —
live, and shared with anything the binding sets dynamically:

```ruby
# main.rb
p TOPLEVEL_BINDING.local_variables   # => [:a]   (parse-time locals, before the assignment)
a = 1
p TOPLEVEL_BINDING.local_variable_get(:a)   # => 1
```

The direct way to get that is to *be* the binding's frame: compile the
main script as a body of the binding and run it there
(`Globals::compile_main_script_binding`). That is what monoruby did.

The cost was invisible and large. A `Binding`'s frame is heap-allocated,
so the main script's frame has the `on_heap` bit set in its `Meta`, and
`vm_loop_start` — the VM's loop-JIT trigger — begins with
`branch_if_captured`:

```
testb [r14 - (LFP_META - META_KIND)], 0b1000_1000   ; on_heap | invalidated
jnz   cont                                           ; …never count, never compile
```

A captured frame's locals may be aliased through the heap, which the
register-caching JIT cannot honour, so the trigger skips such frames
entirely. For the main script that meant **top-level code was never
JIT-compiled at all** — not "compiled and then deoptimized", never
compiled: with `--features jit-log` a top-level-loop program reports
`elapsed JIT compile time: 0ns`, and `--no-jit` runs it at the same
speed.

Measured on x86-64 (release, best of 3), the same loop as a main script
versus inside a `require`d file (a plain stack frame):

| | main script | via `require` | CRuby 4.0.2 |
|---|---:|---:|---:|
| `benchmark/so_mandelbrot.rb` | 3.41 s | 0.21 s | 1.98 s |
| minimal `while` loop over floats | 1.35 s | 0.13 s | 0.77 s |

Method-shaped code was never affected — only code running directly at
the top level. (The benchmark harness measures the method-wrapped form:
`benchmark/so_mandelbrot.yml` wraps the body in `def do_it`, which is why
the suite never showed this.)

## 2. What happens now

`TOPLEVEL_BINDING` is registered at startup as a **lazy constant** —
`ConstStateKind::LazyToplevelBinding`. The name is *defined* (it lists in
`Object.constants`, answers `const_defined?` and `defined?`, and reports
no autoload) but holds no `Binding` object yet.

- The main script runs as a plain toplevel body on a **stack** frame
  (`Executor::try_exec_main_script_plain`), so its loops reach the JIT
  like any method's.
- The first *read* of the constant builds the binding
  (`Executor::materialize_toplevel_binding`), over the main script's own
  frame — promoted to the heap exactly as `Kernel#binding` promotes a
  method's frame. Reader and script then share one set of locals, which
  is the semantics above. Promotion mid-run is the same event a
  `binding` call inside a hot method loop causes, and is handled by the
  existing `invalidated` tombstone machinery.
- Reading it where no main-script frame is running — during a `-r`
  require, before the script starts — builds an empty frame on the main
  object, which is what CRuby exposes at that point too.
- If a read happened *before* the main script starts (a `-r` library
  that touched it), the script runs inside that already-built binding:
  the old path, unchanged, because the binding has already handed out
  the frame its locals must live in.

## 3. Scripts that name the constant themselves

Two readers cannot be served by "build it over the frame that is running
right now":

```ruby
z = 7
at_exit { p TOPLEVEL_BINDING.local_variable_get(:z) }   # toplevel frame is gone by then
Thread.new { p TOPLEVEL_BINDING.local_variables }.join  # another call chain
```

So a main script that *names* `TOPLEVEL_BINDING` anywhere in its own
source — a mention inside a block counts — runs inside the binding, the
pre-existing path, and keeps the pre-existing semantics.

The test is the constant sites the script's compilation recorded
(`Store::names_constant_since`), so it sees exactly the literal
references the script compiled: a mention in a comment does not count, a
reference nested in a block or a `def` does. A script that trips this
loses top-level JIT, as before; a script that does not mention the
constant pays nothing.

Dynamic reads from *other* files (`Object.const_get(:TOPLEVEL_BINDING)`
in a library, an `eval`) are not statically visible, and do not need to
be: they run while the main script's frame is live, so §2's
materialization serves them. The residual gap — a *required library*
reading it from a thread or an `at_exit` handler, in a program whose own
source never mentions the constant — yields the empty binding.

## 4. Where the pieces live

| Piece | Location |
|---|---|
| Lazy constant slot | `ConstStateKind::LazyToplevelBinding`, `globals/store/class/constants.rs` |
| Registration at startup | `Executor::init`, `executor.rs` |
| First-read hook | `Executor::get_constant`, `executor/constants.rs` |
| Building the binding | `Executor::materialize_toplevel_binding`, `executor.rs` |
| Fast path + static scan | `Executor::try_exec_main_script_plain`, `executor.rs` |
| Binding path (unchanged) | `Globals::compile_main_script_binding`, `globals.rs` |
| `<main>` backtrace label | `Store::func_description_for`, `globals/store.rs` (keyed by the recorded main-script `FuncId`, since a plain main body is otherwise indistinguishable from a required file's toplevel) |
| Tests | `monoruby/tests/main_script.rs` |
