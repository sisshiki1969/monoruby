# Hidden instance variables

Some of monoruby's built-in classes are written in Ruby where CRuby
writes them in C: `Thread::Mutex`, `Thread::Queue`, `Thread::SizedQueue`,
`Thread::ConditionVariable`, `Monitor`, `ThreadGroup`,
`Thread::Backtrace::Location`, `Process::Waiter`, the Ruby half of
`Thread` — and, since #1950, `Process::Status`, `Dir`, `File::Stat`,
`Enumerator::ArithmeticSequence` / `Lazy` / `Chain` / `Product`, the
`errno` / `signo` / `key` / `matchee` fields of the exceptions that carry
one, the conversion errors' fields, `Digest`'s buffer, `StringScanner`,
`Ractor` and `GC`'s flags. In CRuby their
state is a C struct behind the object. Nothing in Ruby can see that
struct, and that is observable:

```ruby
m = Thread::Mutex.new; m.lock
m.inspect                 # => "#<Thread::Mutex:0x...>" — no owner shown
m.instance_variables      # => []
Thread::Mutex.new.freeze.lock           # works: the lock is not frozen state
class MyQueue < Thread::Queue
  def initialize; super; @mutex = nil; @closed = 7; end   # its own business
end
```

Kept in ordinary instance variables, the same state shows up in
`inspect`, `instance_variables`, `instance_variable_get` and anything
that walks an object graph; a subclass that happens to use the same name
overwrites it; and every lock is an instance variable write, which a
frozen object refuses (#1937).

A **hidden instance variable** is a slot like any other — the same
`IvarId` in the class's table, the same inline / heap storage, the same
JIT fast paths — whose *name* begins with `/`. No Ruby source can spell
such a name, so:

| | ordinary `@x` | hidden |
|---|---|---|
| `instance_variables`, `inspect`, `Marshal`, `pp` | listed | not listed (`Store::get_ivars`, `RValue::get_ivars`) |
| `instance_variable_get` / `_set` / `_defined?` / `remove_` | reach it | cannot name it (`@__x` there is the *ordinary* variable of that name) |
| a subclass's own `@x` | the same variable | a different variable |
| store on a frozen object | `FrozenError` | allowed |
| `dup` / `clone` | copied | copied (reset it in `initialize_copy` where a copy must start fresh, as `Mutex` does) |
| GC | marked | marked |

The Rust side has used such names for a long time (`IdentId::_NAME` is
`/name`, a Struct class's `/members`, a Time's `/zone`, the JSON
generator's `State` fields). What is new is a way to name them from
Ruby.

## Naming one from Ruby

```ruby
# monoruby-hidden-ivars: true

class Thread::Mutex
  def locked?
    (o = @__owner) ? o.alive? : false     # the hidden slot `/__owner`
  end
  attr_accessor :__held_next               # over the hidden `/__held_next`
end
```

In a file whose leading comment block carries the magic comment
`# monoruby-hidden-ivars: true` (same placement rules as
`frozen_string_literal`; `-` and `_` are interchangeable):

- an instance variable spelled `@__name` is the hidden slot `/__name` —
  everywhere the file can write one: reads, assignments, `||=` / `+=`,
  multiple assignment, `defined?(@__name)`, `"#{@__name}"` and `"#@__name"`
  (`BytecodeGen::ivar_ident`);
- `attr_reader` / `attr_writer` / `attr_accessor` *called from that file*
  with a name beginning `__` define accessors over the hidden slot
  (`Executor::attr_ivar_name`). The accessor keeps what makes an `attr_*`
  method different from a `def`: the JIT inlines it, and calling it is
  not a point where the thread can be switched or interrupted —
  `Mutex#try_lock` and `#unlock` depend on that;
- every other instance variable in the file (`@name` without the double
  underscore) is an ordinary one. `builtins/monitor.rb` uses both: the
  `Monitor` class, which is C in CRuby, keeps `@__mutex` / `@__owner` /
  `@__count`, while `MonitorMixin`, which is Ruby in CRuby too, keeps its
  ordinary `@mon_data`.

Without the magic comment `@__name` is an ordinary instance variable, so
no existing program changes meaning, and code evaluated from a string
(`eval`, `instance_eval("...")`) is a source of its own and never sees a
file's hidden slots.

The Rust side reaches the same slot by its real name:
`globals.store.get_ivar(thread, IdentId::get_id("/__spawn_location"))`.
Moving a variable to a hidden slot therefore means renaming it in the
Rust code that reads it as well (`grep 'get_id("@'`) — the scheduler
reads a thread's `/__name`, `/__report_on_exception` and
`/__abort_on_exception` this way.

## Frozen objects

A store into a hidden slot does not check the frozen flag:

- the interpreter's `set_instance_var_with_cache` (also the `attr_writer`
  wrapper) skips the check when `name.is_hidden_ivar()`;
- the JIT's `StoreIvar` and inlined `attr_writer` emit no `GuardFrozen`
  for a hidden name — and, having checked nothing, add no "this object is
  not frozen" proof to the straight-line facts later stores rely on.

`frozen?`, `freeze`, and stores into the object's ordinary instance
variables are untouched: `Thread::Mutex.new.freeze.instance_variable_set(:@a, 1)`
is a `FrozenError`, as in CRuby.

## When to use it

For state CRuby keeps out of Ruby's sight: a class that is C in CRuby and
Ruby here. Not for a class that is Ruby in CRuby as well — its instance
variables are part of its observable behaviour (`MonitorMixin`'s
`@mon_data` shows in `inspect` in CRuby too).

The magic comment is honoured in any file, not only monoruby's own; a
program that used it could keep slots of its own that reflection does not
show and `freeze` does not protect. That is a monoruby extension with no
effect on programs that do not ask for it.

## What moving a class involves

Besides renaming `@name` to `@__name` under the magic comment:

- **Readers.** `attr_reader :name` reads the *ordinary* `@name`; write
  `def name; @__name; end` (or `attr_reader :__name` when the method
  itself may carry the prefix).
- **Another object's state.** `other.instance_variable_get(:@name)` no
  longer reaches it. Inside `initialize_copy` the slot has already been
  copied, so `@__buffer = @__buffer.dup` does; elsewhere go through a
  private helper (`other.__send__(:__chain_enums)`). In `builtins/*.rb` a
  `__`-prefixed helper is left out of the reflection lists
  (`is_internal_helper`); in `stdlib/*.rb` it is not, so prefer the copied
  slot there.
- **Rust readers** of the variable: `get_id("@name")` becomes
  `get_id("/__name")` (`fill_stat_ivars`, `dir_path_ivar`, the scheduler).
- **Marshal.** Hidden slots are not dumped. Where CRuby dumps a field of
  the C struct under a name without `@` — an exception's `key`,
  `receiver`, `name`, `args`, `status`, `tag`, `result`, `errno`, `signo`,
  `matchee`, the conversion errors' fields, `Process::Status`'s `status`
  and `pid` — `marshal.rs` maps the wire name to the slot in both
  directions (`EXCEPTION_FIELDS`, `OBJECT_FIELDS`; #2005). A field of a
  loaded exception that no class claims is kept under `/marshal/<name>`
  and written back.
  A class whose CRuby object is a C struct with no `_dump_data` (`Dir`,
  `File::Stat`, `StringScanner`, `Digest::Base`, `ThreadGroup`, …) says
  so with a class-level `@__no_dump_data = true`; `Marshal.dump` of an
  instance (or of a subclass's) raises CRuby's `no _dump_data is defined
  for class X` instead of writing an object with no state.
- **Frozen objects.** State in hidden slots no longer makes a frozen
  object unusable, which is CRuby's behaviour for most of these classes
  (`StringScanner#scan`, `Digest#update`, an `Enumerator::Chain`). Where
  CRuby does check the flag (`Dir`, through `GetDIR`) the Ruby code checks
  it itself (`Dir#__dir_check`).
- **Copies.** `dup` copies hidden slots like any other. A class whose
  CRuby copy starts uninitialized, or which owns a resource, resets them in
  `initialize_copy` (`Dir` must not share its descriptor).

A class that is a Struct in CRuby is a Struct here rather than a class
with hidden slots: `Process::Tms`, `Etc::Passwd`, `Etc::Group`.

## Tests

`monoruby/tests/hidden_ivars.rs`: the synchronization classes against
CRuby (no state in `inspect` / `instance_variables`, frozen ones still
lock, subclasses with colliding names), and the mechanism itself
(`hidden_ivars_magic_comment`, monoruby-only).
`monoruby/tests/hidden_state.rs`: the classes moved by #1950 — no
instance variables, nothing in `inspect`, subclasses with colliding
names, frozen objects, copies, the Struct classes and the Marshal round
trip of the exception fields.
`monoruby/tests/marshal_exception_fields.rs`: what `Marshal` writes for
an exception (field names, order, `bt_locations`), the round trip of
every builtin exception's fields, and the objects that cannot be dumped.
