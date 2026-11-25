# error_highlight

The source line and the caret under the report of an uncaught
`NameError`, `TypeError` or `ArgumentError`:

```
t.rb:2:in '<main>': undefined method 'foo' for nil (NoMethodError)

x.foo(1)
 ^^^^
```

This is the `error_highlight` default gem, vendored unmodified
(`vendor/ruby-stdlib/error_highlight/`, 0.7.2 with Ruby 4.0.9). It
prepends `ErrorHighlight::CoreExt#detailed_message` to the three
classes; that method asks `ErrorHighlight.spot(exception)` for the
extent to underline and appends the snippet. monoruby supplies the two
things the gem takes from the VM: when it is loaded, and which node a
backtrace location was executing.

## When it is loaded

CRuby's `gem_prelude.rb` requires it at every start. Here that would be
about 6% of startup (14M of 226M instructions) for something only a
failing program shows, so `builtins/gem_prelude.rb` registers
`ErrorHighlight` as an **autoload**, as it does `Gem`:

- `defined?(ErrorHighlight)` and `Object.constants` report the constant;
  any reference to it loads the gem.
- `Exception#detailed_message` (`builtins/exception.rb`) loads it the
  first time it is asked about an exception of one of the three classes
  while the autoload is still pending. Loading prepends `CoreExt` to the
  receiver's class too late for the call already under way, so that one
  call invokes `CoreExt#detailed_message` directly
  (`__error_highlight_message`); its `super` returns to
  `Exception#detailed_message`, which now has nothing left to load. A
  subclass's own `detailed_message` that calls `super` therefore sees
  the same string on the first call as on every later one.
- `message`, `full_message` of other classes, and a rescued error nobody
  asks about load nothing.

The one visible difference from CRuby: until the load,
`NameError.ancestors` (and the other two) lack `ErrorHighlight::CoreExt`.

`--disable-error_highlight` and `--disable-gems` leave the constant
undefined, as in CRuby (`Globals::no_error_highlight`).

A module prepended to one of the three classes has to end up *above*
`CoreExt`, as it does on CRuby where error_highlight was there first:
did_you_mean's `Correctable` on NameError, for a report to read message,
snippet, suggestions, or a program's own decoration. So `Module#prepend`
on NameError, TypeError or ArgumentError runs the pending autoload before
it prepends anything (`Exception.__load_error_highlight`, the same entry
the first message uses).

## Which node a frame was executing

The gem calls, in this order:

1. `RubyVM::AbstractSyntaxTree.of(location, keep_script_lines: true)`,
   which on a prism-compiled CRuby raises a RuntimeError whose message
   names prism;
2. `RubyVM::AbstractSyntaxTree.node_id_for_backtrace_location(location)`;
3. `Prism.parse_file(location.absolute_path)`, in whose tree it looks
   for the node with that id.

monoruby has no `RubyVM`, and does not grow one for this: libraries take
`defined?(RubyVM)` to mean CRuby's VM is there to be asked for
instruction sequences and statistics. `monoruby_hooks/error_highlight.rb`
(loaded right after `error_highlight/base.rb`, whichever copy of the gem
that was) defines `ErrorHighlight::RubyVM::AbstractSyntaxTree` instead,
as a **private constant**. Every reference in the gem is inside
`module ErrorHighlight`, so lexical lookup finds it there, and nothing
outside can.

CRuby's instructions carry the id of the node they were compiled from.
monoruby's carry that node's *extent* in the source (`Loc`, a first and
a last byte, in `ISeqInfo::sourcemap`), which is what backtrace line
numbers come from. So:

- A `Thread::Backtrace::Location` keeps the extent of the node its frame
  was executing, packed in one Integer (`kernel::node_extent_value`),
  and the absolute path of its source. `Kernel.__caller_frames` hands
  them over with each frame; an exception's locations get them from
  `Exception#__raise_backtrace_sites`. (That also makes an exception
  location's `absolute_path` the path the file was loaded from — stable
  across `chdir`, nil for eval'd code — as `caller_locations` already
  had it.)
- `Location#__node_id` → `Kernel.__location_node_id(path, extent)` reads
  the file, parses it with libprism the way `Prism.parse_file` does (no
  options, so the ids are the ones the gem's own parse will produce) and
  walks the tree for the node covering exactly that extent
  (`builtins::prism::node_id_at`). -1 when there is none.

Two details of the walk:

- Several nodes can cover one extent (`foo` on a line of its own is a
  `CallNode` inside a `StatementsNode` inside the `ProgramNode`). A node
  kind the gem can spot wins; failing that, any node that is not a mere
  grouping.
- `Foo::Bar += 1`: monoruby attributes a failed read of `Foo::Bar` to
  the constant path, CRuby to the whole assignment (it compiles the read
  as part of it). The path and each of its scopes start where the
  assignment starts, and nothing else under it does, so those answer the
  assignment's id.

The frame of a method whose arguments did not bind (an arity or keyword
error) is at its `def` / block / lambda, whose extent is the definition
node's: that is what the `callee:` half of the report underlines.

## What differs from CRuby

- A builtin that is C in CRuby and Ruby here keeps its own frame in the
  backtrace (`<internal:class>:19:in 'Class#new'`), where CRuby shows
  the caller's line under the builtin's name. Such a frame has no file
  to quote. So `Foo.new` with the wrong number of arguments reports
  `caller: <internal:class>:19` with no snippet (the `callee:` half is
  intact), and a TypeError raised inside a Ruby-implemented builtin has
  no snippet.
- The first decorated message of a process also loads prism's Ruby half
  (`require "prism"`, about 310M instructions), since the gem works on
  `Prism::Node` objects.

## Tests

`monoruby/tests/error_highlight.rs` spawns the binary:
`tests/error_highlight/cases.rb` holds 121 cases and `cases.expected`
their output on CRuby 4.0.9 (`ruby --disable-did_you_mean cases.rb`,
with the directory replaced by `@DIR@`). The gem's own
`test/error_highlight/test_error_highlight.rb` passes too, apart from
three cases that differ in an unrelated TypeError message; it references
a top-level `RubyVM` itself, so running it needs
`RubyVM = ErrorHighlight.module_eval("RubyVM")` first.
