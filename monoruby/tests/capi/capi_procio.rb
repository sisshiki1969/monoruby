require "capi_procio"
P = CapiProcio
def t = (yield) rescue [$!.class, $!.message]

# Proc / Method
pr = proc { |*a, **kw| [a, kw] }
p P.proc_call_kw(pr, [1, { k: 2 }], true), P.proc_call_kw(pr, [1, { k: 2 }], false)
p t { P.proc_call_kw(pr, [1, 2], true) }
p P.proc_call_with_block_kw(proc { |a, k:, &b| [a, k, b.call] }, [1, { k: 3 }], proc { :blk })
def kwm(*a, **kw, &b) = [a, kw, b&.call]
p P.method_calls(method(:kwm), [1, { z: 9 }], proc { :b })
p P.yield_kw([1, { q: 2 }]) { |*a, **kw| [a, kw] }
r = []
P.yield_block_each([1, 2]) { |x| r << x * 10 }
p r
class Pub; def pub(a, k: 0) = [a, k]; private def priv = :no; end
p P.public_kw(Pub.new, :pub, [1, { k: 5 }]), t { P.public_kw(Pub.new, :priv, [{}]) }
p P.eval_cmd("1 + 2", []), P.eval_cmd(proc { |a, b| a * b }, [6, 7])
class Basic; def hash = 1; end
p P.basic_p(Object, :hash), P.basic_p(Basic, :hash), P.basic_p(Basic, :no_such_method)

# Fiber
p P.fibers
p t { P.fiber_raise }
p P.fiber_storage({ x: 1 })

# Thread
p P.threads
p t { P.thread_dead_wakeup }
Thread.report_on_exception = false
p t { P.thread_raise }
Thread.report_on_exception = true
p P.gvl_in_nogvl

# Exceptions
e = RuntimeError.new("given")
p P.make_exception([]), P.make_exception(["str"]), P.make_exception([e]).equal?(e)
x = P.make_exception([ArgumentError, "msg", ["a:1"]])
p x, x.backtrace
p t { P.make_exception([1]) }, t { P.make_exception([1, 2, 3, 4]) }
bad = Object.new
def bad.exception(*) = 1
p t { P.make_exception([bad]) }
p P.syserr(Errno::ENOENT::Errno, "path"), P.syserr(Errno::EACCES::Errno, nil)
p t { P.error_frozen }
p t { P.error_frozen_object([1]) }
fe = (P.frozen_error_raise(:recv) rescue $!)
p fe.class, fe.message, fe.receiver
k = Class.new
o = k.new
k.define_method(:inspect) { P.error_frozen_object(o) }
p (P.error_frozen_object(o) rescue $!.message.sub(k.inspect, "K"))
p P.set_errinfo(e).equal?(e), P.set_errinfo(nil), t { P.set_errinfo(1) }
p P.errnos
p (begin; P.interrupt; rescue Interrupt => e; [e.class, e.message]; end)

# IO
rd, wr = IO.pipe
p P.io_misc(rd, wr)
rd.close
p t { P.io_maybe_wait_closed(rd) }
# Never initialized is not the same as closed (#2108).
p t { P.io_maybe_wait_closed(IO.allocate) }
p t { P.io_get_open_file(IO.allocate) }
p t { P.io_get_open_file(rd) }
p t { P.io_get_open_file(wr) }
wr.close
path = File.join(__dir__, "capi_procio.rb")
File.open(path) { |f| p P.io_path(f) == path }
p P.io_path(STDIN)
require "tmpdir"
Dir.mktmpdir do |dir|
  f = File.join(dir, "a")
  File.write(f, "x")
  File.open(f, "a") { |io| p P.io_mode_append(io) }
  File.open(f, "r") { |io| p P.io_mode_append(io) }
end
p P.io_open_descriptor(path)
p P.cloexec

# GC
p P.gc_misc
p t { P.latest_gc_info("str") }
P.finalizers(Object.new, Object.new, proc { puts "finalizer ran" })

# Processes and files
p P.processes
p P.files

# Odds and ends
p P.misc
p P.source
p P.frame_info
p P.enum_with_size([1, 2, 3], [:a, :b]).size

# Classes
p t { P.class_new(Class) }, t { P.class_new("".singleton_class) }, t { P.class_new(1) }, t { P.class_new(Comparable) }
class ProcioBase; end
class ProcioSub < ProcioBase; end
p P.define_class("ProcioSub", ProcioBase) == ProcioSub
p t { P.define_class("ProcioSub", String) }, t { P.define_class("Comparable", Object) }
p t { P.define_class("ProcioNoSuper", nil) }
module ProcioOuter; class Inner; end; end
p t { P.define_class_under(ProcioOuter, "Inner", String) }, t { P.define_class_under(ProcioOuter, "Fresh", nil) }
p P.undef_method(ProcioBase, "never_defined")
p t { P.undef_method(Class.new.freeze, "never_defined") }.to_s.sub(/0x\h+/, "X"), t { P.undef_method(Module.new.freeze, "nope") }.to_s.sub(/0x\h+/, "X")
ProcioOuter.const_set(:K, 1)
$stderr = STDOUT
P.define_const(ProcioOuter, "K", 2)
$stderr = STDERR
p ProcioOuter::K
