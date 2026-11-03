require "capi_nogvl"

# No unblock function: the call just runs.
p CapiNogvl.plain(:x), CapiNogvl.plain([1, 2])

def blocked(t)
  Thread.pass until t.stop?
  sleep 0.1 # past rb_thread_call_without_gvl into the read
end

# Thread#wakeup runs the unblock function.
t = Thread.new { CapiNogvl.with_ubf }
blocked(t)
t.wakeup
p t.value

# RUBY_UBF_IO: the interrupt's signal ends the read with EINTR.
t = Thread.new { CapiNogvl.with_ubf_io }
blocked(t)
t.wakeup
p t.value

# Thread#raise also unblocks, and the exception arrives afterwards.
t = Thread.new do
  Thread.current.report_on_exception = false
  CapiNogvl.with_ubf
end
blocked(t)
t.raise(RuntimeError, "boom")
p((t.value rescue [$!.class, $!.message]))

# A trapped signal on the main thread runs main's unblock function, and
# the handler runs once main is back.
going = false
done = false
main = Thread.current
interrupter = Thread.new do
  Thread.pass until going && main.stop?
  sleep 0.1
  prev = Signal.trap(:HUP) { done = true }
  begin
    Process.kill(:HUP, Process.pid)
    sleep 0.001 until done
  ensure
    Signal.trap(:HUP, prev)
  end
end
going = true
p CapiNogvl.with_ubf
interrupter.join
p done
