require "capi_nogvl"

# A hang here is a bug under test: say which case hung and fail fast
# rather than run into the test harness's timeout.
$phase = "start"
Thread.new do
  sleep 60
  $stderr.puts "capi_nogvl: hung in #{$phase}"
  exit!(1)
end

# No unblock function: the call just runs.
p CapiNogvl.plain(:x), CapiNogvl.plain([1, 2])

def blocked(t)
  Thread.pass until t.stop?
  sleep 0.1 # past rb_thread_call_without_gvl into the read
end

# Thread#wakeup runs the unblock function.
$phase = "wakeup"
t = Thread.new { CapiNogvl.with_ubf }
blocked(t)
t.wakeup
p t.value

# RUBY_UBF_IO: the interrupt's signal ends the read with EINTR.
$phase = "ubf_io"
t = Thread.new { CapiNogvl.with_ubf_io }
blocked(t)
t.wakeup
p t.value

# Thread#raise also unblocks, and the exception arrives afterwards.
$phase = "raise"
t = Thread.new do
  Thread.current.report_on_exception = false
  CapiNogvl.with_ubf
end
blocked(t)
t.raise(RuntimeError, "boom")
p((t.value rescue [$!.class, $!.message]))

# A trapped signal on the main thread runs main's unblock function, and
# the handler runs once main is back.
$phase = "signal"
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

# The same with no grace period: the signal can reach main before its
# read starts, and main must still run the handler when it next waits
# (here in #join) rather than park with the signal pending.
$phase = "signal, early"
3.times do
  going = false
  done = false
  interrupter = Thread.new do
    Thread.pass until going && main.stop?
    prev = Signal.trap(:HUP) { done = true }
    begin
      Process.kill(:HUP, Process.pid)
      sleep 0.001 until done
    ensure
      Signal.trap(:HUP, prev)
    end
  end
  going = true
  r = CapiNogvl.with_ubf
  interrupter.join
  p [r, done]
end

# RUBY_UBF_IO with no grace period: the signal can land before the read
# starts; the joiner re-sends it until it takes.
$phase = "ubf_io, early"
3.times do
  t = Thread.new { CapiNogvl.with_ubf_io }
  Thread.pass until t.stop?
  t.wakeup
  p t.value
end
