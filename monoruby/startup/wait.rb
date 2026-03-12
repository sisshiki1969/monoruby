# Minimal io/wait stub for monoruby
# In CRuby this is a C extension (wait.so) that adds wait_readable/wait_writable to IO.
# Provide no-op stubs so that libraries requiring 'io/wait' don't fail.

class IO
  def wait_readable(timeout = nil)
    self
  end

  def wait_writable(timeout = nil)
    self
  end

  def wait(events = nil, timeout = nil)
    self
  end

  def ready?
    true
  end

  def nread
    0
  end
end
