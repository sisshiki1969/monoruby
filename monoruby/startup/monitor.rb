class Monitor
  def synchronize
    yield
  end

  def try_enter
    true
  end

  def enter
  end
end