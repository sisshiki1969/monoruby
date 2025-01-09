class Integer
  def times
    if block_given?
      i = 0
      while i < self
        yield i
        i += 1
      end
      self
    else
      self.to_enum(:times)
    end
  end
end

def f
  block_given?
end

30.times { 30.times { f{} } }