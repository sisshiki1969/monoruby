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

50.times {
  5.times {|i|
    puts i*2
  }
}