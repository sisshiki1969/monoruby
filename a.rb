for j in 0..100
  class B
    attr_reader :w
    def w=(v)
      @w = v * 2
    end
  end
  class A
    attr_accessor :w
  end
  a = A.new
  res = []
  for i in 0..10
    if i == 8
      a = B.new
    end
    a.w = 42
    res << a.w
  end
  puts "#{j} #{res}"
end