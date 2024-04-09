  class C
    def a
      puts "a"
    end
    def b(x, y, z)
      puts "x=#{[x, y, z]}"
    end
    def c(*x)
      puts "x=#{x}"
    end
    def d(a:7)
      puts "a=#{a}"
    end
    def e(a=5)
      puts "a=#{a}"
    end
  end

  o = C.new
  30.times { o.send(:a) }
  30.times { o.send(:b, 1, 2, 3) }
  30.times { o.send(:c, 7, 8, 9) }
  # 30.times { o.send(:d, a:12) } => NG
  30.times { o.send(:e, 7) }