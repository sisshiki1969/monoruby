for __i in 0..7 do
    class A
        attr_accessor :w
    end
    class B
      def w
        100
      end
    end
    a = A.new
    a.w = 42
    res = []
    for i in 0..10
      if i == 8
        a = B.new
      end
      res << a.w
    end
    res
end
