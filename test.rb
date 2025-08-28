      __res = (
        class C
          a = 42
          define_method :foo do |x|
            a += x
          end
        end
        c = C.new
        g = 0
        100.times { |x|
            g += c.foo(x)
        }
        g
        )
      for __i in 0..1000 do
          __res2 = (
        class C
          a = 42
          define_method :foo do |x|
            a += x
          end
        end
        c = C.new
        g = 0
        100.times { |x|
            puts "x:#{x} g:#{g}"
            g += c.foo(x)
        }
        g
        )
          __assert(__res, __res2)
      end
      (
        class C
          a = 42
          define_method :foo do |x|
            a += x
          end
        end
        c = C.new
        g = 0
        100.times { |x|
            g += c.foo(x)
        }
        g
        )