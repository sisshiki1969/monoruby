class A
  attr_accessor :w
end
a = A.new
res = []
for i in 0..100
  puts i
  if i == 8
    class A
      puts "redefine"
      def w=(v)
        @w = v * 2
      end
    end
  end
  a.w = 42
  res << a.w
end
res