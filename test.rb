a = 0
30.times do |x|
  30.times do |y|
    30.times do |z|
      s = if x == 28 && y == 28 && z == 28
        "def *(other); 42; end;"
      else
        ""
      end
      Integer.class_eval(s)
      a += 100 * 100
    end
    a += 100 * 100
  end
  a += 100 * 100
end
puts "OK" if a == 269680572