30.times do |x|
  30.times do |y|
    30.times do |z|
      s = if x == 28 && y == 28 && z == 28
        "def *(other); 42; end"
      else
        ""
      end
      Integer.class_eval(s)
      res = 100 * 100
      if x > 26 && y > 26 && z > 26
        arrow = if x == 28 && y == 28 && z == 28 then "<=" else "" end
        puts "  #{z}: #{res} #{arrow}"
      end
    end
    res = 100 * 100
    if x > 26 && y > 26 then puts " #{y}: #{res}" end
  end
  res = 100 * 100
  if x > 26 then puts "#{x}: #{res}" end
end