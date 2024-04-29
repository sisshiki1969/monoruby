6.times do |x|
  6.times do |y|
    6.times do |z|
      s = if x == 4 && y == 4 && z == 4
        "def *(other); 42; end"
      else
        ""
      end
      Integer.class_eval(s)
      puts 100 * 100
    end
  end
end