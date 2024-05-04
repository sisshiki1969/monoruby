a = 0
for x in 0..30
  for y in 0..30
    for z in 0..30
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
puts a