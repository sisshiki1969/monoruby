a = [*(1..100)]
a << 4.2
100.times do
  x = 0
  1.times do
    a.each do |e|
      x += e
    end
  end
  puts x
end