x = [2] * 20
x << :j
x.each do |x|
  puts [0,1,2,3,4,5][x]
end