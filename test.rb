flag = false
100.times do |x|
  puts x
  if x % 7 == 6 && !flag
    flag = true
    redo
  end
  flag = false
end