def f(a)
  r = nil
  a.each { |x| r = x }
  r
end
25.times do |i|
  m = f([100]) {}
  p [i, m, m.nil?]
end