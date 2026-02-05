def f(r1, r2, o1=100, o2=200, *rest, p1, p2)
  puts "r1:#{r1}, r2:#{r2}, o1:#{o1}, o2:#{o2}, rest:#{rest.inspect}, p1:#{p1}, p2:#{p2}"
end

50.times {
  f("1","2","3","4","5","6","7","8")
}
