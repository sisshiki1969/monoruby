def f(x, a:0, b:0, **rest)
  puts "a:#{a}, b:#{b}, rest:#{rest.inspect}"
end

50.times {
  f(1, a:"a", b:"b", c:"c", d:42)
}
