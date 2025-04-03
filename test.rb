$i = 0

def bar(&b)
  10.times &b
  puts "done: bar"
end

def foo(&b)
  bar &b
  puts "done: foo"
end

foo do |i|
  puts i
  next if i < 5
  puts "break:#{i}"
  break
end