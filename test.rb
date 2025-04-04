$i = 0

def bar(&b)
  10.times &b
  puts "done: bar"
end

def foo(&b)
  bar &b
  puts "done: foo"
end

30.times {
  foo do |i|
    puts i
    next if i < 5
    puts "break:#{i}"
    break
  end

  puts "done: main"
}