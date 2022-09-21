@a = []

def foo
  @a << :foo
  []
end

def bar
  @a << :bar
end

foo[0] = bar
a = foo[0] = bar
x, foo[0] = bar, 0
puts @a