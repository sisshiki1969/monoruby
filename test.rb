@a = []

def foo
  @a << :foo
  []
end
def bar
  @a << :bar
end

x, foo[0] = bar, 0
puts @a