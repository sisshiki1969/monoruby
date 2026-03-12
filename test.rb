j = 100
p = proc {}

def foo(b)
  p b.source_location
end

b =  p.binding
foo(b)
