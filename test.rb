def f(&p)
  g(&p)
end

def g(&p)
  yield
end

f { puts 100 }

p = Proc.new { puts 200 }
f(&p)