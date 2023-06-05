def f(&p)
  g(&p)
end

def g(&p)
  p.call
end

f { puts 100 }

p = Proc.new { puts 200 }
f(&p)