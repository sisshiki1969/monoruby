def f(x,*r,a:1,b:2,&p)
  puts "#{[x, a, b, g(&p)]}"
end

def g
  yield 3
end

for i in 0..10
  f(2,b:3,a:4) {|x| x + 14}
end