def f(a:0, b:1, **c)
  puts "#{a} #{b} #{c}"
end

f()
f(b:100)
f(b:100, **{:a=>1, 5=>7})