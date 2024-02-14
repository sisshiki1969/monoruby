def f(x)
  puts "#{x}"
end

15.times do
  f(**{a:1})
end