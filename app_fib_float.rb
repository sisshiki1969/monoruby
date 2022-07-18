def fib(x)
  if x<3 then
      1.0
  else
      fib(x-1)+fib(x-2)
  end
end;

puts fib 40.0