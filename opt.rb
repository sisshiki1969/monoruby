def fact(x)
  if x <= 1.0 then
      1.0
  else
      x * fact(x-1.0)
  end
end;
puts fact 130.0