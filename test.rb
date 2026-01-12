def bar
  eval("x = x + 1", $b)
end

res = []
100.times do
  [1].all? do |b|
    $b = binding
  end
  x = 100
  bar
  res << x
end


puts res.inspect