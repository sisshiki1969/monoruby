def f
  yield
end

40.times do
  i = 0
  f do
    i = 42
  end
  puts i
end