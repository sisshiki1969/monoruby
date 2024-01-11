def m
  i = 0
  while i<30_000
    puts i
    i += 1
    yield
  end
end

m{}