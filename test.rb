50.times do
  x = 0.0
  begin
    x += 1.0
    raise "err" if x < 3.0
  rescue
    i = 0
    while i < 50
      retry if i == 49
      i += 1
    end
  end
end