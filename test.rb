def calc
  f=4
  c=0
  while c < 10
    f = 1.2*f
    c += 1
  end
  puts f
end

100.times do calc end