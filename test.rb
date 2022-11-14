
num = 50000
c = 0

for i in 0...num
  x = rand
  y = rand

  if x*x+y*y <= 1.0
    c += 1
  end
end

puts 4.0 * c / num
