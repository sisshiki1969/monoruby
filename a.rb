a = [1,2,3]
b = []
10.times do
    b.prepend(*a)
end
puts "#{b}"