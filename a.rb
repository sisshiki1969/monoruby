a = [1,2]
b = []
7.times do
    b.prepend(*a)
end
puts "#{b}"