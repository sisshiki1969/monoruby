class A < Array
end

for i in 0..8 do
mul = 3
a = A.new(5) {|i| i * mul }
a << 4
a[2] = 5
puts "#{a}"
end