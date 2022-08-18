CONST = 5.7
sum = 0
for i in 0..19 do
    sum += CONST
    CONST = 1000 if i == 12
    puts "#{i}, #{CONST}, #{sum}"
end
puts sum