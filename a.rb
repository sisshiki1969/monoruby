fib = Enumerator::Generator.new do |y|
    a = b = 1
    loop do 
        y << a
        a, b = a + b, a
    end
end

fib.each do |num|
    puts "#{num}"
    if num > 1000
        break
    end
end