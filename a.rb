fib = Enumerator.new do |y|
    a = b = 1
    loop do 
        y << a
        a, b = a + b, a
        if a > 100 then break end
    end
    7777
end
puts "res = #{fib.each {|x| puts x}}"