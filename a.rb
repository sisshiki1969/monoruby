fib = Enumerator.new do |y|
    a = b = 1
    loop do 
        y.<< a
        a, b = a + b, a
        if a > 100 then break end
    end
    777
end

ans = []
puts "#{ans << fib.with_index(1.1) {|x, i| ans << x; ans << i}}"