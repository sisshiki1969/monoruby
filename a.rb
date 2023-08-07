fib = Enumerator.new do |y|
    a = b = 1
    loop do 
        y << a
        a, b = a + b, a
    end
end

#fib.with_index.each do |(num, idx2), idx1|
#    puts "#{idx1} #{idx2} #{num} "
#    if num > 1000
#        break
#    end
#end

e = fib.with_index.with_index.with_index.with_index.with_index.with_index
puts e
puts "#{e.next}"
puts "#{e.next}"
puts "#{e.next}"
puts "#{e.next}"
puts "#{e.next}"
puts "#{e.next}"