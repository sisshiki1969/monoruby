100000.times do    
    a = Enumerator.new do |y|
        3.times do |i|
            y << i
        end
    end
    puts a.next
    puts a.next
end