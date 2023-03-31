10.times do
    #c = [0,0,0,0]
    puts begin
        puts "body"
        1/0
        100
    rescue => c
        puts "rescue #{c}"
        150
    else
        puts "else"
        200
    ensure
        puts "ensure"
        250
    end
end
