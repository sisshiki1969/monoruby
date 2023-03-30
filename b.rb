puts begin
    puts "body"
    100
rescue
    puts "rescue"
    150
else
    puts "else"
    200
ensure
    puts "ensure"
    250
end
