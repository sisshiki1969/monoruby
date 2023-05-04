begin
  100
rescue StandardError
  puts "rescue"
  100
else
  puts "else"
  200
ensure
  puts "ensure"
  300
end
500