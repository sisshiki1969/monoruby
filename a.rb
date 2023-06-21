puts begin
  puts "begin"
  1/0
  100
ensure
  puts "ensure"
end