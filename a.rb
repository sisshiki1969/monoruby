puts begin
  puts "begin"
  1/0
  100
rescue LoadError
  puts "error"
ensure
  puts "ensure"
end