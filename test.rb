begin
	puts 100
  1/0
rescue Exception => e
	puts e
else
  puts 200
end