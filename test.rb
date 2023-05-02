begin
	puts 100
  require "woooxxx"
rescue ZeroDivisionError => e
	puts e
rescue LoadError => e
	puts e
else
  puts 200
end