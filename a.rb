def f
	yield [1,2,3]
end

10.times do
	f do |a, *b|
		puts "a:#{a} b:#{b}"
	end
end