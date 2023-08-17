@peeks = {}
peek = [1,2,3,4,5]
peek = @peeks[peek] ||= peek
puts "peak:#{peek} @peeks:#{@peeks}"