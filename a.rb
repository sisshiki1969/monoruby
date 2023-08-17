@peeks = {}
peek = [1,2,3,4,5]
peek = @peeks[peek] ||= peek
#peek = @peeks[peek] || (@peeks[peek] = peek)
#       %1 = %2.[%1]    %2 = @peeks; %2:.[%1:] = %1
puts "peak:#{peek} @peeks:#{@peeks}"