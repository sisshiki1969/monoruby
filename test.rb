# foo.rb
def foo
  2.times do |i|
    2.times  do |j|
      puts "i=#{i} j=#{j}"
      return 3 if i == 1 && j == 0
    ensure
      puts "ensure in j loop, j=#{j}"
    end
  ensure
    puts "ensure in i loop, i=#{i}"
  end
ensure
  puts "ensure in foo"
end

puts "foo=#{foo}"
