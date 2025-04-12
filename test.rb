100.times do |x|
  case x % 20
  when 0
    puts "zero"
  when 1
    puts "one"
  when 2
    puts "two"
  when 3,4,5,6
    puts "three-six"
  when 7
    puts "seven"
  when 8
    puts "eight"
  when 9
    puts "nine"
  when 10,11,12,13,14
    puts "ten-fourteen"
  when 15
    puts "fifteen"
  when 16
    puts "sixteen"
  when 17
    puts "seventeen"
  else
    puts "above eighteen"
  end
end