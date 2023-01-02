a = 100
p = Proc.new { 1.times { a+=1 } }
puts a
p.call
puts a
