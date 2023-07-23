res = []
e = [1,2,3,4,5].to_enum
e.each do |x|
  res << x.to_s
end
puts "#{res}"