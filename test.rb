a = []

100.times do |x|
  case x % 10
  when *[1,2],5
    a << "Matched 1"
  when 3,*[3,9,Object.new]
    a << "Matched 2"
  else
    a << "Not matched"
  end
end

puts a.inspect