def f
  a = 42						# <= このa
  1.times do |x|
  	puts a
  end
end

100.times do f end