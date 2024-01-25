def splitmix32(x)
	x = (x + 0x9e3779b9) & 0xffffffff
	z = x
	z = (z ^ (z >> 16)) * 0x21f0aaad & 0xffffffff
	z = (z ^ (z >> 15)) * 0x735a2d97 & 0xffffffff
	return z ^ (z >> 15), x
end

20.times do |x|
  r, x = splitmix32(x)
  puts r
end