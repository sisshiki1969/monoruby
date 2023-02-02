class C
end
c = C.new
def c.f; 5; end

for __i in 0..7 do
  puts c.f
end
