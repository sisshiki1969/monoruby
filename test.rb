class C
  def initialize(a,b,c,d)
    @a=a
    @b=b
    @c=c
    @d=d
  end
end

for i in 0..10
  puts C.new(10,20,30,40).inspect
end