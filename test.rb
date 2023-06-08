class C
  def initialize
    @a=42
  end
end

for i in 0..10
  puts C.new.inspect
end