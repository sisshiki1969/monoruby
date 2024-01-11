class C
  @@x=100
  class D
    @@x=200
  end
end

class C
  def f
    puts @@x
  end
  def self.g
    puts @@x
  end
end

20.times do
  C.new.f
  C.g
end