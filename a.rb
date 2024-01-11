class C
  @@x=100
  class D
    puts @@x
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

C.new.f
C.g