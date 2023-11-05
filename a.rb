module M
  def initialize
    puts self.class::C
  end
end

class A
  include M
  C = 100
end

class B
  include M
  C = 200
end

A.new # => 100
B.new # => 200
