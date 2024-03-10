class C
  class D
    E = 1
    def self.e
      E
    end
  end
end

C::D::E = 100
puts C::D.e