class A
  CONST = 'A'

  class B < A
    def self.test1
      CONST          # 通常の定数探索（CREF）
    end

    def self.test2
      self::CONST    # self起点の探索（self == B）
    end
  end
end

puts A::B.test1  # => 'A'
puts A::B.test2  # => 'A'
