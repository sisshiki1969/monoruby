  class C
    def m
    end
  end

  o = C.new
  m = :m

  for i in 1..100_000_000
    o.__send__ m
  end