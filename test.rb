class C
  def f(a,(b,c),d,e:30,f:40)
    puts "#{a} #{b} #{c} #{d} #{e} #{f}"
  end
end

class D < C
  def f(a,(b,c),d=100,e:42,f:10)
    a = 100
    c = 50
    e = 200
    __dump
    super
  end
end

D.new.f(1,[2,3],f:70)