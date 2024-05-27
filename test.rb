class C
  def f(*rest, **kw)
    puts "#{rest} #{kw}"
  end
end

class D < C
  def f(a,...)
    a = 50
    super
  end
end

D.new.f(1,*[2,3],e:70,**{f:80, g:90})