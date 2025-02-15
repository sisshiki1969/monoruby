class C
  def g(*rest, **kw)
    $res << "#{rest} #{kw}"
  end
  
  def f(a,b,...)
    a = 50
    g(1,2,a:100)
    g(...)
    g(b,a,...)
  end
end

40.times {
  $res = []
  C.new.f(1,*[2,3],e:70,**{f:80, g:90})
  puts "#{$res}"
}