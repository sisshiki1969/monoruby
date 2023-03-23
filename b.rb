class C
  def f(a,b,e:30,f:40)
    puts "#{a} #{b} #{e} #{f}"
  end
end

class D < C
  def f(a,*b,e:42,f:10) # %1(a), %2, %3(b), %4(c)  ArgList
    super # Callsite arg_num:2
  end
end

D.new.f(1,2,3,4,f:70)  # Callsite arg_num:2