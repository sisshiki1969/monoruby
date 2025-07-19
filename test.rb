class C
  def foo(*x)
    puts "#{x.inspect}"
  end
end

C.new.foo(a:42, 100 => 100, c:5)
