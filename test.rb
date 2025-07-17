class C
  def foo(a:100, b:10)
    puts "#{a.inspect} #{b.inspect}"
  end
end

C.new.foo(a:42, :b => 100)
