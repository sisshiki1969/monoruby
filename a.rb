class Foo
  def foo(x)
    x * 100
  end
end

x = [[0,1,2,3]] * 20
x << Foo.new.method(:foo)
x.each do |a|
  puts a[2]
end