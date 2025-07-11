
class Foo
  def foo(*arg, a: nil)
    "foo called with arg #{arg}"
  end
end

m = Foo.new.method(:foo)
p = m.to_proc
puts p.call("test", 42, :Ruby, a:1, b:2)