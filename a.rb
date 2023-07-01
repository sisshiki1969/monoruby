class Foo
  def foo(arg)
    "foo called with arg #{arg}"
  end
end

m = Foo.new.method(:foo) # => #<Method: Foo#foo>
        res = []
        res << m[1]       # => "foo called with arg 1"
        res << m.call(2)  # => "foo called with arg 2"
        res << (m === 3)    # => "foo called with arg 3"
        puts res