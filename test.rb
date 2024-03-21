class C
  def bar
    "bar"
  end
  p = Proc.new { return "foo" }
  define_method "foo", p
end

puts C.new.foo

class Integer
  define_method "baz" do |other|
    self * other
  end
end

puts 3.baz(4)
