class C
  def bar
    "bar"
  end
  p = Proc.new { |a| a }
  define_method "foo",7
end

puts C.new.foo

class Integer
  define_method "baz" do |other|
    self * other
  end
end

puts 3.baz(4)
