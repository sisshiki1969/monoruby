module A
  def method1()  end
  def protected_method1()  end
  protected :protected_method1
end
class B
  def method2()  end
  def private_method2()  end
  private :private_method2
end
class C < B
  include A
  def method3()  end
end

puts A.method_defined? :method1              #=> true
puts C.method_defined? "method1"             #=> true
puts C.method_defined? "method2"             #=> true
puts C.method_defined? "method2", true       #=> true
puts C.method_defined? "method2", false      #=> false
puts C.method_defined? "method3"             #=> true
puts C.method_defined? "protected_method1"   #=> true
puts C.method_defined? "method4"             #=> false
puts C.method_defined? "private_method2"     #=> false