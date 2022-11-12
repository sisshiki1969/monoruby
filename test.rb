class Array
  def iich
    for i in 0...self.size
      yield(self[i])
    end
  end
end

a = []
[2,5,7,10,2.2,7,9].iich do |x|
  a << x*2
end
puts a