a = 100
p = nil
q = nil
1.times {
  p = Proc.new {
    3.times {
      a+=1
    }
  }
  q = Proc.new {
    5.times {
      a+=10
    }
  }
}
p.call
q.call
puts a