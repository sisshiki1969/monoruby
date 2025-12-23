a = 100

p = Proc.new{ puts a}

100.times {
  p.call
}