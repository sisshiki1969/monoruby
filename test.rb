e = Enumerator.new do |y|
  a = b = 1
  loop do
      y << a
      a, b = a + b, a
  end
end

def f(e)
  res = []
  e.with_index.with_index do |(num, idx2), idx1|
    res << num
    res << idx1
    res << idx2
    if num > 1000
        break
    end
  end
  res
end

30.times { puts f(e).inspect }
