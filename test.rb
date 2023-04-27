$res = []
def f
  10.times { |x|
      $res << x
      return x if x == 8
  }
  $res << 20
  nil
end

puts "result: #{f} #{$res}"