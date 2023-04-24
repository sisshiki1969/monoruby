$res = []
def f
  3.times { |x|
      $res << x
      return x if x == 2
  }
  $res << 20
  nil
end

puts "result: #{f} #{$res}"