
def foo
  2.times do |i|
    2.times  do |j|
      $x << [i,j]
      return 3 if i == 1 && j == 0
    ensure
      $x << ["j",j]
    end
  ensure
    $x << ["i",i]
  end
ensure
  $x << "foo"
end

$x = []
puts "#{[foo, $x]}"