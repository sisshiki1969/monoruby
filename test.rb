def f
  z = 1.0
  if $a
    a = 5.0
    b = 7.5
    c = 100
  else
    z = $b * z * z
    b = 2.0
    a = 8.0
  end
  "#{a * b}#{c.inspect}"
end

$a = false
$b = 1.0
50.times do
  p f
end