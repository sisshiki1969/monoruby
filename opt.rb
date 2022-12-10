def f
  yield [1,2]
end

10.times {
  f { |a,b,c=42|
    puts [a,b,c]
  }
}
