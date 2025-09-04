22.times {        
  ary = [0, 4, 7, 10, 12]
  puts (0...ary.size).bsearch {|i| __dump; ary[i] >= 4 } # => 1
}