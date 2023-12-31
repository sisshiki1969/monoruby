def f(a,b,&block)
  block.call
end

15.times {
  puts f(1,2){3}
}