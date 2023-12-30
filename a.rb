def f(a,b,&block)
  block.call(a,b)
end

10.times {
  f(1,2) do |a,b| puts(a + b) end
}