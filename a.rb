def f(a,&blk)
  blk.call
end

blk = Proc.new do puts 100 end
a = 5
b = 100
f(a,&blk)
f(a) do puts 200 end