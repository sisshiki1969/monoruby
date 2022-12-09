def f
  yield 1,2,3,4
end

8.times { f {|a,b,c,d| e=42; puts a,b,c,d,e} }
