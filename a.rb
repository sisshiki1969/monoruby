# 実位置引数の数が仮位置引数より１個少ない
# 実引数に１個以上のキーワード引数がある
# calleeの仮引数にキーワード引数がない

def f(a, b)
  "a:#{a} b:#{b}"
end

10.times do
  puts f("a", x:100, y:200)
end
#def f
#  yield 1,2,3,x:100,y:200
#end
#
#f do |a,b,*c| puts "a:#{a} b:#{b} c:#{c}" end