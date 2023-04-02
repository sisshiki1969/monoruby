def f(x,y,a:100,b:200,c:300,d:400)
    [a,b,c,d,x,y]
end

puts f(1,2,**{c:3,d:2},d:4,a:1)