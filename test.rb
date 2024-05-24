def get_binding(str)
  binding
end
str = "hello"
p eval("str + ' Fred'")                      #=> "hello Fred"
p eval("str + ' Fred'", get_binding("bye"))   #=> "bye Fred"
