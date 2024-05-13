a = 1
b = 2
def f(x)
  a = 100
  b = 100
  binding
end
puts f(42).local_variables