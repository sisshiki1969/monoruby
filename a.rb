p = []
o = Object.new
def o.each
  yield
  yield 1
  yield 1, 2
  yield nil
  yield [1, 2]
end
e = o.to_enum
p << e.next_values
p << e.next_values
p << e.next_values
p << e.next_values
p << e.next_values
e = o.to_enum
p << e.next
p << e.next
p << e.next
p << e.next
p << e.next
p