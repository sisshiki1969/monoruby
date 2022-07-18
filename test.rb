def f
  1
end
a = 0
i = 0
while i < 1000000
  a = a + f()
  if i == 500
    def f
      0
    end
  end
  i = i + 1
end
a 