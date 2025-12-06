def a(x)
  1.times do
    # block1 in a
    if x.odd?
      return yield(7)
    else
      return yield(3)
    end
  end
end

25.times do |x|
  # block2 in main
  puts (a(x) do |x|
    # block3 in main
    if 3 + x == 10
      100
    else
      200
    end
  end)
end

#
#   +---------------------------------+
#   |       +---------------+         |
#   v       v               |         |
# block2 -> a -> times -> block1 -> block3
#
#
