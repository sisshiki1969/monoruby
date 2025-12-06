def a
  50.times do
    # block1 in a
    yield
  end
end

50.times do
  # block2 in main
  a do
    # block3 in main
    100
  end
end

#
#   +---------------------------------+
#   |       +---------------+         |
#   v       v               |         |
# block2 -> a -> times -> block1 -> block3
#
#
