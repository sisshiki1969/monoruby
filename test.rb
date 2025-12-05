def a
  1.times do
    # block1 in a
    yield
  end
end

60.times do
  # block2 in main
  a do
    # block3 in main
    puts 100
  end
end

#
#   +---------------------------------+
#   |       +---------------+         |
#   v       v               |         |
# block2 -> a -> times -> block1 -> block3
#
#
