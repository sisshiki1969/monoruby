100000.times {
  a = [*(0..1000)]
  a.each_slice(20) do |slice|
    # puts slice.sum
  end
}