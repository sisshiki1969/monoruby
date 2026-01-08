res = []
1000.times do
  5.times do |a|
      5.times do |b|
          eval "res << a; res << b"
      end
  end
end