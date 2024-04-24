h = Hash.new do |hash, key|
  hash[key] = "foo"
  "bar"
end

puts h[:a] # => "bar"
puts h[:a] # => "foo"