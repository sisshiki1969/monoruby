b=1
b.times do
  a = 100
  puts <<-RUBY
    #{a}
  RUBY
end