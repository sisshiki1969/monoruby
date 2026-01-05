def foo
  $b.call
end

35.times {
  a = :a
  $b = proc { a = 1 }
  a = nil
  foo
  puts a
}

