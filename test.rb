def g
  begin
    yield
  ensure
    puts "ensure in g"
  end
end

def f
  g do
    begin
      break 42
    ensure
      puts "ensure in f"
    end
  end
end

puts f