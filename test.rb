def test(c)
  puts "enter #{c}: #{100 * 100}"
  if c == 0
    return
  else
    test(c-1)
    puts "exit #{c}: #{100 * 100}"
    if c == 25
      Integer.class_eval do
        def *(other)
          42
        end
      end
    end
  end
end

test(40)
