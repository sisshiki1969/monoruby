    text = "Hello\nこんにちは\nWorld\n世界\n"
    res = []
    text.each_line("\n", chomp:true) do |line|
        res << line
    end
    p res