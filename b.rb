$x = []
    begin
        $x << 50
        1/0
        $x << 100
    rescue 3 => c
        $x << 150
    else
        $x << 200
    ensure
        $x << 250
        1/0
        $x << 300
    end

puts $x