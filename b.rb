res = []
for i in 0..10
    begin
        x = 50.0 + i
        1/0
        x = 100.0
    rescue => c
        res << x
        x = 150.0
    else
        x = 200.0
    ensure
        x = 300.0
    end
end
res << x
puts res