        res = []
        begin
            for i in 0..10
                x = 50.0 + 50/(9-i)
                res << x
            end
        rescue => c
            puts c
            x = 150.0
        else
            x = 200.0
        ensure
            x = 300.0
        end
        res << x
        puts res