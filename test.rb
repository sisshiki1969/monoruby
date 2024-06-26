        flag = false
        500.times do |x|
          puts x
          if x == 3 && !flag
            flag = true
            redo
          end
        end