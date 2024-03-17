    for i in 0..20
      begin
        puts 50/0
      rescue => c
        puts c
      end
    end