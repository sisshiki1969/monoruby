list = Encoding.list.map(&:to_s).sort
have = {}
list.each { |a| list.each { |b| next if a == b; have[[a, b]] = (Encoding::Converter.search_convpath(a, b); true) rescue false } }
puts "pairs=#{have.count { |_, v| v }}"
have.each { |(a, b), v| puts "#{a}\t#{b}\t#{v}" }
