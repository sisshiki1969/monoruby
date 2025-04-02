def foo(&block)
  a = 1
  loop do
    {a:1,b:2}.each &block
    a += 1
    if a > 50
      break
    end
  end
end

E=100
a=77
100.times do
foo do |k,v|
  __dump
  exit
  puts "Hello key=#{k} value=#{v} a=#{a} E=#{E}"
end
end