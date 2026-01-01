b = c = d = 0x12345678
40.times do |i|
  case i
  when  0..19
    f = ((b & c) | (~b & d))
    k = 0x5A827999
  when 20..39
    f = (b ^ c ^ d)
    k = 0x6ED9EBA1
  when 40..59
    f = ((b & c) | (b & d) | (c & d))
    k = 0x8F1BBCDC
  when 60..79
    f = (b ^ c ^ d)
    k = 0xCA62C1D6
  end
  puts "#{f} #{k}"
end