ENCS = ["UTF-8","US-ASCII","EUC-JP","Shift_JIS","Windows-31J","ISO-2022-JP","stateless-ISO-2022-JP",
        "UTF-16BE","ISO-8859-1","CP949","Big5","GB18030","EUC-KR","UTF8-MAC","CESU-8","EUC-TW"]
SAMPLES = {
  "UTF-8" => ["A\x80B", "€", "é", "一", "\u{1f600}"],
  "US-ASCII" => ["A\x80B", "abc"],
  "EUC-JP" => ["A\x80B", "A\xa4\xa2B", "A\x8f\xa2\xafB"],
  "Shift_JIS" => ["A\x80B", "A\x82\xa0B", "A\xb1B"],
  "Windows-31J" => ["A\x80B", "A\x82\xa0B"],
  "ISO-2022-JP" => ["A\x80B", "\e$B0l\e(B", "\e$@0l\e(B", "\e$I!\x21"],
  "stateless-ISO-2022-JP" => ["A\x80B", "A\x92\xa4\xa2B", "A\x90\xa4\xa2B", "A\x91\xa4\xa2B"],
  "UTF-16BE" => ["\x00A\xd8\x00\x00B", "\x30\x42"],
  "ISO-8859-1" => ["A\xe9B"],
  "CP949" => ["A\x80B", "A\xb0\xa1B"],
  "Big5" => ["A\x80B", "A\xa4\x40B"],
  "GB18030" => ["A\x80B", "A\xb0\xa1B"],
  "EUC-KR" => ["A\x80B", "A\xb0\xa1B"],
  "UTF8-MAC" => ["A\x80B", "一"],
  "CESU-8" => ["A\x80B", "一"],
  "EUC-TW" => ["A\x80B", "A\xc4\xa1B"],
}
ENCS.each do |s|
  ENCS.each do |d|
    next if s == d
    (SAMPLES[s] || []).each do |bytes|
      ec = begin; Encoding::Converter.new(s, d); rescue; next; end
      src = bytes.b.dup.force_encoding(s); dst = +""
      r = begin; ec.primitive_convert(src, dst); rescue => e; e.class.to_s; end
      next if r == :finished || r == :source_buffer_empty
      ei = begin; ec.primitive_errinfo; rescue => e; [e.class.to_s]; end
      puts "%s -> %s\t%s\t%s\t%s\t%s\t%s" % [s, d, bytes.b.unpack1('H*'), r,
        dst.b.unpack1('H*'), src.b.unpack1('H*'),
        ei.map { |x| x.is_a?(String) ? x.b.unpack1('H*') : x.inspect }.inspect]
    end
  end
end
