# encoding: EUC-JP
puts [__ENCODING__, "abc".encoding, "日本".encoding, "日本".length, "\xC6\xFC".encoding, :abc.encoding, :"日".encoding, ?a.encoding, ?日.encoding, %w[a 日].map(&:encoding), <<~E.encoding, "a".force_encoding("UTF-8").encoding, "日".inspect, "日".inspect.encoding].inspect
  日
E
