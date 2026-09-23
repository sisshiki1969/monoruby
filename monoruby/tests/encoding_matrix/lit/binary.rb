# encoding: binary
puts [__ENCODING__, "abc".encoding, "ÿ".encoding, :"ÿ".encoding].inspect
