# encoding: utf-8
puts [__ENCODING__, "\xff".encoding, "\xff".valid_encoding?, "あ".encoding, "\xe3\x81\x82".encoding, "\xe3\x81\x82" == "あ", /あ/.encoding, /a/n.encoding, /a/e.encoding, /a/s.encoding, /a/u.encoding, /a/n.fixed_encoding?, /\xff/n.encoding, "\xffあ".encoding, "\xffあ".valid_encoding?].inspect
