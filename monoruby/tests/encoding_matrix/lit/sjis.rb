# -*- coding: shift_jis -*-
puts [__ENCODING__, "日本".encoding, "日本".length, "日本".bytes, "\x93\xfa".valid_encoding?, :"日".encoding, "表\".length, "表\".bytes].inspect
