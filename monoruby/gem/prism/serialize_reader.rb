# frozen_string_literal: true
#
# monoruby's reader for `Prism::Serialize::Loader`.
#
# The loader reads the serialized tree one byte at a time through
# `FastStringIO`, which the gem defines as a small pure-Ruby class on
# TruffleRuby and as `::StringIO` everywhere else. On monoruby `StringIO`
# is CRuby's C extension behind the C API layer, so each `getbyte` is a
# crossing into C and back (~680 instructions, 600K of them for a
# ruby-lsp indexing pass), where `String#getbyte` on an instance variable
# is a handful of JIT-compiled instructions.
#
# `prism/prism.rb` (the stand-in for the gem's C extension) re-registers
# `Prism::Serialize`'s autoload to this file, which loads the gem's own
# `prism/serialize` and then swaps the reader. A program that requires
# `prism/serialize` by itself keeps `StringIO`, which is just as correct.

require "prism/serialize"

module Prism
  module Serialize
    # The same class the gem defines under `RUBY_ENGINE == "truffleruby"`.
    class ByteReader # :nodoc:
      attr_accessor :pos

      def initialize(string)
        @string = string
        @pos = 0
      end

      def getbyte
        byte = @string.getbyte(@pos)
        @pos += 1
        byte
      end

      def read(n)
        slice = @string.byteslice(@pos, n)
        @pos += n
        slice
      end

      def eof?
        @pos >= @string.bytesize
      end
    end

    remove_const(:FastStringIO)
    FastStringIO = ByteReader # :nodoc:
    private_constant :FastStringIO, :ByteReader
  end
end
