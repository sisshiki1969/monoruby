# frozen_string_literal: true
#
# monoruby's stand-in for the json gem's parser.so (json 2.18.0, the
# version CRuby 4.0.6 ships): it defines what the C extension's
# Init_parser does, JSON::Ext::ParserConfig (#initialize, #parse) and
# JSON::Ext::Parser.parse, with the methods implemented natively in
# src/builtins/json.rs as a port of ext/json/ext/parser/parser.c.

require 'json/common'

module JSON
  module Ext
    class ParserConfig
    end

    class Parser
    end
  end
end

::String.__json_setup_parser(JSON::Ext::ParserConfig, JSON::Ext::Parser)
