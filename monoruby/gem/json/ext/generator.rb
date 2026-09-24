# frozen_string_literal: true
#
# monoruby's stand-in for the json gem's generator.so (json 2.18.0, the
# version CRuby 4.0.6 ships): it defines what the C extension's
# Init_generator does, JSON::Ext::Generator::State and the
# GeneratorMethods modules JSON.generator= mixes into the core classes,
# with the methods implemented natively in src/builtins/json.rs as a
# port of ext/json/ext/generator/generator.c.

require 'json/common'

module JSON
  module Ext
    module Generator
      class State
      end

      module GeneratorMethods
        %i[Object Hash Array Integer Float String TrueClass FalseClass NilClass].each do |name|
          const_set(name, ::Module.new)
        end
      end
    end
  end
end

::String.__json_setup_generator(JSON::Ext::Generator::State, JSON::Ext::Generator::GeneratorMethods)

require 'json/ext/generator/state'
