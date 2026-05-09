# frozen_string_literal: true
#
# `yaml` stub for monoruby.
#
# CRuby's `yaml.rb` requires `psych` and then sets `YAML = Psych`. Our
# psych stub already defines the `YAML` alias, so we just delegate to
# it without re-assigning the constant (which would trigger an
# "already initialized constant" warning).

require "psych"
