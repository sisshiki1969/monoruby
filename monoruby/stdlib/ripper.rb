# frozen_string_literal: true
#
# Ripper for monoruby: prism's Ripper translation layer.
#
# CRuby's Ripper is the parser's own event stream and lives in `ripper.so`;
# monoruby parses with prism. The prism gem ships
# `Prism::Translation::Ripper` (pure Ruby): it parses the source with
# `Prism.parse` (see `stdlib/prism/prism.rb`) and walks the tree
# dispatching the Ripper events, with the same class-level API
# (`Ripper.parse` / `lex` / `sexp` / `sexp_raw` / `lex_state_name`), the
# `PARSER_EVENTS` / `SCANNER_EVENTS` tables, `SexpBuilder` /
# `SexpBuilderPP` and `Ripper::Lexer`. This is what the gem's own
# `prism/translation/ripper/shim` does.
#
# It is a translation, not the parser's event stream: a few events are
# not dispatched (`on_sp`, `on_nl`, `on_op`, `on_kw`, the delimiters; see
# the header of `prism/translation/ripper.rb`), so a subclass that counts
# on those sees fewer callbacks than under CRuby, and `#lineno` /
# `#column` answer the initial position rather than nil before `#parse`.

require "prism"
require "prism/translation/ripper"
require "prism/translation/ripper/sexp"
require "prism/translation/ripper/lexer"

Ripper = Prism::Translation::Ripper
