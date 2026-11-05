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

class Prism::Translation::Ripper
  # CRuby's Ripper reports parse.y's wording, which differs from prism's
  # for some diagnostics; `on_parse_error` receives the parse.y text for
  # the error types listed here (keyed by prism's error type).
  PARSE_Y_ERROR_MESSAGES = {
    def_endless_setter: "setter method cannot be defined in an endless method definition",
  }.freeze

  module ParseYErrorMessages # :nodoc:
    def result
      @result ||= begin
        result = super
        result.errors.map! do |error|
          message = PARSE_Y_ERROR_MESSAGES[error.type]
          next error unless message
          Prism::ParseError.new(error.type, message, error.location, error.level)
        end
        result
      end
    end
    private :result
  end
  prepend ParseYErrorMessages
end
