# frozen_string_literal: true
#
# Ripper for monoruby: the class, its event tables and the API surface, but
# no parser behind them.
#
# CRuby's Ripper is the parser's own event stream and lives in `ripper.so`;
# monoruby parses with prism and has nothing to feed those events from. What
# this file provides is enough for libraries that only *define* Ripper
# subclasses at load time to load. Rails does that in three places (`rails
# notes`, the test runner's failure parser and the fallback template
# dependency tracker) and requires `ripper` unconditionally, which used to
# stop the lobsters benchmark at boot. Actually parsing raises
# NotImplementedError, so a caller that reaches for the token or S-expression
# stream fails loudly rather than getting an empty answer.
#
# The event tables are those of Ruby 4.0.2 (`Ripper::PARSER_EVENT_TABLE`
# and `SCANNER_EVENT_TABLE`, name => arity); subclasses enumerate them to
# generate their `on_*` handlers.

class Ripper
  Version = "0.1.0"

  PARSER_EVENT_TABLE = {
    :BEGIN => 1, :END => 1, :alias => 2, :alias_error => 2,
    :aref => 2, :aref_field => 2, :arg_ambiguous => 1, :arg_paren => 1,
    :args_add => 2, :args_add_block => 2, :args_add_star => 2, :args_forward => 0,
    :args_new => 0, :array => 1, :aryptn => 4, :assign => 2,
    :assign_error => 2, :assoc_new => 2, :assoc_splat => 1, :assoclist_from_args => 1,
    :bare_assoc_hash => 1, :begin => 1, :binary => 3, :block_var => 2,
    :blockarg => 1, :bodystmt => 4, :brace_block => 2, :break => 1,
    :call => 3, :case => 2, :class => 3, :class_name_error => 2,
    :command => 2, :command_call => 4, :const_path_field => 2, :const_path_ref => 2,
    :const_ref => 1, :def => 3, :defined => 1, :defs => 5,
    :do_block => 2, :dot2 => 2, :dot3 => 2, :dyna_symbol => 1,
    :else => 1, :elsif => 3, :ensure => 1, :excessed_comma => 0,
    :fcall => 1, :field => 3, :fndptn => 4, :for => 3,
    :hash => 1, :heredoc_dedent => 2, :hshptn => 3, :if => 3,
    :if_mod => 2, :ifop => 3, :in => 3, :kwrest_param => 1,
    :lambda => 2, :magic_comment => 2, :massign => 2, :method_add_arg => 2,
    :method_add_block => 2, :mlhs_add => 2, :mlhs_add_post => 2, :mlhs_add_star => 2,
    :mlhs_new => 0, :mlhs_paren => 1, :module => 2, :mrhs_add => 2,
    :mrhs_add_star => 2, :mrhs_new => 0, :mrhs_new_from_args => 1, :next => 1,
    :nokw_param => 1, :opassign => 3, :operator_ambiguous => 2, :param_error => 2,
    :params => 7, :paren => 1, :parse_error => 1, :program => 1,
    :qsymbols_add => 2, :qsymbols_new => 0, :qwords_add => 2, :qwords_new => 0,
    :redo => 0, :regexp_add => 2, :regexp_literal => 2, :regexp_new => 0,
    :rescue => 4, :rescue_mod => 2, :rest_param => 1, :retry => 0,
    :return => 1, :return0 => 0, :sclass => 2, :stmts_add => 2,
    :stmts_new => 0, :string_add => 2, :string_concat => 2, :string_content => 0,
    :string_dvar => 1, :string_embexpr => 1, :string_literal => 1, :super => 1,
    :symbol => 1, :symbol_literal => 1, :symbols_add => 2, :symbols_new => 0,
    :top_const_field => 1, :top_const_ref => 1, :unary => 2, :undef => 1,
    :unless => 3, :unless_mod => 2, :until => 2, :until_mod => 2,
    :var_alias => 2, :var_field => 1, :var_ref => 1, :vcall => 1,
    :void_stmt => 0, :when => 3, :while => 2, :while_mod => 2,
    :word_add => 2, :word_new => 0, :words_add => 2, :words_new => 0,
    :xstring_add => 2, :xstring_literal => 1, :xstring_new => 0, :yield => 1,
    :yield0 => 0, :zsuper => 0,
  }.freeze

  SCANNER_EVENT_TABLE = {
    :CHAR => 1, :__end__ => 1, :backref => 1, :backtick => 1,
    :comma => 1, :comment => 1, :const => 1, :cvar => 1,
    :embdoc => 1, :embdoc_beg => 1, :embdoc_end => 1, :embexpr_beg => 1,
    :embexpr_end => 1, :embvar => 1, :float => 1, :gvar => 1,
    :heredoc_beg => 1, :heredoc_end => 1, :ident => 1, :ignored_nl => 1,
    :imaginary => 1, :int => 1, :ivar => 1, :kw => 1,
    :label => 1, :label_end => 1, :lbrace => 1, :lbracket => 1,
    :lparen => 1, :nl => 1, :op => 1, :period => 1,
    :qsymbols_beg => 1, :qwords_beg => 1, :rational => 1, :rbrace => 1,
    :rbracket => 1, :regexp_beg => 1, :regexp_end => 1, :rparen => 1,
    :semicolon => 1, :sp => 1, :symbeg => 1, :symbols_beg => 1,
    :tlambda => 1, :tlambeg => 1, :tstring_beg => 1, :tstring_content => 1,
    :tstring_end => 1, :words_beg => 1, :words_sep => 1, :ignored_sp => 1,
  }.freeze

  PARSER_EVENTS = PARSER_EVENT_TABLE.keys.freeze
  SCANNER_EVENTS = SCANNER_EVENT_TABLE.keys.freeze
  EVENTS = (PARSER_EVENTS + SCANNER_EVENTS).freeze

  # The default handlers, as CRuby: a parser event answers its arguments, a
  # scanner event its token. Subclasses `undef` and redefine them.
  PARSER_EVENTS.each do |event|
    define_method(:"on_#{event}") { |*args| args }
  end
  SCANNER_EVENTS.each do |event|
    define_method(:"on_#{event}") { |tok| tok }
  end

  NOT_AVAILABLE = "Ripper cannot parse on monoruby (there is no Ripper event source; use Prism)"

  attr_reader :filename, :lineno, :column, :error
  attr_accessor :yydebug

  def initialize(src, filename = "(ripper)", lineno = 1)
    @src = src.respond_to?(:gets) ? src.read : src.to_str
    @filename = filename
    @lineno = lineno
    @column = 0
    @error = nil
    @yydebug = false
  end

  def parse
    raise NotImplementedError, NOT_AVAILABLE
  end

  def error?
    false
  end

  def encoding
    @src.encoding
  end

  def state
    nil
  end

  def token
    nil
  end

  def end_seen?
    false
  end

  def debug_output
    nil
  end

  def debug_output=(_out)
  end

  def self.parse(src, filename = "(ripper)", lineno = 1)
    new(src, filename, lineno).parse
  end

  def self.lex(src, filename = "-", lineno = 1, **_kw)
    Lexer.new(src, filename, lineno).lex
  end

  def self.tokenize(src, filename = "-", lineno = 1, **_kw)
    Lexer.new(src, filename, lineno).tokenize
  end

  def self.sexp(src, filename = "-", lineno = 1, **_kw)
    SexpBuilderPP.new(src, filename, lineno).parse
  end

  def self.sexp_raw(src, filename = "-", lineno = 1, **_kw)
    SexpBuilder.new(src, filename, lineno).parse
  end

  def self.slice(_src, _pattern, _n = 0)
    raise NotImplementedError, NOT_AVAILABLE
  end

  def self.token_match(_src, _pattern)
    raise NotImplementedError, NOT_AVAILABLE
  end

  def self.lex_state_name(state)
    state.to_s
  end

  def self.dedent_string(_input, _width)
    raise NotImplementedError, NOT_AVAILABLE
  end

  class SexpBuilder < Ripper
  end

  class SexpBuilderPP < SexpBuilder
  end

  class Lexer < Ripper
    Elem = Struct.new(:pos, :event, :tok, :state, :message)
    State = Struct.new(:to_int, :to_s)

    def lex(**_kw)
      raise NotImplementedError, NOT_AVAILABLE
    end

    def tokenize(**_kw)
      lex.map(&:tok)
    end

    def scan(**_kw)
      lex
    end

    def parse(raise_errors: false)
      lex
    end
  end

  class Filter
    def initialize(src, filename = "-", lineno = 1)
      @__parser = Lexer.new(src, filename, lineno)
      @__line = nil
      @__col = nil
      @__state = nil
    end

    def filename
      @__parser.filename
    end

    def lineno
      @__line
    end

    def column
      @__col
    end

    def state
      @__state
    end

    def parse(_init = nil)
      @__parser.lex
    end
  end
end
