# frozen_string_literal: true
#
# Stub for the `cgi/escape` C extension.
#
# In CRuby, `cgi/escape` is a C extension that defines fast versions of
# `CGI.escape`, `CGI.unescape`, `CGI.escapeHTML`, `CGI.unescapeHTML`,
# `CGI.escapeURIComponent`, and `CGI.unescapeURIComponent`. The methods
# are defined directly on the `CGI` class.
#
# Starting with Ruby 3.5 the `cgi` library was extracted from the
# default stdlib, so `cgi/util.rb` may not be on the load path even
# when CRuby is. To stay compatible with both old and new Rubies,
# define the surface area in pure Ruby here without relying on
# `cgi/util.rb`.

class CGI; end unless defined?(::CGI)

class CGI
  TABLE_FOR_ESCAPE_HTML__ = {
    "'" => '&#39;',
    '&' => '&amp;',
    '"' => '&quot;',
    '<' => '&lt;',
    '>' => '&gt;',
  } unless defined?(TABLE_FOR_ESCAPE_HTML__)

  unless respond_to?(:escape)
    def self.escape(string)
      encoding = string.encoding
      buffer = string.b
      buffer.gsub!(/([^ a-zA-Z0-9_.\-~]+)/) do |m|
        '%' + m.unpack('H2' * m.bytesize).join('%').upcase
      end
      buffer.tr!(' ', '+')
      buffer.force_encoding(encoding)
    end
  end

  unless respond_to?(:unescape)
    def self.unescape(string, encoding = Encoding::UTF_8)
      str = string.tr('+', ' ')
      str = str.b
      str.gsub!(/((?:%[0-9a-fA-F]{2})+)/) do |m|
        [m.delete('%')].pack('H*')
      end
      str.force_encoding(encoding)
      str.valid_encoding? ? str : str.force_encoding(string.encoding)
    end
  end

  unless respond_to?(:escapeURIComponent)
    def self.escapeURIComponent(string)
      encoding = string.encoding
      buffer = string.b
      buffer.gsub!(/([^a-zA-Z0-9_.\-~]+)/) do |m|
        '%' + m.unpack('H2' * m.bytesize).join('%').upcase
      end
      buffer.force_encoding(encoding)
    end
    class << self
      alias escape_uri_component escapeURIComponent
    end
  end

  unless respond_to?(:unescapeURIComponent)
    def self.unescapeURIComponent(string, encoding = Encoding::UTF_8)
      str = string.b
      str.gsub!(/((?:%[0-9a-fA-F]{2})+)/) do |m|
        [m.delete('%')].pack('H*')
      end
      str.force_encoding(encoding)
      str.valid_encoding? ? str : str.force_encoding(string.encoding)
    end
    class << self
      alias unescape_uri_component unescapeURIComponent
    end
  end

  unless respond_to?(:escapeHTML)
    def self.escapeHTML(string)
      string.gsub(/['&\"<>]/, TABLE_FOR_ESCAPE_HTML__)
    end
    class << self
      alias escape_html escapeHTML
      alias h escapeHTML
    end
  end

  unless respond_to?(:unescapeHTML)
    def self.unescapeHTML(string)
      return string unless string.include?('&')
      string.gsub(/&(apos|amp|quot|gt|lt|#[0-9]+|#x[0-9A-Fa-f]+);/) do
        case $1
        when 'apos'                then "'"
        when 'amp'                 then '&'
        when 'quot'                then '"'
        when 'gt'                  then '>'
        when 'lt'                  then '<'
        when /\A#0*(\d+)\z/        then [$1.to_i].pack('U')
        when /\A#x([0-9a-f]+)\z/i  then [$1.to_i(16)].pack('U')
        end
      end
    end
    class << self
      alias unescape_html unescapeHTML
    end
  end
end
