# Minimal CGI::Escape stub for monoruby.
#
# The real cgi/escape is a C extension. ActiveSupport uses it for
# URL-encoding query parameters. Provide pure-Ruby fallbacks.

module CGI
  module Escape
    def escapeHTML(string)
      string.to_s
        .gsub("&", "&amp;")
        .gsub("<", "&lt;")
        .gsub(">", "&gt;")
        .gsub('"', "&quot;")
        .gsub("'", "&#39;")
    end
    alias escape_html escapeHTML

    def unescapeHTML(string)
      string.to_s
        .gsub("&amp;", "&")
        .gsub("&lt;", "<")
        .gsub("&gt;", ">")
        .gsub("&quot;", '"')
        .gsub("&#39;", "'")
    end
    alias unescape_html unescapeHTML

    def escape(string)
      string.to_s.gsub(/([^a-zA-Z0-9_.\-~])/) do |m|
        "%" + m.unpack("H2" * m.bytesize).join("%").upcase
      end
    end

    def unescape(string)
      string.to_s.gsub(/%([0-9a-fA-F]{2})/) { [$1].pack("H*") }
    end

    def escapeURIComponent(string)
      escape(string.to_s)
    end
    alias escape_uri_component escapeURIComponent

    def unescapeURIComponent(string)
      unescape(string.to_s)
    end
    alias unescape_uri_component unescapeURIComponent
  end

  extend Escape
end
