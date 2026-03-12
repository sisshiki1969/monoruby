# frozen_string_literal: true
#
# URI implementation for monoruby.
# Provides URI.parse, URI.encode_www_form, URI.decode_www_form
# and URI::Generic, URI::HTTP, URI::HTTPS classes.
#

module URI
  # Check if a byte is an unreserved URI character
  def self._unreserved_byte?(b)
    (b >= 65 && b <= 90) ||   # A-Z
    (b >= 97 && b <= 122) ||  # a-z
    (b >= 48 && b <= 57) ||   # 0-9
    b == 45 || b == 95 || b == 46 || b == 126  # - _ . ~
  end

  # Percent-encode a byte
  def self._encode_byte(b)
    if _unreserved_byte?(b)
      b.chr
    elsif b == 32  # space
      '+'
    else
      "%%%02X" % b
    end
  end

  class Error < StandardError; end
  class InvalidURIError < Error; end
  class InvalidComponentError < Error; end
  class BadURIError < Error; end

  # RFC 3986 generic URI
  class Generic
    attr_accessor :scheme, :userinfo, :host, :port, :path, :query, :fragment

    DEFAULT_PORT = nil

    def self.default_port
      self::DEFAULT_PORT
    end

    def self.build(args)
      if args.is_a?(Array)
        scheme, userinfo, host, port, registry, path, opaque, query, fragment = args
      elsif args.is_a?(Hash)
        scheme = args[:scheme]
        userinfo = args[:userinfo]
        host = args[:host]
        port = args[:port]
        path = args[:path]
        query = args[:query]
        fragment = args[:fragment]
      end
      uri = new
      uri.scheme = scheme
      uri.userinfo = userinfo
      uri.host = host
      uri.port = port
      uri.path = path || ''
      uri.query = query
      uri.fragment = fragment
      uri
    end

    def initialize
      @scheme = nil
      @userinfo = nil
      @host = nil
      @port = nil
      @path = ''
      @query = nil
      @fragment = nil
    end

    def user
      @userinfo ? @userinfo.split(':', 2)[0] : nil
    end

    def password
      @userinfo ? @userinfo.split(':', 2)[1] : nil
    end

    def hostname
      @host
    end

    def hostname=(v)
      @host = v
    end

    def default_port
      self.class.default_port
    end

    def absolute?
      !@scheme.nil?
    end

    def relative?
      @scheme.nil?
    end

    def opaque
      nil
    end

    def hierarchical?
      !opaque
    end

    def request_uri
      return nil unless @path
      result = @path.empty? ? '/' : @path
      result = "#{result}?#{@query}" if @query
      result
    end

    def to_s
      str = ''
      str << "#{@scheme}:" if @scheme
      if @host
        str << '//'
        str << "#{@userinfo}@" if @userinfo
        str << @host
        str << ":#{@port}" if @port && @port != default_port
      end
      str << @path
      str << "?#{@query}" if @query
      str << "##{@fragment}" if @fragment
      str
    end

    def inspect
      "#<#{self.class} #{to_s}>"
    end

    def ==(other)
      return false unless other.is_a?(URI::Generic)
      to_s == other.to_s
    end

    def dup
      uri = self.class.new
      uri.scheme = @scheme
      uri.userinfo = @userinfo
      uri.host = @host
      uri.port = @port
      uri.path = @path.dup
      uri.query = @query ? @query.dup : nil
      uri.fragment = @fragment ? @fragment.dup : nil
      uri
    end

    def merge(other)
      other = URI.parse(other) if other.is_a?(String)
      result = dup
      if other.scheme
        result.scheme = other.scheme
        result.host = other.host
        result.port = other.port
        result.path = other.path
        result.query = other.query
      else
        if other.host
          result.host = other.host
          result.port = other.port
          result.path = other.path
          result.query = other.query
        else
          if other.path.empty?
            result.query = other.query if other.query
          else
            if other.path[0] == '/'
              result.path = other.path
            else
              # Merge relative path
              base = result.path.sub(%r{[^/]*$}, '')
              result.path = base + other.path
            end
            result.query = other.query
          end
        end
      end
      result.fragment = other.fragment
      result
    end
    alias + merge
  end

  class HTTP < Generic
    DEFAULT_PORT = 80

    def self.build(args)
      uri = super
      uri.scheme ||= 'http'
      uri
    end
  end

  class HTTPS < HTTP
    DEFAULT_PORT = 443

    def self.build(args)
      uri = super
      uri.scheme = 'https'
      uri
    end
  end

  class FTP < Generic
    DEFAULT_PORT = 21
  end

  class File < Generic
    DEFAULT_PORT = nil
  end

  # Scheme registry
  @@schemes = {
    'HTTP'  => HTTP,
    'HTTPS' => HTTPS,
    'FTP'   => FTP,
    'FILE'  => URI::File,
  }

  def self.scheme_list
    @@schemes
  end

  def self.register_scheme(scheme, klass)
    @@schemes[scheme.upcase] = klass
  end

  # --- Parsing ---

  # Simple URI parser based on RFC 3986 regex
  URI_REGEXP = %r{\A
    (?:([a-zA-Z][a-zA-Z0-9+\-.]*)://)?   # scheme
    (?:([^@/\#?]*)@)?                      # userinfo
    ([^/:?\#]*)?                           # host
    (?::(\d+))?                            # port
    ([^?\#]*)                              # path
    (?:\?([^\#]*))?                        # query
    (?:\#(.*))?                            # fragment
  \z}x

  def self.parse(uri_str)
    return uri_str if uri_str.is_a?(URI::Generic)
    uri_str = uri_str.to_s

    m = URI_REGEXP.match(uri_str)
    unless m
      raise InvalidURIError, "bad URI(is not URI?): #{uri_str}"
    end

    scheme = m[1]
    userinfo = m[2]
    host = m[3]
    port = m[4] ? m[4].to_i : nil
    path = m[5] || ''
    query = m[6]
    fragment = m[7]

    klass = if scheme
              @@schemes[scheme.upcase] || Generic
            else
              Generic
            end

    uri = klass.new
    uri.scheme = scheme ? scheme.downcase : nil
    uri.userinfo = userinfo
    uri.host = host
    uri.port = port
    uri.path = path
    uri.query = query
    uri.fragment = fragment
    uri
  end

  def self.split(uri_str)
    m = URI_REGEXP.match(uri_str.to_s)
    unless m
      raise InvalidURIError, "bad URI(is not URI?): #{uri_str}"
    end
    [m[1], m[2], m[3], m[4]&.to_i, nil, m[5], nil, m[6], m[7]]
  end

  # --- Encoding ---

  def self.encode_www_form_component(str, enc = nil)
    str = str.to_s
    str.bytes.map { |b| _encode_byte(b) }.join
  end

  def self.decode_www_form_component(str, enc = nil)
    str.gsub(/\+/, ' ').gsub(/%([0-9A-Fa-f]{2})/) do
      $1.to_i(16).chr
    end
  end

  def self.encode_www_form(enum)
    enum.map do |k, v|
      if v.is_a?(Array)
        v.map { |vv| "#{encode_www_form_component(k)}=#{encode_www_form_component(vv)}" }.join('&')
      else
        "#{encode_www_form_component(k)}=#{encode_www_form_component(v)}"
      end
    end.join('&')
  end

  def self.decode_www_form(str, enc = nil)
    str.split('&').map do |pair|
      k, v = pair.split('=', 2)
      [decode_www_form_component(k || ''), decode_www_form_component(v || '')]
    end
  end

  # Convenience
  def self.join(*strs)
    base = parse(strs.shift)
    strs.each { |s| base = base.merge(s) }
    base
  end

  def self.extract(str, schemes = nil)
    result = []
    str.scan(%r{[a-zA-Z][a-zA-Z0-9+\-.]*://[^\s<>\]]+}) do |uri|
      if schemes.nil? || schemes.any? { |s| uri.downcase.start_with?(s.downcase + '://') }
        result << uri
      end
    end
    result
  end
end
