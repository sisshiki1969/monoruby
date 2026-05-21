# frozen_string_literal: true
#
# Digest implementation for monoruby.
#
# The hashing itself is performed natively by `String.__digest`
# (see src/builtins/digest.rs). Each algorithm class buffers the input
# it receives and finalizes by calling that one-shot helper — hashing the
# concatenation of all updates is equivalent to streaming, so no native
# per-instance state is needed.

module Digest
  VERSION = '3.2.0'

  # Autoload algorithm classes on first reference, mirroring CRuby:
  #   Digest::SHA256 / SHA384 / SHA512  -> digest/sha2
  #   Digest::MD5 / SHA1 / ...          -> digest/<name>
  def self.const_missing(name)
    case name
    when :SHA256, :SHA384, :SHA512
      require 'digest/sha2'
    else
      require "digest/#{name.to_s.downcase}"
    end
    if const_defined?(name, false)
      const_get(name, false)
    else
      raise NameError, "uninitialized constant Digest::#{name}"
    end
  end

  # Class-level (one-shot) API shared by every algorithm.
  class Class
    def self.digest(str, *args)
      new(*args).update(str).digest!
    end

    def self.hexdigest(str, *args)
      new(*args).update(str).hexdigest!
    end

    def self.base64digest(str, *args)
      [digest(str, *args)].pack('m0')
    end

    def self.file(name, *args)
      new(*args).file(name)
    end
  end

  # Instance-level API. Subclasses set ALGORITHM / DIGEST_LENGTH /
  # BLOCK_LENGTH (or override the corresponding methods).
  module Instance
    def update(str)
      @buffer << str.b
      self
    end
    alias << update

    def reset
      @buffer = ''.b
      self
    end

    # The raw digest of the accumulated data, computed natively.
    def finish
      String.__digest(self.class::ALGORITHM, @buffer)
    end
    private :finish

    def digest(str = nil)
      return finish if str.nil?
      reset
      update(str)
      value = finish
      reset
      value
    end

    def digest!
      value = finish
      reset
      value
    end

    def hexdigest(str = nil)
      digest(str).unpack1('H*')
    end
    alias to_s hexdigest

    def hexdigest!
      digest!.unpack1('H*')
    end

    def base64digest(str = nil)
      [digest(str)].pack('m0')
    end

    def base64digest!
      [digest!].pack('m0')
    end

    def file(name)
      update(File.binread(name))
    end

    def digest_length
      self.class::DIGEST_LENGTH
    end
    alias length digest_length
    alias size digest_length

    def block_length
      self.class::BLOCK_LENGTH
    end

    def ==(other)
      case other
      when Digest::Instance
        digest == other.digest
      when String
        to_s == other
      else
        false
      end
    end

    def inspect
      "#<#{self.class.name}: #{hexdigest}>"
    end
  end

  # Base class for the buffered algorithm implementations.
  class Base < Digest::Class
    include Instance

    def initialize
      @buffer = ''.b
    end

    def initialize_copy(other)
      @buffer = other.instance_variable_get(:@buffer).dup
    end
  end
end
