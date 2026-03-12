# frozen_string_literal: true
#
# Time extensions for monoruby.
# Provides Time.parse and related methods.
# This mirrors CRuby's time.rb stdlib extension.
#

require 'date'

class Time
  # Mapping of month abbreviations
  MonthValue = {
    'JAN' => 1, 'FEB' => 2, 'MAR' => 3, 'APR' => 4,
    'MAY' => 5, 'JUN' => 6, 'JUL' => 7, 'AUG' => 8,
    'SEP' => 9, 'OCT' => 10, 'NOV' => 11, 'DEC' => 12
  }

  # Time.parse - parse a time string
  def self.parse(str, now = nil)
    now ||= Time.now

    year = nil
    month = nil
    day = nil
    hour = nil
    min = nil
    sec = nil

    # Try ISO 8601: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS
    if str =~ /(\d{4})-(\d{1,2})-(\d{1,2})[T ](\d{1,2}):(\d{2}):(\d{2})/
      year = $1.to_i
      month = $2.to_i
      day = $3.to_i
      hour = $4.to_i
      min = $5.to_i
      sec = $6.to_i
    # Try YYYY-MM-DD
    elsif str =~ /(\d{4})-(\d{1,2})-(\d{1,2})/
      year = $1.to_i
      month = $2.to_i
      day = $3.to_i
    # Try HH:MM:SS only
    elsif str =~ /\A(\d{1,2}):(\d{2}):(\d{2})\z/
      year = now.year
      month = now.month
      day = now.day
      hour = $1.to_i
      min = $2.to_i
      sec = $3.to_i
    # Try HH:MM only
    elsif str =~ /\A(\d{1,2}):(\d{2})\z/
      year = now.year
      month = now.month
      day = now.day
      hour = $1.to_i
      min = $2.to_i
      sec = 0
    # Try RFC 2822: DD Mon YYYY HH:MM:SS
    elsif str =~ /(\d{1,2})\s+(\w{3})\s+(\d{4})\s+(\d{1,2}):(\d{2}):(\d{2})/
      day = $1.to_i
      month = MonthValue[$2.upcase] || raise(ArgumentError, "invalid date: #{str}")
      year = $3.to_i
      hour = $4.to_i
      min = $5.to_i
      sec = $6.to_i
    # Try Mon DD, YYYY
    elsif str =~ /(\w{3})\s+(\d{1,2}),?\s+(\d{4})/
      month = MonthValue[$1.upcase] || raise(ArgumentError, "invalid date: #{str}")
      day = $2.to_i
      year = $3.to_i
    # Try YYYY/MM/DD
    elsif str =~ /(\d{4})\/(\d{1,2})\/(\d{1,2})/
      year = $1.to_i
      month = $2.to_i
      day = $3.to_i
    else
      raise ArgumentError, "no time information in #{str.inspect}"
    end

    year  ||= now.year
    month ||= now.month
    day   ||= now.day
    hour  ||= 0
    min   ||= 0
    sec   ||= 0

    Time.local(year, month, day, hour, min, sec)
  end

  # Time.iso8601 / Time.xmlschema
  def self.iso8601(str)
    parse(str)
  end
  class << self
    alias xmlschema iso8601
  end

  # Time.rfc2822 / Time.rfc822
  def self.rfc2822(str)
    parse(str)
  end
  class << self
    alias rfc822 rfc2822
  end

  # Time.httpdate
  def self.httpdate(str)
    parse(str)
  end

  # Instance methods

  def iso8601(fraction_digits = 0)
    if fraction_digits > 0
      strftime("%Y-%m-%dT%H:%M:%S.%#{fraction_digits}N%:z")
    else
      strftime('%Y-%m-%dT%H:%M:%S%:z')
    end
  end
  alias xmlschema iso8601

  def rfc2822
    strftime('%a, %d %b %Y %H:%M:%S %z')
  end
  alias rfc822 rfc2822

  def httpdate
    strftime('%a, %d %b %Y %H:%M:%S GMT')
  end
end
