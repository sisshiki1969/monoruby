# Minimal Date/DateTime stub for monoruby
# Provides just enough functionality for sequel benchmark

class Date
  ITALY = 2299161
  ENGLAND = 2361222
  JULIAN = Float::INFINITY
  GREGORIAN = -Float::INFINITY

  # Raised by the parsers for a date that does not exist (or a
  # `strptime` mismatch); an ArgumentError, as in date_core.
  class Error < ArgumentError; end

  MONTHNAMES = [nil, "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"]
  DAYNAMES = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
  ABBR_MONTHNAMES = [nil, "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  ABBR_DAYNAMES = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

  attr_reader :year, :month, :day

  def initialize(year = -4712, month = 1, day = 1, _sg = ITALY)
    @sg = _sg
    @year = year
    @month = month
    @day = day
  end

  def self.civil(year = -4712, month = 1, day = 1, sg = ITALY)
    new(year, month, day, sg)
  end

  class << self
    alias_method :new!, :civil
  end

  def self.today
    t = Time.now
    civil(t.year, t.month, t.day)
  end

  def self.jd(jd = 0, sg = ITALY)
    # Convert Julian Day Number to civil date
    l = jd + 68569
    n = (4 * l) / 146097
    l = l - (146097 * n + 3) / 4
    i = (4000 * (l + 1)) / 1461001
    l = l - (1461 * i) / 4 + 31
    j = (80 * l) / 2447
    day = l - (2447 * j) / 80
    l = j / 11
    month = j + 2 - 12 * l
    year = 100 * (n - 49) + i + l
    civil(year, month, day, sg)
  end

  def self.parse(str = "-4712-01-01", comp = true, start = ITALY, limit: 128)
    h = _parse(str, comp, limit: limit)
    y, m, d = _complete_frags(h, str)
    civil(y, m, d, start)
  end

  # ---------------------------------------------------------------------
  # Date._strptime / Date.strptime — the format-directed parser
  # (date_strptime.c). Answers the same hash `_parse` does (:year, :mon,
  # :mday, :yday, :hour, :min, :sec, :sec_fraction, :zone, :offset,
  # :seconds, :cwyear, :cweek, :cwday, :wday, :wnum0, :wnum1), plus
  # :leftover for unconsumed input, or nil when the format does not match.

  STRPTIME_MONTHS = %w[january february march april may june july august september october november december].freeze
  STRPTIME_DAYS = %w[sunday monday tuesday wednesday thursday friday saturday].freeze

  def self._strptime(str, fmt = "%F")
    str = str.to_str unless str.is_a?(String)
    fmt = fmt.to_str unless fmt.is_a?(String)
    h = {}
    pos = __strptime(str, 0, fmt, h)
    return nil unless pos
    h[:leftover] = str[pos..-1] if pos < str.length
    if h.key?(:_cent)
      cent = h.delete(:_cent)
      h[:cwyear] = h[:cwyear] + cent * 100 if h[:cwyear]
      h[:year] = h[:year] + cent * 100 if h[:year]
    end
    if h.key?(:_merid)
      merid = h.delete(:_merid)
      h[:hour] = (h[:hour] % 12) + merid if h[:hour]
    end
    h
  end

  # Match `fmt` against `str` from `pos`, filling `h`; the position after
  # the match, or nil.
  def self.__strptime(str, pos, fmt, h)
    fi = 0
    while fi < fmt.length
      c = fmt[fi]
      if c != "%"
        if c =~ /\s/
          # A space in the format eats any run of whitespace.
          pos += 1 while pos < str.length && str[pos] =~ /\s/
          fi += 1
          next
        end
        return nil unless str[pos] == c
        pos += 1
        fi += 1
        next
      end
      fi += 1
      colons = 0
      while fmt[fi] == ":"
        colons += 1
        fi += 1
      end
      fmt[fi] == "E" || fmt[fi] == "O" and fi += 1
      d = fmt[fi]
      fi += 1
      return nil if d.nil?
      num = lambda do |width, sign = false|
        m = str[pos..-1].match(sign ? /\A[-+]?\d{1,#{width}}/ : /\A\d{1,#{width}}/)
        return nil unless m
        pos += m[0].length
        m[0].to_i
      end
      case d
      when "A", "a"
        m = str[pos..-1].match(/\A(#{STRPTIME_DAYS.join("|")}|#{STRPTIME_DAYS.map { |x| x[0, 3] }.join("|")})/i)
        return nil unless m
        pos += m[0].length
        h[:wday] = STRPTIME_DAYS.index { |x| x.start_with?(m[0].downcase[0, 3]) }
      when "B", "b", "h"
        m = str[pos..-1].match(/\A(#{STRPTIME_MONTHS.join("|")}|#{STRPTIME_MONTHS.map { |x| x[0, 3] }.join("|")})/i)
        return nil unless m
        pos += m[0].length
        h[:mon] = STRPTIME_MONTHS.index { |x| x.start_with?(m[0].downcase[0, 3]) } + 1
      when "C"
        v = num.call(2, true) or return nil
        h[:_cent] = v
      when "c"
        pos = __strptime(str, pos, "%a %b %e %H:%M:%S %Y", h) or return nil
      when "D", "x"
        pos = __strptime(str, pos, "%m/%d/%y", h) or return nil
      when "d", "e"
        pos += 1 while d == "e" && str[pos] == " "
        v = num.call(2) or return nil
        return nil unless v.between?(1, 31)
        h[:mday] = v
      when "F"
        pos = __strptime(str, pos, "%Y-%m-%d", h) or return nil
      when "G"
        v = num.call(fmt[fi] ? 4 : 30, true) or return nil
        h[:cwyear] = v
      when "g"
        v = num.call(2) or return nil
        h[:cwyear] = v
        h[:_cent] ||= v >= 69 ? 19 : 20
      when "H", "k"
        pos += 1 while d == "k" && str[pos] == " "
        v = num.call(2) or return nil
        return nil unless v.between?(0, 24)
        h[:hour] = v
      when "I", "l"
        pos += 1 while d == "l" && str[pos] == " "
        v = num.call(2) or return nil
        return nil unless v.between?(1, 12)
        h[:hour] = v
      when "j"
        v = num.call(3) or return nil
        return nil unless v.between?(1, 366)
        h[:yday] = v
      when "L", "N"
        m = str[pos..-1].match(/\A\d+/) or return nil
        pos += m[0].length
        h[:sec_fraction] = Rational(m[0].to_i, 10**m[0].length)
      when "M"
        v = num.call(2) or return nil
        return nil unless v.between?(0, 59)
        h[:min] = v
      when "m"
        v = num.call(2) or return nil
        return nil unless v.between?(1, 12)
        h[:mon] = v
      when "n", "t"
        pos += 1 while pos < str.length && str[pos] =~ /\s/
      when "P", "p"
        m = str[pos..-1].match(/\A(a\.?m\.?|p\.?m\.?)/i) or return nil
        pos += m[0].length
        h[:_merid] = m[0].downcase.start_with?("p") ? 12 : 0
      when "Q"
        m = str[pos..-1].match(/\A-?\d+/) or return nil
        pos += m[0].length
        h[:seconds] = Rational(m[0].to_i, 1000)
      when "R"
        pos = __strptime(str, pos, "%H:%M", h) or return nil
      when "r"
        pos = __strptime(str, pos, "%I:%M:%S %p", h) or return nil
      when "S"
        v = num.call(2) or return nil
        return nil unless v.between?(0, 60)
        h[:sec] = v
      when "s"
        m = str[pos..-1].match(/\A-?\d+/) or return nil
        pos += m[0].length
        h[:seconds] = m[0].to_i
      when "T", "X"
        pos = __strptime(str, pos, "%H:%M:%S", h) or return nil
      when "U", "W"
        v = num.call(2) or return nil
        return nil unless v.between?(0, 53)
        h[d == "U" ? :wnum0 : :wnum1] = v
      when "u"
        v = num.call(1) or return nil
        return nil unless v.between?(1, 7)
        h[:cwday] = v
      when "V"
        v = num.call(2) or return nil
        return nil unless v.between?(1, 53)
        h[:cweek] = v
      when "v"
        pos = __strptime(str, pos, "%e-%b-%Y", h) or return nil
      when "w"
        v = num.call(1) or return nil
        return nil unless v.between?(0, 6)
        h[:wday] = v
      when "Y"
        v = num.call(fmt[fi] =~ /\d/ ? 4 : 30, true) or return nil
        h[:year] = v
      when "y"
        v = num.call(2) or return nil
        return nil unless v.between?(0, 99)
        h[:year] = v
        h[:_cent] ||= v >= 69 ? 19 : 20
      when "Z", "z"
        m = str[pos..-1].match(/\A(?:gmt|utc?|z|[a-z]{3,4}(?:\s+dst)?|[-+]\d{1,2}(?::?\d{2}(?::?\d{2})?)?(?:\s*[-+]\d{1,2}(?::?\d{2})?)?|[a-z]+(?:\s+(?:standard|daylight)\s+time)?)/i)
        return nil unless m
        pos += m[0].length
        h[:zone] = m[0]
        h[:offset] = zone_to_diff(m[0])
      when "+"
        pos = __strptime(str, pos, "%a %b %e %H:%M:%S %Z %Y", h) or return nil
      when "%"
        return nil unless str[pos] == "%"
        pos += 1
      else
        # An unknown directive matches itself literally (`%q` matches "%q").
        return nil unless str[pos, 2] == "%#{d}"
        pos += 2
      end
    end
    pos
  end

  def self.strptime(str = "-4712-01-01", fmt = "%F", start = ITALY)
    h = _strptime(str, fmt)
    raise Date::Error, "invalid date" if h.nil? || h.key?(:leftover)
    if h[:seconds]
      return (Date.civil(1970, 1, 1) + Rational(h[:seconds], 86400)).__truncate_to_date
    end
    y, m, d = _complete_frags(h, str)
    civil(y, m, d, start)
  end

  def __truncate_to_date
    Date.jd(jd)
  end

  # ---------------------------------------------------------------------
  # Date._parse — the heuristic parser behind Date.parse, DateTime.parse
  # and Time.parse (`time.rb` calls it directly).
  #
  # A Ruby transcription of the sub-parsers in CRuby's date_parse.c
  # (`date__parse`): the string is normalized, the weekday and the time
  # are cut out, then the date sub-parsers run in CRuby's order and the
  # first one that matches wins. Returns the same hash CRuby does:
  # :year, :mon, :mday, :yday, :cwyear/:cweek/:cwday, :wday, :hour,
  # :min, :sec, :sec_fraction (Rational), :zone (String) and :offset
  # (seconds, or nil for an unknown zone name).

  ABBR_MONTH_TABLE = {
    "jan" => 1, "feb" => 2, "mar" => 3, "apr" => 4, "may" => 5, "jun" => 6,
    "jul" => 7, "aug" => 8, "sep" => 9, "oct" => 10, "nov" => 11, "dec" => 12,
  }.freeze
  ABBR_DAY_TABLE = {
    "sun" => 0, "mon" => 1, "tue" => 2, "wed" => 3, "thu" => 4, "fri" => 5, "sat" => 6,
  }.freeze
  # Zone abbreviation => UTC offset in hours (zonetab.list, abridged to
  # the common entries plus the military single letters).
  ZONE_TABLE = {
    "ut" => 0, "utc" => 0, "gmt" => 0, "z" => 0, "wet" => 0,
    "est" => -5, "edt" => -4, "cst" => -6, "cdt" => -5, "mst" => -7, "mdt" => -6,
    "pst" => -8, "pdt" => -7, "akst" => -9, "akdt" => -8, "hst" => -10, "hast" => -10,
    "hadt" => -9, "ast" => -4, "adt" => -3, "nst" => -3.5, "ndt" => -2.5,
    "bst" => 1, "cet" => 1, "cest" => 2, "met" => 1, "mest" => 2, "mez" => 1, "mesz" => 2,
    "west" => 1, "eet" => 2, "eest" => 3, "msk" => 3, "msd" => 4, "ist" => 5.5,
    "jst" => 9, "kst" => 9, "hkt" => 8, "sgt" => 8, "wib" => 7, "wit" => 9, "wita" => 8,
    "awst" => 8, "acst" => 9.5, "acdt" => 10.5, "aest" => 10, "aedt" => 11,
    "nzst" => 12, "nzdt" => 13, "sast" => 2, "cat" => 2, "eat" => 3, "wat" => 1,
    "brt" => -3, "art" => -3, "clt" => -4, "clst" => -3,
    "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5, "f" => 6, "g" => 7, "h" => 8,
    "i" => 9, "k" => 10, "l" => 11, "m" => 12, "n" => -1, "o" => -2, "p" => -3,
    "q" => -4, "r" => -5, "s" => -6, "t" => -7, "u" => -8, "v" => -9, "w" => -10,
    "x" => -11, "y" => -12,
  }.freeze
  JIS_ERA_BASE = { "m" => 1867, "t" => 1911, "s" => 1925, "h" => 1988, "r" => 2018 }.freeze
  MONTH_PAT = "jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"
  BC_PAT = "c(?:e|\\.e\\.)|b(?:ce|\\.c\\.e\\.)|a(?:d|\\.d\\.)|b(?:c|\\.c\\.)"

  def self._parse(str, comp = true, limit: 128)
    unless str.is_a?(String)
      raise TypeError, "no implicit conversion of #{str.class} into String" unless str.respond_to?(:to_str)
      str = str.to_str
    end
    if limit && str.length > limit
      raise ArgumentError, "string length (#{str.length}) exceeds the limit #{limit}"
    end
    str = str.gsub(/[^-+',.\/:@[:alnum:]\[\]]+/, " ")
    h = {}
    # Character classes present in the string (CRuby's `check_class`).
    # Each sub-parser is tried only when the characters its pattern
    # needs are there at all: "2026-09-10 08:40:12" has no letters, so
    # the month-name patterns (`__parse_eu` / `__parse_us`, several µs
    # each as case-insensitive alternations) are never run on it.
    alpha = str.match?(/[a-zA-Z]/)
    digit = str.match?(/\d/)
    dash = str.include?("-")
    dot = str.include?(".")
    slash = str.include?("/")
    __parse_day(str, h) if alpha
    __parse_time(str, h) if digit
    (alpha && digit && __parse_eu(str, h)) ||
      (alpha && digit && __parse_us(str, h)) ||
      (digit && dash && __parse_iso(str, h)) ||
      (digit && dot && __parse_jis(str, h)) ||
      (alpha && digit && dash && __parse_vms(str, h)) ||
      (digit && slash && __parse_sla(str, h)) ||
      (digit && dot && __parse_dot(str, h)) ||
      (digit && __parse_iso2(str, h)) ||
      (digit && __parse_year(str, h)) ||
      (alpha && __parse_mon(str, h)) ||
      (digit && __parse_mday(str, h)) ||
      (digit && __parse_ddd(str, h))
    __parse_bc(str, h) if alpha
    __parse_frag(str, h) if digit
    if h.delete(:_bc)
      h[:cwyear] = -h[:cwyear] + 1 if h[:cwyear]
      h[:year] = -h[:year] + 1 if h[:year]
    end
    if comp && h.delete(:_comp) != false
      [:cwyear, :year].each do |k|
        y = h[k]
        h[k] = y + (y >= 69 ? 1900 : 2000) if y && y >= 0 && y <= 99
      end
    else
      h.delete(:_comp)
    end
    h[:offset] = zone_to_diff(h[:zone]) if h.key?(:zone)
    h
  end

  # Offset in seconds for a zone string ("+09:00", "-0500", "UTC+9",
  # "EST", "Pacific Standard Time"); nil when it is not recognized.
  # Private in CRuby too (`Date.send(:zone_to_diff, ...)`).
  def self.zone_to_diff(zone)
    return nil unless zone.is_a?(String)
    z = zone.downcase.strip
    dst = false
    if z.sub!(/\s+(standard)\s+time\z/, "")
      # nothing
    elsif z.sub!(/\s+daylight\s+time\z/, "")
      dst = true
    elsif z.sub!(/\s+dst\z/, "")
      dst = true
    end
    if (off = ZONE_TABLE[z])
      return (off * 3600).to_i + (dst ? 3600 : 0)
    end
    if z =~ /\A(?:gmt|utc?)?([-+])(\d+)(?:([,.:])(\d+)(?::(\d+))?)?\z/
      sign = $1 == "-" ? -1 : 1
      digits = $2
      if $3 == ","  || $3 == "."
        secs = digits.to_i * 3600 + Rational("0.#{$4}") * 3600
        return (sign * secs).to_i
      elsif $3 == ":"
        hour = digits.to_i
        min = $4.to_i
        sec = $5.to_i
        return sign * (hour * 3600 + min * 60 + sec)
      else
        hour, min, sec = case digits.length
                         when 1, 2 then [digits.to_i, 0, 0]
                         when 3 then [digits[0, 1].to_i, digits[1, 2].to_i, 0]
                         when 4 then [digits[0, 2].to_i, digits[2, 2].to_i, 0]
                         when 5 then [digits[0, 1].to_i, digits[1, 2].to_i, digits[3, 2].to_i]
                         else [digits[0, 2].to_i, digits[2, 2].to_i, digits[4, 2].to_i]
                         end
        return sign * (hour * 3600 + min * 60 + sec)
      end
    end
    nil
  end

  class << self
    private :zone_to_diff
    private

    def __mon_num(s)
      ABBR_MONTH_TABLE[s[0, 3].downcase]
    end

    def __parse_day(str, h)
      if str.sub!(/\b(sun|mon|tue|wed|thu|fri|sat)[^-\/\d\s]*/i, " ")
        h[:wday] = ABBR_DAY_TABLE[$1.downcase]
        true
      end
    end

    TIME_PAT = /(
        (?:\d+\s*:\s*\d+(?:\s*:\s*\d+(?:[,.]\d*)?)?
          |\d+\s*h(?:\s*\d+m?(?:\s*\d+s?)?)?)
        (?:\s*[ap](?:m\b|\.m\.))?
      |\d+\s*[ap](?:m\b|\.m\.)
      )
      (?:\s*
        ((?:gmt|utc?)?[-+]\d+(?:[,.:]\d+(?::\d+)?)?
         |(?-i:[[:alpha:].\s]+)(?:standard|daylight)\stime\b
         |(?-i:[[:alpha:]]+)(?:\sdst)?\b)
      )?/xi

    def __parse_time(str, h)
      return unless str.sub!(TIME_PAT, " ")
      time = $1
      zone = $2
      h[:zone] = zone if zone
      if time =~ /\A(\d+)h?(?:\s*:?\s*(\d+)m?(?:\s*:?\s*(\d+)(?:[,.](\d+))?s?)?)?(?:\s*([ap])(?:m\b|\.m\.))?/i
        hour = $1.to_i
        min = $2
        sec = $3
        frac = $4
        merid = $5
        if merid
          hour %= 12
          hour += 12 if merid.downcase == "p"
        end
        h[:hour] = hour
        h[:min] = min.to_i if min
        h[:sec] = sec.to_i if sec
        h[:sec_fraction] = Rational(frac.to_i, 10**frac.length) if frac
      end
      true
    end

    # `s3e`: assign (year, mon, mday) from three loosely ordered fields,
    # rotating them as CRuby does when the "year" is not year-like and
    # another field is (a 3+ digit or apostrophe-prefixed number).
    def __s3e(h, y, m, d, bc = false)
      m = m.to_s unless m.nil? || m.is_a?(String)
      if !y.nil? && !m.nil? && d.nil?
        y, m, d = d, y, m
      end
      if y.nil?
        if !d.nil? && d.length > 2
          y, d = d, nil
        end
        if !d.nil? && d.start_with?("'")
          y, d = d, nil
        end
      end
      # A plain (optionally signed / apostrophe-prefixed) digit run is the
      # common case ("2026" from an ISO date); it has no trailing text, so
      # the three regexp steps below are only needed for anything else.
      if !y.nil? && !y.match?(/\A[-+']?\d+\z/)
        s = y.sub(/\A[^-+\d]*/, "")
        digits = s.match(/\A[-+]?\d+/)
        if digits && digits.end(0) < s.length
          # trailing non-digits ("14th"): this was the day
          y, d = d, y
        end
      end
      if !m.nil? && (m.start_with?("'") || m.length > 2)
        # us -> be
        y, m, d = m, d, y
      end
      if !d.nil? && (d.start_with?("'") || d.length > 2)
        y, d = d, y
      end
      unless y.nil?
        if y.match?(/\A\d+\z/)
          iy = y.to_i
          iy = -iy + 1 if bc
          h[:year] = iy
          h[:_comp] = false if y.length > 2
        elsif y =~ /([-+]?)(\d+)/
          iy = $2.to_i
          iy = -iy if $1 == "-"
          iy = -iy + 1 if bc
          h[:year] = iy
          h[:_comp] = false if $2.length > 2
        end
      end
      h[:_bc] = true if bc
      h[:mon] = (m.match?(/\A\d+\z/) ? m : m[/\d+/]).to_i unless m.nil?
      h[:mday] = (d.match?(/\A\d+\z/) ? d : d[/\d+/]).to_i unless d.nil?
      true
    end

    # The month/era alternations are interpolated, so these are hoisted
    # into constants: a `/#{...}/` literal in the method body would be
    # rebuilt (source concatenation, validation, cache lookup) on every
    # call, and `_parse` runs several of them per string. CRuby's C
    # implementation compiles each of these patterns exactly once too.
    EU_PAT = /'?(\d+)[^-\d\s]*\s*(#{MONTH_PAT})[^-\d\s']*(?:\s*(#{BC_PAT})?\s*('?-?\d+(?:(?:st|nd|rd|th)\b)?))?/i
    US_PAT = /\b(#{MONTH_PAT})[^-\d\s']*\s*('?\d+)[^-\d\s']*(?:\s*,?\s*(#{BC_PAT})?\s*('?-?\d+))?/i
    VMS_PAT1 = /('?-?\d+)-(#{MONTH_PAT})[^-\/.]*-('?-?\d+)/i
    VMS_PAT2 = /\b(#{MONTH_PAT})[^-\/.]*-('?-?\d+)(?:-('?-?\d+))?/i
    MON_PAT = /\b(#{MONTH_PAT})\S*/i

    def __parse_eu(str, h)
      return unless str.sub!(EU_PAT, " ")
      d, mon, bc, y = $1, $2, $3, $4
      __s3e(h, y, __mon_num(mon), d, !!(bc && bc =~ /\Ab/i))
    end

    def __parse_us(str, h)
      return unless str.sub!(US_PAT, " ")
      mon, d, bc, y = $1, $2, $3, $4
      __s3e(h, y, __mon_num(mon), d, !!(bc && bc =~ /\Ab/i))
    end

    def __parse_iso(str, h)
      return unless str.sub!(/('?[-+]?\d+)-(\d+)-('?-?\d+)/, " ")
      __s3e(h, $1, $2, $3)
    end

    def __parse_jis(str, h)
      return unless str.sub!(/\b([mtshr])(\d+)\.(\d+)\.(\d+)/i, " ")
      h[:year] = JIS_ERA_BASE[$1.downcase] + $2.to_i
      h[:mon] = $3.to_i
      h[:mday] = $4.to_i
      true
    end

    def __parse_vms(str, h)
      if str.sub!(VMS_PAT1, " ")
        __s3e(h, $3, __mon_num($2), $1)
      elsif str.sub!(VMS_PAT2, " ")
        __s3e(h, $3, __mon_num($1), $2)
      end
    end

    def __parse_sla(str, h)
      return unless str.sub!(%r{('?-?\d+)/\s*('?\d+)(?:\D\s*('?-?\d+))?}, " ")
      __s3e(h, $1, $2, $3)
    end

    def __parse_dot(str, h)
      return unless str.sub!(/('?-?\d+)\.\s*('?\d+)\.\s*('?-?\d+)/, " ")
      __s3e(h, $1, $2, $3)
    end

    def __parse_iso2(str, h)
      if str.sub!(/\b(\d{2}|\d{4})?-?w(\d{2})(?:-?(\d))?\b/i, " ")
        h[:cwyear] = $1.to_i if $1
        h[:cweek] = $2.to_i
        h[:cwday] = $3.to_i if $3
        true
      elsif str.sub!(/-w-(\d)\b/i, " ")
        h[:cwday] = $1.to_i
        true
      elsif str.sub!(/--(\d{2})?-(\d{2})\b/, " ")
        h[:mon] = $1.to_i if $1
        h[:mday] = $2.to_i
        true
      elsif str.sub!(/--(\d{2})(\d{2})?\b/, " ")
        h[:mon] = $1.to_i
        h[:mday] = $2.to_i if $2
        true
      elsif str.sub!(/[-']?(\d{2}|\d{4})-(\d{3})\b/, " ")
        h[:year] = $1.to_i
        h[:yday] = $2.to_i
        h[:_comp] = false if $1.length > 2
        true
      elsif str.sub!(/\b-(\d{3})\b/, " ")
        h[:yday] = $1.to_i
        true
      end
    end

    def __parse_year(str, h)
      return unless str.sub!(/'(\d+)\b/, " ")
      h[:year] = $1.to_i
      true
    end

    def __parse_mon(str, h)
      return unless str.sub!(MON_PAT, " ")
      h[:mon] = __mon_num($1)
      true
    end

    def __parse_mday(str, h)
      return unless str.sub!(/(\d+)(st|nd|rd|th)\b/i, " ")
      h[:mday] = $1.to_i
      true
    end

    # Digit runs without separators: "20130814", "20130814T150000Z", …
    def __parse_ddd(str, h)
      re = /([-+]?)(\d{2,14})(?:\s*t?\s*(\d{2,6})?(?:[,.](\d*))?)?(?:\s*(z\b|[-+]\d{1,4}\b|\[[-+]?\d[^\]]*\]))?/i
      return unless str.sub!(re, " ")
      sign, s2, s3, s4, s5 = $1, $2, $3, $4, $5
      case s2.length
      when 2
        if sign.empty? && s3.nil?
          h[:mday] = s2.to_i
        else
          h[:hour] = s2.to_i
        end
      when 3
        h[:yday] = s2.to_i
      when 4
        if s3.nil?
          h[:mon] = s2[0, 2].to_i
          h[:mday] = s2[2, 2].to_i
        else
          h[:hour] = s2[0, 2].to_i
          h[:min] = s2[2, 2].to_i
        end
      when 5
        h[:year] = s2[0, 2].to_i
        h[:yday] = s2[2, 3].to_i
      when 6
        h[:year] = s2[0, 2].to_i
        h[:mon] = s2[2, 2].to_i
        h[:mday] = s2[4, 2].to_i
      when 7
        h[:year] = s2[0, 4].to_i
        h[:yday] = s2[4, 3].to_i
        h[:_comp] = false
      when 8, 10, 12, 14
        h[:year] = s2[0, 4].to_i
        h[:mon] = s2[4, 2].to_i
        h[:mday] = s2[6, 2].to_i
        h[:hour] = s2[8, 2].to_i if s2.length >= 10
        h[:min] = s2[10, 2].to_i if s2.length >= 12
        h[:sec] = s2[12, 2].to_i if s2.length >= 14
        h[:_comp] = false
      when 9
        h[:year] = s2[0, 4].to_i
        h[:yday] = s2[4, 3].to_i
        h[:hour] = s2[7, 2].to_i
        h[:_comp] = false
      when 11
        h[:year] = s2[0, 4].to_i
        h[:yday] = s2[4, 3].to_i
        h[:hour] = s2[7, 2].to_i
        h[:min] = s2[9, 2].to_i
        h[:_comp] = false
      when 13
        h[:year] = s2[0, 4].to_i
        h[:yday] = s2[4, 3].to_i
        h[:hour] = s2[7, 2].to_i
        h[:min] = s2[9, 2].to_i
        h[:sec] = s2[11, 2].to_i
        h[:_comp] = false
      end
      h[:year] = -h[:year] if sign == "-" && h[:year]
      if s3
        h[:hour] = s3[0, 2].to_i
        h[:min] = s3[2, 2].to_i if s3.length >= 4
        h[:sec] = s3[4, 2].to_i if s3.length >= 6
      end
      h[:sec_fraction] = Rational(s4.to_i, 10**s4.length) if s4 && !s4.empty?
      if s5
        zone = s5.start_with?("[") ? s5[1..-2] : s5
        h[:zone] = zone
      end
      true
    end

    def __parse_bc(str, h)
      if str.sub!(/\b(bc\b|bce\b|b\.c\.|b\.c\.e\.)/i, " ")
        h[:_bc] = true
      end
    end

    # A lone 1- or 2-digit number left over is the day (or hour when a
    # date is already known).
    def __parse_frag(str, h)
      if str =~ /\A\s*(\d{1,2})\s*\z/
        n = $1.to_i
        if h.key?(:hour) && !h.key?(:mday)
          h[:mday] = n if n >= 1 && n <= 31
        elsif h.key?(:mday) && !h.key?(:hour)
          h[:hour] = n if n >= 0 && n <= 24
        end
      end
    end
  end

  # Turn a `_parse` hash into (year, mon, mday), filling what the string
  # did not say from today the way CRuby's `rt_complete_frags` does, and
  # rejecting a hash that names no date at all.
  def self._complete_frags(h, str)
    if h[:year] && h[:yday]
      d = Date.civil(h[:year], 1, 1) + (h[:yday] - 1)
      return [d.year, d.month, d.day]
    end
    if h[:cwyear] || h[:cweek]
      raise ArgumentError, "invalid date" unless h[:cwyear] && h[:cweek]
      jan4 = Date.civil(h[:cwyear], 1, 4)
      monday = jan4 - ((jan4.wday + 6) % 7)
      d = monday + (h[:cweek] - 1) * 7 + ((h[:cwday] || 1) - 1)
      return [d.year, d.month, d.day]
    end
    y, m, d = h[:year], h[:mon], h[:mday]
    if y.nil? && m.nil? && d.nil?
      if h[:wday] && !h.key?(:hour)
        t = Date.today
        dd = t + ((h[:wday] - t.wday) % 7)
        return [dd.year, dd.month, dd.day]
      end
      raise ArgumentError, "invalid date" unless h[:hour] || h[:yday]
      if h[:yday]
        dd = Date.civil(Date.today.year, 1, 1) + (h[:yday] - 1)
        return [dd.year, dd.month, dd.day]
      end
      t = Date.today
      return [t.year, t.month, t.day]
    end
    t = Date.today
    if y.nil?
      y = t.year
    end
    if m.nil?
      m = d.nil? ? 1 : (h[:year] ? 1 : t.month)
    end
    d = 1 if d.nil?
    unless m.between?(1, 12) && d.between?(1, Date._days_in_month(y, m))
      raise ArgumentError, "invalid date"
    end
    [y, m, d]
  end

  def jd
    a = (14 - @month) / 12
    y = @year + 4800 - a
    m = @month + 12 * a - 3
    @day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045
  end

  # JD 0 was a Monday, so Sunday (Ruby's wday 0) is `jd % 7 == 6`.
  def wday
    (jd + 1) % 7
  end

  def yday
    days = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    days[2] = 29 if leap?
    days[0...@month].sum + @day
  end

  def leap?
    if @year % 4 == 0
      if @year % 100 == 0
        @year % 400 == 0
      else
        true
      end
    else
      false
    end
  end

  def +(n)
    # Simple day addition via Time
    t = Time.new(@year, @month, @day) + (n.to_i * 86400)
    Date.civil(t.year, t.month, t.day)
  end

  def -(other)
    case other
    when Numeric
      self + (-other)
    when Date
      jd - other.jd
    else
      raise TypeError, "expected numeric or date"
    end
  end

  def <=>(other)
    case other
    when Date
      [year, month, day] <=> [other.year, other.month, other.day]
    else
      nil
    end
  end

  include Comparable

  def to_s
    format("%04d-%02d-%02d", @year, @month, @day)
  end

  # The calendar-reform start given at construction (`Date::ITALY`
  # unless the caller chose otherwise); `inspect` shows it as CRuby does.
  def start
    @sg || ITALY
  end

  def __start_for_inspect
    sg = start
    if sg == Float::INFINITY then "Infj"
    elsif sg == -Float::INFINITY then "-Infj"
    else "#{sg.to_i}j"
    end
  end

  def inspect
    "#<Date: #{to_s} ((#{jd}j,0s,0n),+0s,#{__start_for_inspect})>"
  end

  def to_time
    Time.new(@year, @month, @day)
  end

  def to_date
    self
  end

  def to_datetime
    DateTime.civil(@year, @month, @day, 0, 0, 0)
  end

  def strftime(fmt = "%F")
    fmt.gsub("%F", to_s)
       .gsub("%Y", format("%04d", @year))
       .gsub("%m", format("%02d", @month))
       .gsub("%d", format("%02d", @day))
  end

  def hash
    to_s.hash
  end

  def eql?(other)
    self.class == other.class && self == other
  end

  # Add `n` months. The result's day is clamped to the last valid
  # day of the resulting month (e.g. Jan 31 >> 1 ⇒ Feb 28/29).
  # Dispatches through `self.class.civil` so a DateTime receiver
  # returns a DateTime (CRuby preserves the type).
  def >>(n)
    n = n.to_int
    total = @year * 12 + (@month - 1) + n
    y = total.div(12)
    m = total.modulo(12) + 1
    d = [@day, Date._days_in_month(y, m)].min
    if self.is_a?(DateTime)
      self.class.civil(y, m, d, @hour, @min, @sec, @offset)
    else
      self.class.civil(y, m, d)
    end
  end

  # Subtract `n` months. `Date#<<(n)` is `Date#>>(-n)`.
  def <<(n)
    self >> -n.to_int
  end

  # Next-month / previous-month helpers; both delegate to `>>` so
  # the end-of-month clamping is identical.
  def next_month(n = 1) ; self >> n.to_int ; end
  def prev_month(n = 1) ; self << n.to_int ; end

  # Next-year / previous-year — multiply by 12 and reuse `>>`.
  def next_year(n = 1)  ; self >> (n.to_int * 12) ; end
  def prev_year(n = 1)  ; self << (n.to_int * 12) ; end

  # CRuby: `Date#julian?` is true iff the date is interpreted in
  # the Julian calendar. monoruby's Date is always Gregorian (the
  # `sg` switchover argument is accepted for compatibility but
  # never consulted), so this is always false. `gregorian?` is the
  # mirror.
  def julian?    ; false ; end
  def gregorian? ; true  ; end

  # Pattern-matching deconstructor (CRuby 3.2+):
  # `keys=nil` ⇒ all four keys; otherwise only the matching ones.
  def deconstruct_keys(keys)
    all = { year: @year, month: @month, day: @day, yday: yday, wday: wday }
    return all if keys.nil?
    keys.each_with_object({}) { |k, h| h[k] = all[k] if all.key?(k) }
  end

  # Helper for `>>`/`<<`: number of days in the given (y, m) under
  # the Gregorian calendar with the same leap-year rules as `leap?`.
  def self._days_in_month(y, m)
    days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if m == 2
      leap = (y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0))
      leap ? 29 : 28
    else
      days[m - 1]
    end
  end
end

class DateTime < Date
  attr_reader :hour, :min, :sec, :offset

  def initialize(year = -4712, month = 1, day = 1, hour = 0, min = 0, sec = 0, offset = 0, _sg = ITALY)
    super(year, month, day, _sg)
    @hour = hour
    @min = min
    @sec = sec
    @offset = offset
  end

  def self.civil(year = -4712, month = 1, day = 1, hour = 0, min = 0, sec = 0, offset = 0, sg = ITALY)
    new(year, month, day, hour, min, sec, offset, sg)
  end

  def self.now(sg = ITALY)
    t = Time.now
    off = t.utc_offset
    civil(t.year, t.month, t.day, t.hour, t.min, t.sec, Rational(off, 86400), sg)
  end

  def self.parse(str = "-4712-01-01T00:00:00+00:00", comp = true, start = ITALY, limit: 128)
    h = Date._parse(str, comp, limit: limit)
    y, m, d = Date._complete_frags(h, str)
    hour = h[:hour] || 0
    min = h[:min] || 0
    sec = h[:sec] || 0
    sec = 59 if sec == 60
    unless hour.between?(0, 24) && min.between?(0, 59) && sec.between?(0, 59)
      raise ArgumentError, "invalid date"
    end
    if hour == 24
      raise ArgumentError, "invalid date" unless min == 0 && sec == 0
      hour = 0
      d1 = Date.civil(y, m, d) + 1
      y, m, d = d1.year, d1.month, d1.day
    end
    offset = h[:offset] ? Rational(h[:offset], 86400) : 0
    dt = civil(y, m, d, hour, min, sec, offset, start)
    dt.instance_variable_set(:@sec_fraction, h[:sec_fraction]) if h[:sec_fraction]
    dt
  end

  def self.iso8601(str = "-4712-01-01T00:00:00+00:00", start = ITALY, limit: 128)
    parse(str, true, start, limit: limit)
  end

  def self._strptime(str, fmt = "%FT%T%z")
    Date._strptime(str, fmt)
  end

  def self.strptime(str = "-4712-01-01T00:00:00+00:00", fmt = "%FT%T%z", start = ITALY)
    h = Date._strptime(str, fmt)
    raise Date::Error, "invalid date" if h.nil? || h.key?(:leftover)
    if h[:seconds]
      offset = h[:offset] || 0
      return DateTime.civil(1970, 1, 1, 0, 0, 0, 0, start).__new_offset_seconds(h[:seconds], offset)
    end
    y, m, d = Date._complete_frags(h, str)
    hour = h[:hour] || 0
    min = h[:min] || 0
    sec = h[:sec] || 0
    sec = 59 if sec == 60
    unless hour.between?(0, 24) && min.between?(0, 59) && sec.between?(0, 59)
      raise Date::Error, "invalid date"
    end
    offset = h[:offset] ? Rational(h[:offset], 86400) : 0
    dt = civil(y, m, d, hour, min, sec, offset, start)
    dt.instance_variable_set(:@sec_fraction, h[:sec_fraction]) if h[:sec_fraction]
    dt
  end

  # `DateTime` at `seconds` since the epoch, shown at `offset` seconds.
  def __new_offset_seconds(seconds, offset)
    (self + Rational(seconds, 86400)).new_offset(Rational(offset, 86400))
  end

  alias minute min
  alias second sec

  # The UTC offset as a fraction of a day (always a Rational, as in
  # CRuby, whatever `civil` was handed).
  def offset
    off = @offset || 0
    off.is_a?(Rational) ? off : Rational(off, 1)
  end

  # Fraction of the second as a Rational (0 unless the parsed string
  # carried one).
  def sec_fraction
    @sec_fraction || Rational(0, 1)
  end
  alias second_fraction sec_fraction

  # The UTC offset in seconds (the `offset` attribute is the CRuby
  # Rational fraction of a day).
  def __offset_seconds
    off = @offset || 0
    (off * 86400).round
  end

  def zone
    s = __offset_seconds
    sign = s < 0 ? "-" : "+"
    s = s.abs
    format("%s%02d:%02d", sign, s / 3600, (s % 3600) / 60)
  end

  def new_offset(offset = 0)
    off = case offset
          when String then Rational(Date.send(:zone_to_diff, offset) || 0, 86400)
          when Rational, Integer, Float then offset
          else raise TypeError, "invalid offset"
          end
    t = to_time.getutc + (off * 86400).to_i
    dt = self.class.civil(t.year, t.month, t.day, t.hour, t.min, t.sec, off)
    dt.instance_variable_set(:@sec_fraction, @sec_fraction) if @sec_fraction
    dt
  end

  def to_s
    format("%04d-%02d-%02dT%02d:%02d:%02d%s", @year, @month, @day, @hour, @min, @sec, zone)
  end

  def iso8601(n = 0)
    s = to_s
    return s if n <= 0
    frac = format("%.#{n}f", sec_fraction)[1..]
    s.sub(/(?=[-+]\d\d:\d\d\z)/, frac)
  end
  alias xmlschema iso8601

  def inspect
    utc = new_offset(0)
    secs = utc.hour * 3600 + utc.min * 60 + utc.sec
    ns = (sec_fraction * 1_000_000_000).to_i
    off = (offset * 86400).to_i
    "#<DateTime: #{to_s} ((#{utc.jd}j,#{secs}s,#{ns}n),#{format('%+d', off)}s,#{__start_for_inspect})>"
  end

  def <=>(other)
    case other
    when DateTime
      [to_time.to_r, sec_fraction] <=> [other.to_time.to_r, other.sec_fraction]
    when Date
      [year, month, day] <=> [other.year, other.month, other.day]
    else
      nil
    end
  end

  def to_time
    Time.new(@year, @month, @day, @hour, @min, @sec + sec_fraction, __offset_seconds)
  end

  def strftime(fmt = "%FT%T%:z")
    to_time.strftime(fmt)
  end

  def deconstruct_keys(keys)
    all = { year: @year, month: @month, day: @day, yday: yday, wday: wday,
            hour: @hour, min: @min, sec: @sec, sec_fraction: sec_fraction, zone: zone }
    return all if keys.nil?
    keys.each_with_object({}) { |k, h| h[k] = all[k] if all.key?(k) }
  end

  def to_date
    Date.civil(@year, @month, @day)
  end

  def to_datetime
    self
  end
end
