# strptime/strptime.rb – monoruby's stand-in for strptime.so
#
# The strptime gem is two C classes, `Strptime` and `Strftime`: a format
# string compiled once, then applied many times (fluentd parses and
# formats every event's time through them). This file is a Ruby port of
# the extension's `strptime.c` / `strftime.c`, keeping their exact
# semantics rather than delegating to `Time.strptime` / `Time#strftime`:
#
# - Only the directives the extension implements are accepted; any other
#   `%x` (including `%%`, `%Z`, `%L` in Strptime, `%F`, `%T`) is
#   `ArgumentError: invalid format` at compile time.
# - Strptime reads *up to* the directive's width in digits (at least one),
#   matches month names case-insensitively (full names first), treats a
#   run of whitespace in the format as "skip whitespace", compares any
#   other text byte for byte, ignores trailing input, and answers
#   `ArgumentError: string doesn't match` on any failure.
# - Unset fields default per the extension: with a year given, month /
#   day default to 1 and the time to 00:00:00; without one they come from
#   the current time, with every field finer than the first one given
#   reset.
# - `%z` gives the Time a fixed offset, `Z` makes it UTC, no `%z` makes it
#   local time.
# - Strftime supports `%H %L %M %N %S %Y %d %b %m %y %z`, formats in the
#   Time's own offset (`execi` in UTC) and needs a Time (`TypeError: can't
#   convert X into time` otherwise).

class Strptime
  MONTH_NAMES = %w[January February March April May June July August
                   September October November December
                   Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec].freeze

  def initialize(fmt)
    fmt = Strptime.__implicit_string(fmt)
    raise ArgumentError, "string contains null byte" if fmt.include?("\0")
    @fmt = fmt.frozen? ? fmt : fmt.dup.freeze
    @insns = Strptime.__compile(@fmt)
  end

  def initialize_copy(other)
    super
    @fmt = other.source
    @insns = Strptime.__compile(@fmt)
  end

  def source
    @fmt
  end

  def exec(str)
    str = Strptime.__implicit_string(str)
    sec, nsec, gmtoff = __exec0(str)
    case gmtoff
    when :local
      Time.at(sec, nsec, :nsec)
    when :utc
      Time.at(sec, nsec, :nsec).utc
    else
      Time.at(sec, nsec, :nsec, in: gmtoff)
    end
  end

  def execi(str)
    str = Strptime.__implicit_string(str)
    __exec0(str)[0]
  end

  def self.__implicit_string(v) # :nodoc:
    return v if v.is_a?(String)
    if v.respond_to?(:to_str)
      s = v.to_str
      return s if s.is_a?(String)
    end
    name = case v
           when nil then "nil"
           when true then "true"
           when false then "false"
           else v.class.name
           end
    raise TypeError, "no implicit conversion of #{name} into String"
  end

  # The format as a list of instructions: a directive Symbol, :n for a
  # whitespace run, or [:lit, bytes] for literal text.
  def self.__compile(fmt) # :nodoc:
    insns = []
    i = 0
    len = fmt.bytesize
    while i < len
      b = fmt.getbyte(i)
      if b == 0x25 # %
        i += 1
        c = fmt.getbyte(i)
        case c
        when 0x42 then insns << :B # %B
        when 0x48 then insns << :H
        when 0x4d then insns << :M
        when 0x4e then insns << :N
        when 0x53 then insns << :S
        when 0x59 then insns << :Y
        when 0x62, 0x68 then insns << :B # %b %h
        when 0x64, 0x65 then insns << :d # %d %e
        when 0x6d then insns << :m
        when 0x6e then insns << :n
        when 0x79 then insns << :y
        when 0x7a then insns << :z
        else raise ArgumentError, "invalid format"
        end
        i += 1
      elsif __space?(b)
        insns << :n
        i += 1
      else
        start = i
        i += 1 while i < len && fmt.getbyte(i) != 0x25 && !__space?(fmt.getbyte(i))
        insns << [:lit, fmt.byteslice(start, i - start)]
      end
    end
    insns
  end

  def self.__space?(b) # :nodoc:
    b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0b || b == 0x0c || b == 0x0d
  end

  # Reads up to `width` digits at `si`; answers [value, digits read].
  def self.__read_digits(str, si, width) # :nodoc:
    r = 0
    n = 0
    len = str.bytesize
    while n < width && si + n < len
      b = str.getbyte(si + n)
      break unless b >= 0x30 && b <= 0x39
      r = r * 10 + (b - 0x30)
      n += 1
    end
    [r, n]
  end

  private

  def __fail
    raise ArgumentError, "string doesn't match"
  end

  # Answers [seconds since the epoch, nanoseconds, offset] where offset is
  # :local, :utc or an Integer number of seconds.
  def __exec0(str)
    si = 0
    slen = str.bytesize
    year = nil
    mon = -1
    mday = -1
    hour = -1
    min = -1
    sec = -1
    nsec = 0
    gmtoff = :local
    @insns.each do |insn|
      case insn
      when :B
        found = false
        MONTH_NAMES.each_with_index do |name, idx|
          l = name.bytesize
          if si + l <= slen && str.byteslice(si, l).casecmp?(name)
            si += l
            mon = (idx % 12) + 1
            found = true
            break
          end
        end
        __fail unless found
      when :H
        hour, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0 || hour > 23
        si += n
      when :M
        min, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0 || min > 59
        si += n
      when :N
        nsec, n = Strptime.__read_digits(str, si, 9)
        __fail if n == 0
        si += n
        nsec *= 10 while (n += 1) <= 9
      when :S
        sec, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0 || sec > 60
        si += n
      when :Y
        c = str.getbyte(si)
        si += 1 if c == 0x2d || c == 0x2b
        year, n = Strptime.__read_digits(str, si, 4)
        __fail if n == 0
        si += n
        year = -year if c == 0x2d
      when :d
        mday, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0 || mday < 1 || mday > 31
        si += n
      when :m
        mon, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0 || mon < 1 || mon > 12
        si += n
      when :n
        si += 1 while si < slen && Strptime.__space?(str.getbyte(si))
      when :y
        year, n = Strptime.__read_digits(str, si, 2)
        __fail if n == 0
        si += n
        year += year < 69 ? 2000 : 1900
      when :z
        c = str.getbyte(si)
        if c == 0x7a || c == 0x5a # z Z
          gmtoff = :utc
          si += 1
        else
          si += 1 if c == 0x2d || c == 0x2b
          r, n = Strptime.__read_digits(str, si, 2)
          __fail if n == 0
          si += n
          off = r * 60
          si += 1 if str.getbyte(si) == 0x3a
          r, n = Strptime.__read_digits(str, si, 2)
          if n > 0
            si += n
            off += r
          end
          off *= 60
          off = -off if c == 0x2d
          gmtoff = off
        end
      else
        lit = insn[1]
        l = lit.bytesize
        __fail unless str.byteslice(si, l) == lit
        si += l
      end
    end

    if year
      mon = 1 if mon == -1
      mday = 1 if mday == -1
      hour = 0 if hour == -1
      min = 0 if min == -1
      sec = 0 if sec == -1
    else
      now = case gmtoff
            when :local then Time.now
            when :utc then Time.now.utc
            else Time.now.localtime(gmtoff)
            end
      year = now.year
      if mon != -1
        mday = 1 if mday == -1
        hour = 0 if hour == -1
        min = 0 if min == -1
        sec = 0 if sec == -1
      else
        mon = now.month
        if mday != -1
          hour = 0 if hour == -1
          min = 0 if min == -1
          sec = 0 if sec == -1
        else
          mday = now.day
          if hour != -1
            min = 0 if min == -1
            sec = 0 if sec == -1
          else
            hour = now.hour
            if min != -1
              sec = 0 if sec == -1
            else
              min = now.min
              sec = now.sec if sec == -1
            end
          end
        end
      end
    end

    t = case gmtoff
        when :local
          Time.local(year, mon, mday, hour, min, sec).to_i
        when :utc
          Time.utc(year, mon, mday, hour, min, sec).to_i
        else
          Time.utc(year, mon, mday, hour, min, sec).to_i - gmtoff
        end
    [t, nsec, gmtoff]
  end
end

class Strftime
  MONTH_ABBRS = %w[Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec].freeze

  def initialize(fmt)
    fmt = Strptime.__implicit_string(fmt)
    raise ArgumentError, "string contains null byte" if fmt.include?("\0")
    raise ArgumentError, "too long format string (>65335)" if fmt.bytesize > 65535
    @fmt = fmt.frozen? ? fmt : fmt.dup.freeze
    @insns = Strftime.__compile(@fmt)
  end

  def initialize_copy(other)
    super
    @fmt = other.source
    @insns = Strftime.__compile(@fmt)
  end

  def source
    @fmt
  end

  def exec(time)
    unless time.is_a?(Time)
      if time.is_a?(Numeric)
        raise TypeError, "wrong argument type #{time.class} (expected time)"
      end
      raise TypeError, "can't convert #{time.class} into time"
    end
    __exec0(time.to_i, time.nsec, time.utc_offset)
  end

  def execi(epoch)
    case epoch
    when Integer
      __exec0(epoch, 0, 0)
    when Float
      sec = epoch.to_i
      nsec = ((epoch * 1_000_000_000).to_i % 1_000_000_000)
      __exec0(sec, nsec, 0)
    when Rational
      __exec0(epoch.to_i, ((epoch * 1_000_000_000) % 1_000_000_000).to_i, 0)
    else
      raise TypeError, "can't convert #{epoch.class} into time"
    end
  end

  def self.__compile(fmt) # :nodoc:
    insns = []
    i = 0
    len = fmt.bytesize
    while i < len
      if fmt.getbyte(i) == 0x25
        i += 1
        c = fmt.getbyte(i)
        case c
        when 0x48 then insns << :H
        when 0x4c then insns << :L
        when 0x4d then insns << :M
        when 0x4e then insns << :N
        when 0x53 then insns << :S
        when 0x59 then insns << :Y
        when 0x64 then insns << :d
        when 0x62 then insns << :b
        when 0x6d then insns << :m
        when 0x79 then insns << :y
        when 0x7a then insns << :z
        else raise ArgumentError, "invalid format"
        end
        i += 1
      else
        start = i
        i += 1 while i < len && fmt.getbyte(i) != 0x25
        insns << fmt.byteslice(start, i - start)
      end
    end
    insns
  end

  private

  def __two(n)
    n < 10 ? "0#{n}" : n.to_s
  end

  def __exec0(sec, nsec, gmtoff)
    t = Time.at(sec + gmtoff).utc
    out = "".b
    @insns.each do |insn|
      case insn
      when :H then out << __two(t.hour)
      when :L
        ms = nsec / 1_000_000
        out << (ms < 10 ? "00#{ms}" : ms < 100 ? "0#{ms}" : ms.to_s)
      when :M then out << __two(t.min)
      when :N then out << nsec.to_s.rjust(9, "0")
      when :S then out << __two(t.sec)
      when :Y
        # the extension's two-digit-era quirk: years since 1900, then
        # +2000 below 69, +1900 otherwise
        y = t.year - 1900
        y += y < 69 ? 2000 : 1900
        out << y.to_s.rjust(4, "0")
      when :d then out << __two(t.day)
      when :b then out << MONTH_ABBRS[t.month - 1]
      when :m then out << __two(t.month)
      when :y then out << __two((t.year - 1900) % 100)
      when :z
        tmp = gmtoff
        if tmp >= 0
          out << "+"
        else
          out << "-"
          tmp = -tmp
        end
        tmp /= 60
        h = (tmp / 60) & 15
        m = tmp % 60
        out << __two(h) << __two(m)
      else
        out << insn
      end
    end
    out.force_encoding(@fmt.encoding)
  end
end
