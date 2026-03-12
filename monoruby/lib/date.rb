# frozen_string_literal: true
#
# Date and DateTime implementation for monoruby.
# Pure Ruby implementation using Julian Day Number (JDN) arithmetic.
#

class Date
  include Comparable

  ITALY     = 2299161  # 1582-10-15
  ENGLAND   = 2361222  # 1752-09-14
  JULIAN    = Float::INFINITY
  GREGORIAN = -Float::INFINITY

  MONTHNAMES = [nil,
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'
  ].freeze

  ABBR_MONTHNAMES = [nil,
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  ].freeze

  DAYNAMES = [
    'Sunday', 'Monday', 'Tuesday', 'Wednesday',
    'Thursday', 'Friday', 'Saturday'
  ].freeze

  ABBR_DAYNAMES = [
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'
  ].freeze

  # --- Internal helpers: Julian Day Number <-> Civil date ---
  # Gregorian calendar conversion.

  def self._civil_to_jd(y, m, d)
    # Proleptic Gregorian calendar
    if m <= 2
      y -= 1
      m += 12
    end
    a = y / 100
    b = 2 - a + a / 4
    (365.25 * (y + 4716)).floor + (30.6001 * (m + 1)).floor + d + b - 1524
  end

  def self._jd_to_civil(jd)
    # Proleptic Gregorian calendar
    a = jd
    # Gregorian correction
    alpha = ((a - 1867216.25) / 36524.25).floor
    a = a + 1 + alpha - alpha / 4

    b = a + 1524
    c = ((b - 122.1) / 365.25).floor
    d = (365.25 * c).floor
    e = ((b - d) / 30.6001).floor

    day = b - d - (30.6001 * e).floor
    month = e < 14 ? e - 1 : e - 13
    year = month > 2 ? c - 4716 : c - 4715

    [year, month, day]
  end

  def self._valid_civil?(y, m, d)
    # Basic validation
    return false if m < 1 || m > 12
    max_day = _days_in_month(y, m)
    return false if d < 1 || d > max_day
    true
  end

  def self._days_in_month(y, m)
    case m
    when 1, 3, 5, 7, 8, 10, 12 then 31
    when 4, 6, 9, 11 then 30
    when 2
      _leap?(y) ? 29 : 28
    else
      raise ArgumentError, "invalid month: #{m}"
    end
  end

  def self._leap?(y)
    (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)
  end

  # --- Class methods ---

  def self.new(year = -4712, month = 1, day = 1, start = ITALY)
    # Handle negative months/days (Ruby's Date doesn't, but be safe)
    unless _valid_civil?(year, month, day)
      raise ArgumentError, "invalid date"
    end
    jd = _civil_to_jd(year, month, day)
    _new_with_jd(jd, start)
  end

  def self.civil(year = -4712, month = 1, day = 1, start = ITALY)
    new(year, month, day, start)
  end

  def self._new_with_jd(jd, start = ITALY)
    obj = allocate
    obj.__send__(:_init, jd, start)
    obj
  end

  def self.jd(jd = 0, start = ITALY)
    _new_with_jd(jd, start)
  end

  def self.today(start = ITALY)
    t = Time.now
    new(t.year, t.month, t.day, start)
  end

  def self.parse(str, comp = true, start = ITALY)
    # Try ISO 8601: YYYY-MM-DD
    if str =~ /\A(\d{4})-(\d{1,2})-(\d{1,2})\z/
      return new($1.to_i, $2.to_i, $3.to_i, start)
    end
    # Try YYYY/MM/DD
    if str =~ /\A(\d{4})\/(\d{1,2})\/(\d{1,2})\z/
      return new($1.to_i, $2.to_i, $3.to_i, start)
    end
    # Try DD Mon YYYY or Mon DD, YYYY
    months = {
      'jan' => 1, 'feb' => 2, 'mar' => 3, 'apr' => 4,
      'may' => 5, 'jun' => 6, 'jul' => 7, 'aug' => 8,
      'sep' => 9, 'oct' => 10, 'nov' => 11, 'dec' => 12
    }
    if str =~ /\A(\d{1,2})\s+(\w{3})\s+(\d{4})\z/
      mon = months[$2.downcase]
      return new($3.to_i, mon, $1.to_i, start) if mon
    end
    if str =~ /\A(\w{3})\s+(\d{1,2}),?\s+(\d{4})\z/
      mon = months[$1.downcase]
      return new($3.to_i, mon, $2.to_i, start) if mon
    end
    # Try YYYYMMDD
    if str =~ /\A(\d{4})(\d{2})(\d{2})\z/
      return new($1.to_i, $2.to_i, $3.to_i, start)
    end
    raise ArgumentError, "invalid date: #{str.inspect}"
  end

  def self.valid_date?(year, month, day, start = ITALY)
    _valid_civil?(year, month, day)
  end
  class << self
    alias valid_civil? valid_date?
  end

  def self.leap?(year)
    _leap?(year)
  end

  # --- Initialization (private) ---

  def _init(jd, start)
    @jd = jd
    @start = start
    @civil = nil
  end
  private :_init

  def _civil
    @civil ||= self.class._jd_to_civil(@jd)
  end
  private :_civil

  # --- Instance accessors ---

  def jd
    @jd
  end

  def year
    _civil[0]
  end

  def month
    _civil[1]
  end
  alias mon month

  def day
    _civil[2]
  end
  alias mday day

  def wday
    (@jd + 1) % 7
  end

  def yday
    jd0 = self.class._civil_to_jd(year, 1, 1)
    @jd - jd0 + 1
  end

  def leap?
    self.class._leap?(year)
  end

  def sunday?;    wday == 0; end
  def monday?;    wday == 1; end
  def tuesday?;   wday == 2; end
  def wednesday?; wday == 3; end
  def thursday?;  wday == 4; end
  def friday?;    wday == 5; end
  def saturday?;  wday == 6; end

  def start
    @start
  end

  # --- Arithmetic ---

  def +(n)
    if n.is_a?(Integer)
      self.class._new_with_jd(@jd + n, @start)
    else
      self.class._new_with_jd(@jd + n.to_i, @start)
    end
  end

  def -(other)
    if other.is_a?(Date)
      @jd - other.jd
    elsif other.is_a?(Integer)
      self.class._new_with_jd(@jd - other, @start)
    else
      self.class._new_with_jd(@jd - other.to_i, @start)
    end
  end

  def >>(n)
    y = year
    m = month + n
    d = day
    # Normalize month
    while m > 12
      m -= 12
      y += 1
    end
    while m < 1
      m += 12
      y -= 1
    end
    max_d = self.class._days_in_month(y, m)
    d = max_d if d > max_d
    self.class.new(y, m, d, @start)
  end

  def <<(n)
    self >> (-n)
  end

  def next_day(n = 1)
    self + n
  end

  def prev_day(n = 1)
    self - n
  end

  def next_month(n = 1)
    self >> n
  end

  def prev_month(n = 1)
    self << n
  end

  def next_year(n = 1)
    self >> (n * 12)
  end

  def prev_year(n = 1)
    self << (n * 12)
  end

  def succ
    self + 1
  end
  alias next succ

  # --- Comparison ---

  def <=>(other)
    if other.is_a?(Date)
      @jd <=> other.jd
    elsif other.respond_to?(:to_date)
      @jd <=> other.to_date.jd
    else
      nil
    end
  end

  def ==(other)
    if other.is_a?(Date)
      @jd == other.jd
    else
      false
    end
  end

  def eql?(other)
    other.is_a?(Date) && @jd == other.jd
  end

  def hash
    @jd.hash
  end

  # --- Formatting ---

  def to_s
    format('%04d-%02d-%02d', year, month, day)
  end

  def inspect
    "#<Date: #{to_s}>"
  end

  def iso8601
    to_s
  end

  def strftime(fmt = '%F')
    result = fmt.dup
    result = result.gsub('%Y', format('%04d', year))
    result = result.gsub('%C', format('%02d', year / 100))
    result = result.gsub('%y', format('%02d', year % 100))
    result = result.gsub('%m', format('%02d', month))
    result = result.gsub('%d', format('%02d', day))
    result = result.gsub('%e', format('%2d', day))
    result = result.gsub('%j', format('%03d', yday))
    result = result.gsub('%w', wday.to_s)
    result = result.gsub('%u', (wday == 0 ? 7 : wday).to_s)
    result = result.gsub('%A', DAYNAMES[wday])
    result = result.gsub('%a', ABBR_DAYNAMES[wday])
    result = result.gsub('%B', MONTHNAMES[month])
    result = result.gsub('%b', ABBR_MONTHNAMES[month])
    result = result.gsub('%h', ABBR_MONTHNAMES[month])
    result = result.gsub('%F', format('%04d-%02d-%02d', year, month, day))
    result = result.gsub('%D', format('%02d/%02d/%02d', month, day, year % 100))
    result = result.gsub('%x', format('%02d/%02d/%02d', month, day, year % 100))
    result = result.gsub('%n', "\n")
    result = result.gsub('%t', "\t")
    result = result.gsub('%%', '%')
    result
  end

  # --- Conversion ---

  def to_date
    self
  end

  def to_datetime
    DateTime.new(year, month, day, 0, 0, 0, 0)
  end

  def to_time
    Time.local(year, month, day)
  end

  # --- Iteration ---

  def upto(max, &block)
    return to_enum(:upto, max) unless block
    d = self
    while d <= max
      block.call(d)
      d = d.succ
    end
    self
  end

  def downto(min, &block)
    return to_enum(:downto, min) unless block
    d = self
    while d >= min
      block.call(d)
      d = d.prev_day
    end
    self
  end

  # --- Day of week constants (for cwday compatibility) ---
  def cwday
    d = wday
    d == 0 ? 7 : d
  end

  def cweek
    # ISO 8601 week number
    # Thursday of the current week determines the year
    jan1 = Date.new(year, 1, 1)
    jan1_wday = jan1.wday
    # ISO weekday: Monday=1..Sunday=7
    jan1_iso = jan1_wday == 0 ? 7 : jan1_wday
    this_iso = wday == 0 ? 7 : wday
    # Day of year
    doy = yday
    # Calculate week number
    wk = (doy - this_iso + 10) / 7
    if wk < 1
      # Last week of previous year
      wk = Date.new(year - 1, 12, 31).cweek
    elsif wk > 52
      # Check if it's actually week 1 of next year
      dec31 = Date.new(year, 12, 31)
      dec31_iso = dec31.wday == 0 ? 7 : dec31.wday
      if dec31_iso < 4
        wk = 1
      end
    end
    wk
  end

  def cwyear
    wk = cweek
    if month == 1 && wk > 50
      year - 1
    elsif month == 12 && wk == 1
      year + 1
    else
      year
    end
  end
end

class DateTime < Date
  # DateTime adds hour, minute, second, and offset to Date.

  def self.new(year = -4712, month = 1, day = 1, hour = 0, minute = 0, second = 0, offset = 0, start = ITALY)
    unless _valid_civil?(year, month, day)
      raise ArgumentError, "invalid date"
    end
    jd = _civil_to_jd(year, month, day)
    obj = allocate
    obj.__send__(:_init_datetime, jd, hour, minute, second, offset, start)
    obj
  end

  def self.civil(year = -4712, month = 1, day = 1, hour = 0, minute = 0, second = 0, offset = 0, start = ITALY)
    new(year, month, day, hour, minute, second, offset, start)
  end

  def self.now(start = ITALY)
    t = Time.now
    # offset as a Rational fraction of a day
    offset_sec = t.strftime('%z').to_i
    offset_hours = offset_sec / 100
    offset_minutes = offset_sec % 100
    offset_rational = (offset_hours * 3600 + offset_minutes * 60).to_f / 86400
    new(t.year, t.month, t.day, t.strftime('%H').to_i, t.strftime('%M').to_i, t.strftime('%S').to_i, offset_rational, start)
  end

  def self.parse(str, comp = true, start = ITALY)
    # Try ISO 8601: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS
    if str =~ /\A(\d{4})-(\d{1,2})-(\d{1,2})[T ](\d{1,2}):(\d{2}):(\d{2})(?:([+-]\d{2}):?(\d{2}))?\z/
      y, m, d = $1.to_i, $2.to_i, $3.to_i
      hh, mm, ss = $4.to_i, $5.to_i, $6.to_i
      if $7
        oh = $7.to_i
        om = $8.to_i
        off = (oh * 3600 + (oh < 0 ? -om : om) * 60).to_f / 86400
      else
        off = 0
      end
      return new(y, m, d, hh, mm, ss, off, start)
    end
    # Fall back to Date.parse for date-only strings
    d = Date.parse(str, comp, start)
    new(d.year, d.month, d.day, 0, 0, 0, 0, start)
  end

  def _init_datetime(jd, hour, minute, second, offset, start)
    _init(jd, start)
    @hour = hour
    @minute = minute
    @second = second
    @offset = offset
  end
  private :_init_datetime

  def hour
    @hour
  end

  def minute
    @minute
  end
  alias min minute

  def second
    @second
  end
  alias sec second

  def offset
    @offset
  end

  def zone
    if @offset == 0
      '+00:00'
    else
      total_seconds = (@offset * 86400).round
      sign = total_seconds < 0 ? '-' : '+'
      total_seconds = total_seconds.abs
      h = total_seconds / 3600
      m = (total_seconds % 3600) / 60
      format('%s%02d:%02d', sign, h, m)
    end
  end

  def to_s
    format('%04d-%02d-%02dT%02d:%02d:%02d%s', year, month, day, @hour, @minute, @second, zone)
  end

  def inspect
    "#<DateTime: #{to_s}>"
  end

  def iso8601
    to_s
  end

  def strftime(fmt = '%FT%T%:z')
    result = super(fmt)
    result = result.gsub('%H', format('%02d', @hour))
    result = result.gsub('%M', format('%02d', @minute))
    result = result.gsub('%S', format('%02d', @second))
    result = result.gsub('%T', format('%02d:%02d:%02d', @hour, @minute, @second))
    result = result.gsub('%R', format('%02d:%02d', @hour, @minute))
    result = result.gsub('%:z', zone)
    result = result.gsub('%z', zone.delete(':'))
    result
  end

  def to_date
    Date.new(year, month, day, start)
  end

  def to_datetime
    self
  end

  def to_time
    Time.local(year, month, day, @hour, @minute, @second)
  end

  # Arithmetic for DateTime keeps the time part
  def +(n)
    if n.is_a?(Integer)
      dt_jd = jd + n
    else
      dt_jd = jd + n.to_i
    end
    DateTime.new(*self.class._jd_to_civil(dt_jd), @hour, @minute, @second, @offset, start)
  end

  def -(other)
    if other.is_a?(Date)
      # Return difference in days (as a rational for DateTime)
      if other.is_a?(DateTime)
        day_diff = jd - other.jd
        time_diff = (@hour - other.hour) * 3600 + (@minute - other.minute) * 60 + (@second - other.second)
        day_diff + time_diff.to_f / 86400
      else
        jd - other.jd
      end
    elsif other.is_a?(Integer)
      self + (-other)
    else
      self + (-other.to_i)
    end
  end
end

# --- Time extensions ---
# These are loaded when 'date' is required, matching CRuby behavior.

class Time
  def to_date
    Date.new(year, month, day)
  end

  def to_datetime
    offset_str = strftime('%z')
    oh = offset_str[0, 3].to_i
    om = offset_str[3, 2].to_i
    off = (oh * 3600 + (oh < 0 ? -om : om) * 60).to_f / 86400
    DateTime.new(year, month, day, strftime('%H').to_i, strftime('%M').to_i, strftime('%S').to_i, off)
  end
end
