# Minimal Date/DateTime stub for monoruby
# Provides just enough functionality for sequel benchmark

class Date
  ITALY = 2299161
  ENGLAND = 2361222
  JULIAN = Float::INFINITY
  GREGORIAN = -Float::INFINITY

  MONTHNAMES = [nil, "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"]
  DAYNAMES = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
  ABBR_MONTHNAMES = [nil, "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  ABBR_DAYNAMES = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

  attr_reader :year, :month, :day

  def initialize(year = -4712, month = 1, day = 1, _sg = ITALY)
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

  def self.parse(str, comp = true)
    if str =~ /\A(\d{4})-(\d{1,2})-(\d{1,2})/
      civil($1.to_i, $2.to_i, $3.to_i)
    elsif str =~ /\A(\d{1,2})\/(\d{1,2})\/(\d{4})/
      civil($3.to_i, $1.to_i, $2.to_i)
    else
      raise ArgumentError, "invalid date: #{str.inspect}"
    end
  end

  def jd
    a = (14 - @month) / 12
    y = @year + 4800 - a
    m = @month + 12 * a - 3
    @day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045
  end

  def wday
    jd % 7
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

  def inspect
    "#<Date: #{to_s}>"
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

  def self.parse(str, comp = true)
    if str =~ /\A(\d{4})-(\d{1,2})-(\d{1,2})T(\d{1,2}):(\d{1,2}):(\d{1,2})/
      civil($1.to_i, $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i)
    elsif str =~ /\A(\d{4})-(\d{1,2})-(\d{1,2})/
      civil($1.to_i, $2.to_i, $3.to_i)
    else
      raise ArgumentError, "invalid date: #{str.inspect}"
    end
  end

  def to_s
    format("%04d-%02d-%02dT%02d:%02d:%02d+00:00", @year, @month, @day, @hour, @min, @sec)
  end

  def inspect
    "#<DateTime: #{to_s}>"
  end

  def to_time
    Time.new(@year, @month, @day, @hour, @min, @sec)
  end

  def to_date
    Date.civil(@year, @month, @day)
  end

  def to_datetime
    self
  end
end
