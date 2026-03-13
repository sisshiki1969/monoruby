# frozen_string_literal: true
#
# ActiveSupport core extensions for Integer/Numeric.
# Duration and byte helpers.
#

require 'active_support/inflector'

class Integer
  def ordinalize
    ActiveSupport::Inflector.ordinalize(self)
  end

  def ordinal
    ActiveSupport::Inflector.ordinal(self)
  end

  # Multiple/plural checking
  def multiple_of?(number)
    number != 0 ? self % number == 0 : zero?
  end
end

class Numeric
  # Byte size helpers
  def bytes
    self
  end
  alias byte bytes

  def kilobytes
    self * 1024
  end
  alias kilobyte kilobytes

  def megabytes
    self * 1024 * 1024
  end
  alias megabyte megabytes

  def gigabytes
    self * 1024 * 1024 * 1024
  end
  alias gigabyte gigabytes

  def terabytes
    self * 1024 * 1024 * 1024 * 1024
  end
  alias terabyte terabytes

  # Duration helpers - return seconds
  def seconds
    self
  end
  alias second seconds

  def minutes
    self * 60
  end
  alias minute minutes

  def hours
    self * 3600
  end
  alias hour hours

  def days
    self * 86400
  end
  alias day days

  def weeks
    self * 604800
  end
  alias week weeks

  def fortnights
    self * 1209600
  end
  alias fortnight fortnights

  # Time helpers
  def ago(time = nil)
    t = time || Time.now
    t - self
  end

  def from_now(time = nil)
    t = time || Time.now
    t + self
  end
  alias since from_now
  alias after from_now

  def until(time = nil)
    ago(time)
  end
  alias before until
end
