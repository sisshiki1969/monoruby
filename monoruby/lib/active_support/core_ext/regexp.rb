# frozen_string_literal: true
#
# ActiveSupport core extensions for Regexp.
#

class Regexp
  def multiline?
    (options & Regexp::MULTILINE) != 0
  end
end
