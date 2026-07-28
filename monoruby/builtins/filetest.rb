module FileTest
  def self.empty?(path)
    File.zero?(path)
  end

  # core/filetest/zero_spec.rb: `FileTest.zero?` is a strict alias of
  # `FileTest.empty?` (identity check `method(:zero?) == method(:empty?)`),
  # so re-point it rather than defining a second method sharing the body.
  class << self
    alias zero? empty?
  end
end
