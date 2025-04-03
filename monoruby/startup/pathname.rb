class Pathname
  def self.pwd
    Pathname.new(Dir.getwd)
  end

  class << self
    alias getwd pwd
  end

  attr_accessor :path

  def initialize(path)
    self.path = path
  end

  def to_str
    path
  end
end