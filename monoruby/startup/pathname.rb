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

  alias to_s to_str

  def expand_path(default_dir = '.')
    Pathname.new(File.expand_path(path, default_dir))
  end

  def basename(suffix = "")
    Pathname.new(File.basename(path, suffix))
  end
end