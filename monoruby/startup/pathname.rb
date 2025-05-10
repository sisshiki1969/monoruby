class Pathname
  def self.pwd
    Pathname.new(Dir.getwd)
  end

  class << self
    alias getwd pwd
  end

  #attr_accessor :path

  def initialize(path)
    @path = if path.is_a?(String)
      path
    elsif path.respond_to?(:to_str)
      path.to_str
    else
      raise TypeError, "no implicit conversion of #{path.class} into String"
    end
  end

  def to_str
    @path
  end

  alias to_s to_str

  def basename(suffix = "")
    Pathname.new(File.basename(@path, suffix))
  end

  def expand_path(default_dir = '.')
    default_dir = if default_dir.is_a?(String)
      default_dir
    elsif default_dir.respond_to?(:to_str)
      default_dir.to_str
    else
      raise TypeError, "no implicit conversion of #{default_dir.class} into String"
    end
    Pathname.new(File.expand_path(@path, default_dir))
  end

  def exist?
    File.exist?(@path)
  end

  def file?
    File.file?(@path)
  end
end