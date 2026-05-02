# Minimal Pathname C-extension stub for monoruby.
#
# The real pathname.so provides core methods in C. The Ruby part in
# pathname.rb loads this via `require "pathname.so"`. We only need
# enough for ActiveRecord's SQLite3 adapter to resolve database paths.

class Pathname
  def initialize(path)
    @path = path.to_s
  end

  def to_s; @path; end
  alias to_str to_s
  alias to_path to_s

  def ==(other)
    other.is_a?(Pathname) && @path == other.to_s
  end
  alias eql? ==

  def hash; @path.hash; end

  def +(other)
    Pathname.new(File.join(@path, other.to_s))
  end
  alias / +

  def join(*args)
    result = self
    args.each { |a| result = result + a }
    result
  end

  def basename(suffix = nil)
    Pathname.new(suffix ? File.basename(@path, suffix) : File.basename(@path))
  end

  def dirname
    Pathname.new(File.dirname(@path))
  end

  def extname
    File.extname(@path)
  end

  def expand_path(dir = nil)
    Pathname.new(File.expand_path(@path, dir))
  end

  def absolute?
    @path.start_with?("/")
  end

  def relative?
    !absolute?
  end

  def exist?
    File.exist?(@path)
  end

  def directory?
    File.directory?(@path)
  end

  def file?
    File.file?(@path)
  end

  def to_a
    @path.split("/").reject(&:empty?).map { |s| Pathname.new(s) }
  end

  def inspect
    "#<Pathname:#{@path}>"
  end

  def cleanpath
    Pathname.new(@path)
  end

  def realpath
    Pathname.new(File.realpath(@path))
  end

  def sub_ext(repl)
    Pathname.new(@path.sub(/\.[^.]*\z/, repl))
  end

  def each_child(with_directory = true, &block)
    Dir.entries(@path).each do |e|
      next if e == "." || e == ".."
      entry = with_directory ? self + e : Pathname.new(e)
      block.call(entry)
    end
  end

  def children(with_directory = true)
    result = []
    each_child(with_directory) { |c| result << c }
    result
  end
end
