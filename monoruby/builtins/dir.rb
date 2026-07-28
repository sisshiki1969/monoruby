class Dir
  include Enumerable

  def self.open(path, encoding: nil, &block)
    dir = new(path)
    if block
      begin
        result = block.call(dir)
      ensure
        dir.close
      end
      result
    else
      dir
    end
  end

  def initialize(path, encoding: nil)
    path = path.to_path if path.respond_to?(:to_path)
    path = path.to_str if path.respond_to?(:to_str)
    raise TypeError, "no implicit conversion of #{path.class} into String" unless path.is_a?(String)
    raise Errno::ENOENT, "No such file or directory @ dir_initialize - #{path}" unless File.directory?(path)
    @path = path
    @entries = Dir.entries(path)
    @pos = 0
    @closed = false
  end

  def read
    raise IOError, "closed directory" if @closed
    return nil if @pos >= @entries.length
    entry = @entries[@pos]
    @pos += 1
    entry
  end

  def each(&block)
    raise IOError, "closed directory" if @closed
    return to_enum(:each) unless block
    # #each always yields every entry (so repeated calls give the same result),
    # then leaves the read cursor at end-of-stream so a following #read returns
    # nil (core/dir/each_spec.rb).
    @entries.each { |e| block.call(e) }
    @pos = @entries.length
    self
  end

  def children(encoding: nil)
    raise IOError, "closed directory" if @closed
    @entries.reject { |e| e == "." || e == ".." }
  end

  def each_child(encoding: nil, &block)
    raise IOError, "closed directory" if @closed
    return to_enum(:each_child) unless block
    children.each { |e| block.call(e) }
    self
  end

  def rewind
    raise IOError, "closed directory" if @closed
    @pos = 0
    self
  end

  def pos
    raise IOError, "closed directory" if @closed
    @pos
  end

  alias tell pos

  def pos=(newpos)
    raise IOError, "closed directory" if @closed
    @pos = newpos
  end

  # Unlike `pos=` (which yields the assigned value), Dir#seek returns the Dir
  # itself (core/dir/seek_spec.rb).
  def seek(newpos)
    self.pos = newpos
    self
  end

  def close
    @closed = true
    nil
  end

  def path
    @path
  end

  alias to_path path

  def inspect
    "#<Dir:#{@path}>"
  end

  def self.children(path, encoding: nil)
    entries(path).reject { |e| e == "." || e == ".." }
  end

  def self.each_child(path, encoding: nil, &block)
    return to_enum(:each_child, path) unless block
    children(path).each { |e| block.call(e) }
    nil
  end

  def self.empty?(path)
    # A path that exists but is not a directory is not "empty" — it returns
    # false rather than raising (core/dir/empty_spec.rb). A missing path still
    # raises Errno::ENOENT (surfaced by `children`/`entries`).
    return false if File.exist?(path) && !File.directory?(path)
    children(path).empty?
  end
end
