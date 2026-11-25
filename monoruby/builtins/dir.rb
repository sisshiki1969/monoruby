# monoruby-hidden-ivars: true
#
# A Dir's state — the path it was opened with, its descriptor, the
# entries read from it and the read position — is in hidden instance
# variables (doc/hidden_ivars.md): CRuby keeps it in a C struct, where
# `instance_variables` does not list it and a subclass's own `@path`
# does not overwrite it. Hidden slots are not what `freeze` protects, so
# the methods that CRuby guards with `rb_check_frozen` (everything that
# goes through `GetDIR`) check for themselves.
class Dir
  # A C struct in CRuby, with no `_dump_data`: `Marshal.dump` refuses it.
  @__no_dump_data = true
  include Enumerable

  def self.open(path, encoding: nil, &block)
    dir = new(path, encoding: encoding)
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
    # `rb_to_encoding`, before the path is looked at: an unknown name is
    # an ArgumentError and must not leave a descriptor open.
    encoding = Encoding.find(encoding) unless encoding.nil? || Encoding === encoding
    path = __to_path(path)
    @__path = path
    # Every Dir holds a real O_DIRECTORY|O_CLOEXEC descriptor (like
    # CRuby's DIR*): #fileno returns it, #close closes it at the
    # close(2) level, and Dir.for_fd can share it.
    @__fd = Dir.__open_fd(path)
    @__entries = Dir.__entries_fd(@__fd, encoding)
    @__pos = 0
    @__closed = false
  end

  # Wrap an existing directory file descriptor (no dup: closing this Dir
  # closes the caller's fd, and a second close raises Errno::EBADF).
  def self.for_fd(fd)
    fd = __num2long(fd)
    dir = allocate
    dir.__setup_fd(fd)
    dir
  end

  def __setup_fd(fd)
    @__path = nil
    @__fd = fd
    @__entries = Dir.__entries_fd(fd, nil)
    @__pos = 0
    @__closed = false
  end

  def fileno
    __dir_check if @__closed != false || frozen?
    @__fd
  end

  def read
    __dir_check if @__closed != false || frozen?
    return nil if @__pos >= @__entries.length
    entry = @__entries[@__pos]
    @__pos += 1
    entry
  end

  def each(&block)
    return to_enum(:each) unless block
    __dir_check if @__closed != false || frozen?
    # #each always yields every entry (so repeated calls give the same result),
    # then leaves the read cursor at end-of-stream so a following #read returns
    # nil (core/dir/each_spec.rb).
    @__entries.each { |e| block.call(e) }
    @__pos = @__entries.length
    self
  end

  # The instance forms take no arguments in CRuby — the encoding was
  # already decided when the Dir was opened (`Dir.new(path, encoding:)`),
  # so `d.children(encoding: ...)` is an ArgumentError there.
  def children
    __dir_check if @__closed != false || frozen?
    @__entries.reject { |e| e == "." || e == ".." }
  end

  def each_child(&block)
    return to_enum(:each_child) unless block
    __dir_check if @__closed != false || frozen?
    children.each { |e| block.call(e) }
    self
  end

  def rewind
    __dir_check if @__closed != false || frozen?
    @__pos = 0
    self
  end

  def pos
    __dir_check if @__closed != false || frozen?
    @__pos
  end

  alias tell pos

  def pos=(newpos)
    __dir_check if @__closed != false || frozen?
    @__pos = newpos
  end

  # Unlike `pos=` (which yields the assigned value), Dir#seek returns the Dir
  # itself (core/dir/seek_spec.rb).
  def seek(newpos)
    self.pos = newpos
    self
  end

  def close
    __dir_check_frozen
    return nil if @__closed != false
    @__closed = true
    Dir.__close_fd(@__fd) if @__fd
    nil
  end

  def path
    @__path
  end

  def chdir(&block)
    __dir_check if @__closed != false || frozen?
    __chdir(&block)
  end

  # A copy is a Dir that was never opened, as in CRuby (whose `dup`
  # allocates a fresh struct and copies nothing into it) — and must not
  # share the descriptor the original will close.
  private def initialize_copy(_)
    @__path = @__fd = @__entries = @__closed = nil
    @__pos = 0
  end

  # CRuby's `GetDIR`: `rb_check_frozen`, then "closed directory" for a
  # Dir that is not open — closed, or never opened (`Dir.allocate`).
  # Callers test the two conditions inline first, so an open, unfrozen
  # Dir pays no method call per `read`.
  private def __dir_check
    __dir_check_frozen
    raise IOError, "closed directory" if @__closed != false
  end

  private def __dir_check_frozen
    if frozen?
      raise FrozenError.new("can't modify frozen #{self.class}: #{inspect}", receiver: self)
    end
  end

  alias to_path path

  def inspect
    return super if @__path.nil?
    __sprintf_repr("#<Dir:#{@__path}>")
  end

  def self.children(path, encoding: nil)
    entries(path, encoding: encoding).reject { |e| e == "." || e == ".." }
  end

  def self.each_child(path, encoding: nil, &block)
    # The Enumerator replays a keyword as a keyword (#1467), so this
    # needs no spare positional for the encoding to ride in.
    unless block
      return encoding.nil? ? to_enum(:each_child, path)
                           : to_enum(:each_child, path, encoding: encoding)
    end
    children(path, encoding: encoding).each { |e| block.call(e) }
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
