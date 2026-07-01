require_relative 'monitor'

class Enumerator
  include Enumerable
end

class CallerLocation
  def initialize(path, lineno, label)
    @path = path
    @lineno = lineno
    @label = label
  end
  attr_reader :path, :lineno, :label

  def base_label
    l = @label
    l = $1 while l =~ /\Ablock in (.+)\z/
    l
  end

  def to_s
    "#{@path}:#{@lineno}:in '#{@label}'"
  end
  alias inspect to_s
end

def caller_locations(start = 1, length = nil)
  frames = caller(start + 1)
  return nil if frames.nil?
  frames = frames[0, length] if length
  frames.map do |frame|
    if frame =~ /\A(.+):(\d+):in ['`](.+)'\z/
      CallerLocation.new($1, $2.to_i, $3)
    else
      CallerLocation.new(frame, 0, "")
    end
  end
end

# `^`, `|`, `&` for `true` and `false` live on the internal `Boolean`
# parent class so that `true.method(:&) == false.method(:&)` and the
# JIT inline cache can treat the receiver as `BOOL_CLASS` regardless of
# which boolean was observed first.

class NilClass
  def |(other)
    !!other
  end
  # ruby/spec core/nil/xor_spec.rb: ^ is an alias of |.
  alias ^ |

  def &(other)
    false
  end

  def to_i
    0
  end
end

class Hash
  include Enumerable

  def to_hash
    self
  end

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    unless block_given?
      return self if self.instance_of?(Hash)
      h = Hash[self]
      h.compare_by_identity if compare_by_identity?
      if (dp = default_proc)
        h.default_proc = dp
      else
        h.default = default
      end
      return h
    end
    h = {}
    self.each {|k, v|
      pair = yield k, v
      pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
      raise TypeError, "wrong element type #{pair.class} (expected array)" unless pair.is_a?(Array)
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
      h[pair[0]] = pair[1]
    }
    h
  end

  def transform_keys(hash = nil, &block)
    return to_enum(:transform_keys) { size } unless block || hash
    h = {}
    if hash
      each do |k, v|
        new_k = hash.key?(k) ? hash[k] : (block ? block.call(k) : k)
        h[new_k] = v
      end
    else
      each { |k, v| h[block.call(k)] = v }
    end
    h
  end

  def transform_keys!(hash = nil, &block)
    return to_enum(:transform_keys!) { size } unless block || hash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    if hash
      keys.each do |k|
        if hash.key?(k)
          self[hash[k]] = delete(k)
        elsif block
          self[block.call(k)] = delete(k)
        end
      end
    else
      keys.each { |k| self[block.call(k)] = delete(k) }
    end
    self
  end

  def transform_values(&block)
    return to_enum(:transform_values) { size } unless block
    h = {}
    h.compare_by_identity if compare_by_identity?
    each { |k, v| h[k] = block.call(v) }
    h
  end

  def transform_values!(&block)
    return to_enum(:transform_values!) { size } unless block
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    each { |k, v| self[k] = block.call(v) }
    self
  end

  def slice(*keys)
    h = {}
    h.compare_by_identity if compare_by_identity?
    # CRuby's Hash#slice uses the internal element reference, not a
    # subclass-overridden #[], so bind the base Hash#[].
    aref = ::Hash.instance_method(:[])
    keys.each { |k| h[k] = aref.bind(self).call(k) if key?(k) }
    h
  end

  def except(*keys)
    h = dup
    h.default = nil
    keys.each { |k| h.delete(k) }
    h
  end

  def dig(key, *rest)
    val = self[key]
    return val if rest.empty? || val.nil?
    raise TypeError, "#{val.class} does not have #dig method" unless val.respond_to?(:dig)
    val.dig(*rest)
  end

  def each_with_object(obj)
    return to_enum(:each_with_object, obj) unless block_given?
    each { |k, v| yield [k, v], obj }
    obj
  end

  def any?(*pattern)
    if !pattern.empty?
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)" if pattern.size != 1
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      each { |k, v| return true if pat === [k, v] }
    elsif block_given?
      each { |k, v| return true if yield([k, v]) }
    else
      return !empty?
    end
    false
  end

  def all?(*pattern)
    if !pattern.empty?
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)" if pattern.size != 1
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      each { |k, v| return false unless pat === [k, v] }
    elsif block_given?
      each { |k, v| return false unless yield([k, v]) }
    else
      each { |k, v| return false unless [k, v] }
    end
    true
  end

  def count(*args)
    if block_given?
      n = 0
      each { |k, v| n += 1 if yield([k, v]) }
      n
    elsif args.empty?
      size
    else
      n = 0
      target = args[0]
      each { |k, v| n += 1 if [k, v] == target }
      n
    end
  end

  def map(&blk)
    return to_enum(:map) { size } unless blk
    result = []
    if blk.arity == 1
      each { |pair| result << blk.call(pair) }
    else
      each { |pair| result << blk.call(*pair) }
    end
    result
  end
  alias collect map

  def flat_map
    return to_enum(:flat_map) unless block_given?
    res = []
    each { |k, v|
      r = yield(k, v)
      if r.is_a?(Array)
        res.concat(r)
      else
        res << r
      end
    }
    res
  end

  def min_by
    return to_enum(:min_by) unless block_given?
    min_entry = nil
    min_val = nil
    each do |k, v|
      val = yield(k, v)
      if min_val.nil? || (val <=> min_val) < 0
        min_entry = [k, v]
        min_val = val
      end
    end
    min_entry
  end

  def has_value?(value)
    each_value { |v| return true if v == value }
    false
  end
  alias value? has_value?

  def deconstruct_keys(keys)
    self
  end

  def compact
    h = {}
    h.compare_by_identity if compare_by_identity?
    each { |k, v| h[k] = v unless v.nil? }
    if (dp = default_proc)
      h.default_proc = dp
    else
      h.default = default
    end
    h
  end

  def compact!
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    drop = []
    each { |k, v| drop << k if v.nil? }
    return nil if drop.empty?
    drop.each { |k| delete(k) }
    self
  end

  def flatten(level = 1)
    level = level.to_int if level.respond_to?(:to_int) && !level.is_a?(Integer)
    raise TypeError, "no implicit conversion of #{level.class} into Integer" unless level.is_a?(Integer)
    to_a.flatten(level)
  end

  def fetch_values(*keys, &block)
    keys.map { |k| fetch(k, &block) }
  end

  def to_proc
    hash = self
    ->(k) { hash[k] }
  end

  def rehash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    pairs = to_a
    clear
    pairs.each { |k, v| self[k] = v }
    self
  end

  def self.ruby2_keywords_hash?(h)
    raise TypeError, "no implicit conversion of #{h.class} into Hash" unless h.is_a?(Hash)
    false
  end

  def self.ruby2_keywords_hash(h)
    raise TypeError, "no implicit conversion of #{h.class} into Hash" unless h.is_a?(Hash)
    h.dup
  end
end


class File
  # File::Stat — the fields (@dev, @ino, @mode, @nlink, @uid, @gid,
  # @rdev, @size, @blksize, @blocks, @atime, @mtime, @ctime) are
  # populated from a real stat(2)/lstat(2) by the Rust constructor
  # (`File.stat`, `File.lstat`, `File::Stat.new`). The accessors and
  # mode-bit predicates below read those fields.
  class Stat
    include Comparable

    attr_reader :dev, :ino, :mode, :nlink, :uid, :gid, :rdev,
                :size, :blksize, :blocks, :atime, :mtime, :ctime

    S_IFMT   = 0o170000
    S_IFSOCK = 0o140000
    S_IFLNK  = 0o120000
    S_IFREG  = 0o100000
    S_IFBLK  = 0o060000
    S_IFDIR  = 0o040000
    S_IFCHR  = 0o020000
    S_IFIFO  = 0o010000

    def directory? ; (@mode & S_IFMT) == S_IFDIR  ; end
    def file?      ; (@mode & S_IFMT) == S_IFREG  ; end
    def symlink?   ; (@mode & S_IFMT) == S_IFLNK  ; end
    def blockdev?  ; (@mode & S_IFMT) == S_IFBLK  ; end
    def chardev?   ; (@mode & S_IFMT) == S_IFCHR  ; end
    def pipe?      ; (@mode & S_IFMT) == S_IFIFO  ; end
    def socket?    ; (@mode & S_IFMT) == S_IFSOCK ; end

    def ftype
      case @mode & S_IFMT
      when S_IFREG  then "file"
      when S_IFDIR  then "directory"
      when S_IFCHR  then "characterSpecial"
      when S_IFBLK  then "blockSpecial"
      when S_IFIFO  then "fifo"
      when S_IFLNK  then "link"
      when S_IFSOCK then "socket"
      else "unknown"
      end
    end

    def setuid?  ; (@mode & 0o4000) != 0 ; end
    def setgid?  ; (@mode & 0o2000) != 0 ; end
    def sticky?  ; (@mode & 0o1000) != 0 ; end

    def world_readable?
      (@mode & 0o004) != 0 ? (@mode & 0o777) : nil
    end

    def world_writable?
      (@mode & 0o002) != 0 ? (@mode & 0o777) : nil
    end

    def zero?     ; @size == 0 ; end
    def size?     ; @size == 0 ? nil : @size ; end

    def owned?    ; @uid == Process.euid ; end
    def grpowned? ; @gid == Process.egid ; end

    def readable?       ; (@mode & 0o400) != 0 ; end
    def readable_real?  ; readable? ; end
    def writable?       ; (@mode & 0o200) != 0 ; end
    def writable_real?  ; writable? ; end
    def executable?     ; (@mode & 0o100) != 0 ; end
    def executable_real? ; executable? ; end

    def dev_major  ; @dev >> 8 ; end
    def dev_minor  ; @dev & 0xff ; end
    def rdev_major ; @rdev >> 8 ; end
    def rdev_minor ; @rdev & 0xff ; end

    def birthtime
      raise NotImplementedError, "birthtime() function is unimplemented"
    end

    def <=>(other)
      return nil unless other.is_a?(File::Stat)
      @mtime <=> other.mtime
    end

    def inspect
      "#<File::Stat dev=0x#{@dev.to_s(16)}, ino=#{@ino}, mode=0#{@mode.to_s(8)}, " \
      "nlink=#{@nlink}, uid=#{@uid}, gid=#{@gid}, rdev=0x#{@rdev.to_s(16)}, " \
      "size=#{@size}, blksize=#{@blksize}, blocks=#{@blocks}, " \
      "atime=#{@atime}, mtime=#{@mtime}, ctime=#{@ctime}>"
    end
  end
end

class Proc
  def >>(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    f = self
    if f.lambda?
      lambda { |*a, &b| g.call(f.call(*a, &b)) }
    else
      proc { |*a, &b| g.call(f.call(*a, &b)) }
    end
  end

  def <<(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    f = self
    if g.is_a?(Proc) && !g.lambda?
      proc { |*a, &b| f.call(g.call(*a, &b)) }
    else
      lambda { |*a, &b| f.call(g.call(*a, &b)) }
    end
  end
end

class Method
  def >>(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    me = self
    lambda { |*a, &b| g.call(me.call(*a, &b)) }
  end

  def <<(g)
    unless g.is_a?(Proc) || g.is_a?(Method) || g.respond_to?(:call, true)
      raise TypeError, "callable object is expected"
    end
    me = self
    if g.is_a?(Proc) && !g.lambda?
      proc { |*a, &b| me.call(g.call(*a, &b)) }
    else
      lambda { |*a, &b| me.call(g.call(*a, &b)) }
    end
  end

  def curry(arity = nil)
    pr = to_proc
    return pr.curry(self.arity) if arity.nil?
    params = parameters
    req = params.count { |t, _| t == :req }
    opt = params.count { |t, _| t == :opt }
    has_rest = params.any? { |t, _| t == :rest }
    max = has_rest ? Float::INFINITY : req + opt
    if arity < req || arity > max
      raise ArgumentError, "wrong number of arguments (given #{arity}, expected #{req})"
    end
    pr.curry(arity)
  end
end

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

class Array
  def values_at(*selectors)
    result = []
    selectors.each do |s|
      if s.is_a?(Range)
        b = s.begin
        e = s.end
        b = b.nil? ? 0 : b.to_int
        endless = e.nil?
        e = endless ? size - 1 : e.to_int
        b += size if b < 0
        e += size if e < 0
        e -= 1 if !endless && s.exclude_end?
        next if b < 0
        i = b
        while i <= e
          result << self[i]
          i += 1
        end
      else
        result << self[s]
      end
    end
    result
  end
end

class File
  # Instance timestamp readers (core/file/{atime,mtime,ctime,birthtime}_spec.rb).
  # monoruby only defined the File.<name>(path) class methods; delegate the
  # instance form through the receiver's #path.
  def atime;     File.atime(path);     end
  def mtime;     File.mtime(path);     end
  def ctime;     File.ctime(path);     end
  def birthtime; File.birthtime(path); end

  # Instance chmod/chown (core/file/{chmod,chown}_spec.rb): only the class
  # forms existed. Delegate through #path and return 0, as CRuby's instance
  # forms do (the class forms return the number of files affected).
  def chmod(mode);         File.chmod(mode, path);          0; end
  def chown(owner, group); File.chown(owner, group, path);  0; end

  # Purely lexical `dirname` (core/file/dirname_spec.rb). CRuby does *not*
  # resolve `.`/`..` or collapse interior slashes; it only strips the last
  # `/component` (and the slashes immediately before it), `level` times.
  def self.dirname(path, level = 1)
    path = path.to_path if !path.is_a?(String) && path.respond_to?(:to_path)
    path = path.to_str  if !path.is_a?(String) && path.respond_to?(:to_str)
    raise TypeError, "no implicit conversion of #{path.class} into String" unless path.is_a?(String)
    unless level.is_a?(Integer)
      unless level.respond_to?(:to_int)
        raise TypeError, "no implicit conversion of #{level.class} into Integer"
      end
      level = level.to_int
    end
    raise ArgumentError, "negative level: #{level}" if level < 0
    result = path
    level.times do
      prev = result
      result = _dirname_once(result)
      break if result == prev
    end
    result
  end

  def self._dirname_once(path)
    return "/" if path =~ %r{\A/+\z}   # all slashes (incl. "/")
    s = path.sub(%r{/+\z}, "")         # drop trailing slashes
    return "." if s.empty?             # empty path
    idx = s.rindex("/")
    return "." if idx.nil?             # no directory part
    prefix = s[0...idx].sub(%r{/+\z}, "")
    prefix.empty? ? "/" : prefix       # empty prefix only for absolute paths
  end
  private_class_method :_dirname_once

  def self.zero?(path)
    # A missing file is not an error here (returns false), but a non-path
    # argument (nil/true/Integer/...) must still raise the TypeError that
    # `File.size` produces — so rescue only filesystem errors, not everything.
    begin
      s = File.size(path)
    rescue SystemCallError
      return false
    end
    s == 0
  end

  def self.empty?(path)
    File.zero?(path)
  end

  def self.readable_real?(path)
    File.readable?(path)
  end

  def self.writable_real?(path)
    File.writable?(path)
  end

  def self.executable_real?(path)
    File.executable?(path)
  end
end

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

# True aliases required by ruby/spec's "Move <Class> to rely less on shared
# examples" refactoring wave (2026-06; e.g. ruby/spec d714c5a84 for Integer and
# its siblings for Float/Symbol/Set/...), which replaced behavioral shared
# examples with strict `Klass.instance_method(:a) == Klass.instance_method(:b)`
# identity checks. monoruby had registered several of these as separate builtins
# sharing one Rust fn (distinct FuncId), so the identity checks failed. Redefine
# them as real aliases, matching CRuby. (Integer's are handled in
# `builtins/numeric/integer.rs`.) This file loads after every class .rb, so the
# aliased originals are all defined by now.
class Float
  alias === ==
  alias modulo %
  alias quo fdiv
end
class Symbol
  alias === ==
  alias id2name to_s
  alias intern to_sym
  alias size length
end
class Rational
  alias magnitude abs
end
class Hash
  alias store []=
end
class Set
  alias < proper_subset?
  alias > proper_superset?
  alias << add
  alias eql? ==
end
class Time
  alias asctime ctime
end
class Encoding
  alias to_s name
end
class MatchData
  alias deconstruct captures
end
class Struct
  alias inspect to_s
end
class TrueClass
  # core/true/inspect_spec.rb: inspect is an alias of to_s (to_s on TrueClass).
  alias inspect to_s
end
class FalseClass
  # core/false/inspect_spec.rb: inspect is an alias of to_s (to_s on FalseClass).
  alias inspect to_s
  # core/false/xor_spec.rb: ^ is an alias of |. Both `|` and `^` are rooted
  # directly on FalseClass (registered by bool_class.rs on FALSE_CLASS, not an
  # inherited parent), so just re-point `^` at the existing `|`. For false,
  # `false ^ x` and `false | x` are both `!!x`, so behaviour is unchanged.
  # (`true` keeps distinct `^`/`|` on the shared Boolean, where they differ.)
  alias ^ |
end

# The remaining aliases need their *original* defined on the class, because
# CRuby keeps the original there too (the strict identity check compares the
# owner). monoruby inherits them, so we re-root the original on the class (a
# `super`-forwarding stub preserves behaviour) and then alias.
#
# NOT done here: Complex#quo == Complex#/. CRuby keeps both on Complex as one
# method; in monoruby `quo` (Rational-component result) and the inherited
# Numeric#/ (and the `/` *operator*, which has a separate fast path that is
# itself buggy — e.g. `Complex(1,2) / 2` => `(0+1i)`) are genuinely different
# implementations, so aliasing them changes results. Tracked as an issue.
class Enumerator
  # identical to with_object; re-root on Enumerator (overrides Enumerable's).
  alias each_with_object with_object
end
class Thread
  alias exit kill
  # inherited Kernel#inspect dumps Thread's ivars; use the short Kernel#to_s
  # form for both (matches CRuby's `inspect`==`to_s`).
  def to_s = super
  alias inspect to_s
end

# ENV is a singleton object (not a class), so its methods live on its
# singleton class. ruby/spec checks several of them for strict alias identity
# (`ENV.method(:a) == ENV.method(:b)`). monoruby registered these as separate
# Rust builtins sharing one fn (distinct FuncId), so re-point them as real
# aliases here. The originals (`include?`, `value?`, `[]=`, `merge!`) are all
# registered by `hash.rs` before this file loads.
class << ENV
  alias has_key? include?
  alias key? include?
  alias member? include?
  alias has_value? value?
  alias store []=
  alias update merge!

  # core/env/clone_spec.rb / dup_spec.rb: unlike a plain Hash, ENV cannot be
  # copied — CRuby raises TypeError from `#clone`/`#dup`. `#clone` still
  # validates its `freeze:` keyword first (ArgumentError for a non-boolean
  # value or an unknown keyword), matching Kernel#clone's signature.
  def clone(freeze: nil, **rest)
    unless rest.empty?
      raise ArgumentError, "unknown keyword: #{rest.keys.first.inspect}"
    end
    unless freeze.nil? || freeze == true || freeze == false
      raise ArgumentError, "unexpected value for freeze: #{freeze.class}"
    end
    raise TypeError, "Cannot clone ENV, use ENV.to_h to get a copy of ENV as a hash"
  end

  def dup
    raise TypeError, "Cannot dup ENV, use ENV.to_h to get a copy of ENV as a hash"
  end

  # Hash#except is implemented via #dup, which ENV now forbids. CRuby's
  # ENV.except returns a plain Hash snapshot, so route it through #to_h.
  def except(*keys)
    to_h.except(*keys)
  end
end
