module Benchmark

  BENCHMARK_VERSION = "2002-04-25" # :nodoc:

  def benchmark(caption = "", label_width = nil, format = nil, *labels) # :yield: report
    sync = $stdout.sync
    $stdout.sync = true
    label_width ||= 0
    label_width += 1
    format ||= FORMAT
    print ' '*label_width + caption unless caption.empty?
    report = Report.new(label_width, format)
    results = yield(report)
    Array === results and results.grep(Tms).each {|t|
      print((labels.shift || t.label || "").ljust(label_width), t.format(format))
    }
    report.list
  ensure
    $stdout.sync = sync unless sync.nil?
  end

  def bm(label_width = 0, *labels, &blk) # :yield: report
    benchmark(CAPTION, label_width, FORMAT, *labels, &blk)
  end

  def bmbm(width = 0) # :yield: job
    job = Job.new(width)
    yield(job)
    width = job.width + 1
    sync = $stdout.sync
    $stdout.sync = true

    # rehearsal
    puts 'Rehearsal '.ljust(width+CAPTION.length,'-')
    ets = job.list.inject(Tms.new) { |sum,(label,item)|
      print label.ljust(width)
      res = Benchmark.measure(&item)
      print res.format
      sum + res
    }.format("total: %tsec")
    print " #{ets}\n\n".rjust(width+CAPTION.length+2,'-')

    # take
    print ' '*width + CAPTION
    job.list.map { |label,item|
      GC.start
      print label.ljust(width)
      Benchmark.measure(label, &item).tap { |res| print res }
    }
  ensure
    $stdout.sync = sync unless sync.nil?
  end

  def measure(label = "") # :yield:
    t0, r0 = Process.times, Process.clock_gettime(Process::CLOCK_MONOTONIC)
    yield
    t1, r1 = Process.times, Process.clock_gettime(Process::CLOCK_MONOTONIC)
    Benchmark::Tms.new(t1.utime  - t0.utime,
                       t1.stime  - t0.stime,
                       t1.cutime - t0.cutime,
                       t1.cstime - t0.cstime,
                       r1 - r0,
                       label)
  end

  def realtime # :yield:
    r0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    yield
    Process.clock_gettime(Process::CLOCK_MONOTONIC) - r0
  end

  module_function :benchmark, :measure, :realtime, :bm, :bmbm

  class Job # :nodoc:
    def initialize(width)
      @width = width
      @list = []
    end

    #
    # Registers the given label and block pair in the job list.
    #
    def item(label = "", &blk) # :yield:
      raise ArgumentError, "no block" unless block_given?
      label = label.to_s
      w = label.length
      @width = w if @width < w
      @list << [label, blk]
      self
    end

    alias report item

    # An array of 2-element arrays, consisting of label and block pairs.
    attr_reader :list

    # Length of the widest label in the #list.
    attr_reader :width
  end

  #
  # This class is used by the Benchmark.benchmark and Benchmark.bm methods.
  # It is of little direct interest to the user.
  #
  class Report # :nodoc:
    #
    # Returns an initialized Report instance.
    # Usually, one doesn't call this method directly, as new
    # Report objects are created by the #benchmark and #bm methods.
    # +width+ and +format+ are the label offset and
    # format string used by Tms#format.
    #
    def initialize(width = 0, format = nil)
      @width, @format, @list = width, format, []
    end

    #
    # Prints the +label+ and measured time for the block,
    # formatted by +format+. See Tms#format for the
    # formatting rules.
    #
    def item(label = "", *format, &blk) # :yield:
      print label.to_s.ljust(@width)
      @list << res = Benchmark.measure(label, &blk)
      print res.format(@format, *format)
      res
    end

    alias report item

    # An array of Benchmark::Tms objects representing each item.
    attr_reader :list
  end



  #
  # A data object, representing the times associated with a benchmark
  # measurement.
  #
  class Tms

    # Default caption, see also Benchmark::CAPTION
    CAPTION = "      user     system      total        real\n"

    # Default format string, see also Benchmark::FORMAT
    FORMAT = "%10.6u %10.6y %10.6t %10.6r\n"

    # User CPU time
    attr_reader :utime

    # System CPU time
    attr_reader :stime

    # User CPU time of children
    attr_reader :cutime

    # System CPU time of children
    attr_reader :cstime

    # Elapsed real time
    attr_reader :real

    # Total time, that is +utime+ + +stime+ + +cutime+ + +cstime+
    attr_reader :total

    # Label
    attr_reader :label

    #
    # Returns an initialized Tms object which has
    # +utime+ as the user CPU time, +stime+ as the system CPU time,
    # +cutime+ as the children's user CPU time, +cstime+ as the children's
    # system CPU time, +real+ as the elapsed real time and +label+ as the label.
    #
    def initialize(utime = 0.0, stime = 0.0, cutime = 0.0, cstime = 0.0, real = 0.0, label = nil)
      @utime, @stime, @cutime, @cstime, @real, @label = utime, stime, cutime, cstime, real, label.to_s
      @total = @utime + @stime + @cutime + @cstime
    end

    #
    # Returns a new Tms object whose times are the sum of the times for this
    # Tms object, plus the time required to execute the code block (+blk+).
    #
    def add(&blk) # :yield:
      self + Benchmark.measure(&blk)
    end

    #
    # An in-place version of #add.
    # Changes the times of this Tms object by making it the sum of the times
    # for this Tms object, plus the time required to execute
    # the code block (+blk+).
    #
    def add!(&blk)
      t = Benchmark.measure(&blk)
      @utime  = utime + t.utime
      @stime  = stime + t.stime
      @cutime = cutime + t.cutime
      @cstime = cstime + t.cstime
      @real   = real + t.real
      self
    end

    def +(other); memberwise(:+, other) end
    def -(other); memberwise(:-, other) end
    def *(x); memberwise(:*, x) end
    def /(x); memberwise(:/, x) end
    def format(format = nil, *args)
      str = (format || FORMAT).dup
      str.gsub!(/(%[-+.\d]*)n/) { "#{$1}s" % label }
      str.gsub!(/(%[-+.\d]*)u/) { "#{$1}f" % utime }
      str.gsub!(/(%[-+.\d]*)y/) { "#{$1}f" % stime }
      str.gsub!(/(%[-+.\d]*)U/) { "#{$1}f" % cutime }
      str.gsub!(/(%[-+.\d]*)Y/) { "#{$1}f" % cstime }
      str.gsub!(/(%[-+.\d]*)t/) { "#{$1}f" % total }
      str.gsub!(/(%[-+.\d]*)r/) { "(#{$1}f)" % real }
      format ? str % args : str
    end

    def to_s
      format
    end

    def to_a
      [@label, @utime, @stime, @cutime, @cstime, @real]
    end

    #
    # Returns a hash containing the same data as `to_a`.
    #
    def to_h
      {
        label:  @label,
        utime:  @utime,
        stime:  @stime,
        cutime: @cutime,
        cstime: @cstime,
        real:   @real
      }
    end

    protected

    #
    # Returns a new Tms object obtained by memberwise operation +op+
    # of the individual times for this Tms object with those of the other
    # Tms object (+x+).
    #
    # +op+ can be a mathematical operation such as <tt>+</tt>, <tt>-</tt>,
    # <tt>*</tt>, <tt>/</tt>
    #
    def memberwise(op, x)
      case x
      when Benchmark::Tms
        Benchmark::Tms.new(utime.__send__(op, x.utime),
                           stime.__send__(op, x.stime),
                           cutime.__send__(op, x.cutime),
                           cstime.__send__(op, x.cstime),
                           real.__send__(op, x.real)
                           )
      else
        Benchmark::Tms.new(utime.__send__(op, x),
                           stime.__send__(op, x),
                           cutime.__send__(op, x),
                           cstime.__send__(op, x),
                           real.__send__(op, x)
                           )
      end
    end
  end

  # The default caption string (heading above the output times).
  CAPTION = Benchmark::Tms::CAPTION

  # The default format string used to display times.  See also Benchmark::Tms#format.
  FORMAT = Benchmark::Tms::FORMAT
end

Benchmark.bmbm {}
