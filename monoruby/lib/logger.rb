# frozen_string_literal: true
#
# Logger implementation for monoruby.
#

class Logger
  # Severity levels
  DEBUG   = 0
  INFO    = 1
  WARN    = 2
  ERROR   = 3
  FATAL   = 4
  UNKNOWN = 5

  SEV_LABEL = %w[DEBUG INFO WARN ERROR FATAL ANY].freeze

  attr_accessor :level, :progname, :formatter
  attr_reader :logdev

  def initialize(logdev = nil, shift_age = nil, shift_size = nil, **kwargs)
    @level = kwargs[:level] || DEBUG
    @progname = kwargs[:progname] || nil
    @formatter = kwargs[:formatter] || nil
    @logdev = nil
    @default_formatter = Formatter.new

    if logdev
      if logdev.is_a?(String)
        @logdev = LogDevice.new(logdev, shift_age: shift_age, shift_size: shift_size)
      else
        @logdev = LogDevice.new(logdev)
      end
    end
  end

  def debug(progname = nil, &block)
    add(DEBUG, nil, progname, &block)
  end

  def info(progname = nil, &block)
    add(INFO, nil, progname, &block)
  end

  def warn(progname = nil, &block)
    add(WARN, nil, progname, &block)
  end

  def error(progname = nil, &block)
    add(ERROR, nil, progname, &block)
  end

  def fatal(progname = nil, &block)
    add(FATAL, nil, progname, &block)
  end

  def unknown(progname = nil, &block)
    add(UNKNOWN, nil, progname, &block)
  end

  def debug?; @level <= DEBUG; end
  def info?;  @level <= INFO;  end
  def warn?;  @level <= WARN;  end
  def error?; @level <= ERROR; end
  def fatal?; @level <= FATAL; end

  def add(severity, message = nil, progname = nil, &block)
    severity ||= UNKNOWN
    return true if severity < @level

    if message.nil?
      if block
        message = block.call
      else
        message = progname
        progname = @progname
      end
    end

    msg = format_message(format_severity(severity), Time.now, progname, message)
    @logdev.write(msg) if @logdev
    true
  end
  alias log add

  def <<(msg)
    @logdev.write(msg) if @logdev
  end

  def close
    @logdev.close if @logdev
  end

  def reopen(logdev = nil)
    @logdev = nil
    if logdev
      if logdev.is_a?(String)
        @logdev = LogDevice.new(logdev)
      else
        @logdev = LogDevice.new(logdev)
      end
    end
    self
  end

  private

  def format_severity(severity)
    SEV_LABEL[severity] || 'ANY'
  end

  def format_message(severity, datetime, progname, msg)
    if @formatter
      @formatter.call(severity, datetime, progname, msg)
    else
      @default_formatter.call(severity, datetime, progname, msg)
    end
  end

  # --- Formatter ---

  class Formatter
    FORMAT = "%s, [%s #%d] %5s -- %s: %s\n"

    attr_accessor :datetime_format

    def initialize
      @datetime_format = nil
    end

    def call(severity, time, progname, msg)
      FORMAT % [
        severity[0..0],
        format_datetime(time),
        $$,
        severity,
        progname,
        msg2str(msg)
      ]
    end

    private

    def format_datetime(time)
      if @datetime_format
        time.strftime(@datetime_format)
      else
        time.strftime("%Y-%m-%dT%H:%M:%S.") + "%06d" % 0  # microseconds placeholder
      end
    end

    def msg2str(msg)
      case msg
      when String
        msg
      when Exception
        "#{msg.message} (#{msg.class})"
      else
        msg.inspect
      end
    end
  end

  # --- LogDevice ---

  class LogDevice
    attr_reader :dev, :filename

    def initialize(log = nil, shift_age: nil, shift_size: nil)
      if log.is_a?(String)
        @filename = log
        @dev = File.open(log, 'a')
      else
        @filename = nil
        @dev = log
      end
      @shift_age = shift_age
      @shift_size = shift_size
    end

    def write(msg)
      @dev.write(msg) if @dev
    rescue => e
      warn("log writing failed. #{e}")
    end

    def close
      if @filename && @dev
        @dev.close rescue nil
      end
      @dev = nil
    end
  end

  # Severity constants as a module for include
  module Severity
    DEBUG   = 0
    INFO    = 1
    WARN    = 2
    ERROR   = 3
    FATAL   = 4
    UNKNOWN = 5
  end
  include Severity
end
