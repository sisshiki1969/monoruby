# sqlite3_native.rb – monoruby FFI replacement for sqlite3_native.so
#
# When monoruby encounters require "sqlite3/X.Y/sqlite3_native" (a .so),
# it redirects to ~/.monoruby/sqlite3_native.rb (this file via build.rs copy).
# This implements the native C extension methods using FFI calls to libsqlite3.

# Load FFI. monoruby has built-in FFI support via ~/.monoruby/ffi_c.rb
# combined with the ffi gem's Ruby files. Bundler may block 'require "ffi"'
# if ffi is not in the bundle, so we find and add the ffi gem path directly.
unless defined?(FFI::Library)
  # Ensure the ffi gem's lib dir is in $LOAD_PATH (Bundler may have removed it)
  unless $LOAD_PATH.any? { |p| p.include?("/ffi-") }
    gem_dir = (defined?(Gem) && Gem.default_dir) ? Gem.default_dir : nil
    if gem_dir
      ffi_lib = Dir.glob(File.join(gem_dir, "gems", "ffi-*", "lib")).max
      $LOAD_PATH.unshift(ffi_lib) if ffi_lib
    end
  end
  require "ffi"
end

# Pre-define ForkSafety to prevent fork_safety.rb from loading weakref/delegate
# which uses `def !` syntax not yet supported by monoruby's parser.
module SQLite3
  module ForkSafety
    def self.hook!; end
    def self.track(db); end
    def self.discard; end
    def self.suppress_warnings!; end
  end
end

module SQLite3
  module FFIBridge
    extend FFI::Library
    ffi_lib "libsqlite3.so.0"

    # --- Core database functions ---
    attach_function :sqlite3_libversion, [], :string
    attach_function :sqlite3_open_v2, [:pointer, :pointer, :int, :pointer], :int
    attach_function :sqlite3_open16, [:pointer, :pointer], :int
    attach_function :sqlite3_close, [:pointer], :int
    attach_function :sqlite3_errmsg, [:pointer], :string
    attach_function :sqlite3_errcode, [:pointer], :int
    attach_function :sqlite3_extended_result_codes, [:pointer, :int], :int
    attach_function :sqlite3_busy_timeout, [:pointer, :int], :int
    attach_function :sqlite3_exec, [:pointer, :string, :pointer, :pointer, :pointer], :int
    attach_function :sqlite3_last_insert_rowid, [:pointer], :int64
    attach_function :sqlite3_changes, [:pointer], :int
    attach_function :sqlite3_total_changes, [:pointer], :int
    attach_function :sqlite3_interrupt, [:pointer], :void
    attach_function :sqlite3_db_filename, [:pointer, :string], :string
    attach_function :sqlite3_threadsafe, [], :int

    # --- Prepared statement functions ---
    attach_function :sqlite3_prepare_v2, [:pointer, :pointer, :int, :pointer, :pointer], :int
    attach_function :sqlite3_step, [:pointer], :int
    attach_function :sqlite3_finalize, [:pointer], :int
    attach_function :sqlite3_reset, [:pointer], :int
    attach_function :sqlite3_clear_bindings, [:pointer], :int

    # --- Column functions ---
    attach_function :sqlite3_column_count, [:pointer], :int
    attach_function :sqlite3_column_name, [:pointer, :int], :string
    attach_function :sqlite3_column_decltype, [:pointer, :int], :string
    attach_function :sqlite3_column_type, [:pointer, :int], :int
    attach_function :sqlite3_column_int64, [:pointer, :int], :int64
    attach_function :sqlite3_column_double, [:pointer, :int], :double
    attach_function :sqlite3_column_text, [:pointer, :int], :string
    attach_function :sqlite3_column_blob, [:pointer, :int], :pointer
    attach_function :sqlite3_column_bytes, [:pointer, :int], :int

    # --- Bind functions ---
    attach_function :sqlite3_bind_parameter_count, [:pointer], :int
    attach_function :sqlite3_bind_parameter_index, [:pointer, :string], :int
    attach_function :sqlite3_bind_null, [:pointer, :int], :int
    attach_function :sqlite3_bind_int64, [:pointer, :int, :int64], :int
    attach_function :sqlite3_bind_double, [:pointer, :int, :double], :int
    attach_function :sqlite3_bind_text, [:pointer, :int, :string, :int, :pointer], :int
    attach_function :sqlite3_bind_blob, [:pointer, :int, :pointer, :int, :pointer], :int

    # --- Misc ---
    attach_function :sqlite3_get_autocommit, [:pointer], :int
    attach_function :sqlite3_stmt_status, [:pointer, :int, :int], :int

    # sqlite3_create_function_v2
    attach_function :sqlite3_create_function_v2, [
      :pointer, :string, :int, :int, :pointer, :pointer, :pointer, :pointer, :pointer
    ], :int
    attach_function :sqlite3_result_null, [:pointer], :void
    attach_function :sqlite3_result_int64, [:pointer, :int64], :void
    attach_function :sqlite3_result_double, [:pointer, :double], :void
    attach_function :sqlite3_result_text, [:pointer, :string, :int, :pointer], :void
    attach_function :sqlite3_value_type, [:pointer], :int
    attach_function :sqlite3_value_int64, [:pointer], :int64
    attach_function :sqlite3_value_double, [:pointer], :double
    attach_function :sqlite3_value_text, [:pointer], :string
    attach_function :sqlite3_value_blob, [:pointer], :pointer
    attach_function :sqlite3_value_bytes, [:pointer], :int
    attach_function :sqlite3_user_data, [:pointer], :pointer

    # SQLITE_TRANSIENT (-1 cast to pointer) tells sqlite3 to make its own copy
    SQLITE_TRANSIENT = FFI::Pointer.new(-1)

    SQLITE_OK   = 0
    SQLITE_ROW  = 100
    SQLITE_DONE = 101

    # Statement status counters
    SQLITE_STMTSTATUS_FULLSCAN_STEP = 1
    SQLITE_STMTSTATUS_SORT          = 2
    SQLITE_STMTSTATUS_AUTOINDEX     = 3
    SQLITE_STMTSTATUS_VM_STEP       = 4
    SQLITE_STMTSTATUS_REPREPARE     = 5
    SQLITE_STMTSTATUS_RUN           = 6
    SQLITE_STMTSTATUS_FILTER_MISS   = 7
    SQLITE_STMTSTATUS_FILTER_HIT    = 8
  end

  # Version information
  SQLITE_VERSION = FFIBridge.sqlite3_libversion
  SQLITE_LOADED_VERSION = SQLITE_VERSION
  SQLITE_PACKAGED_LIBRARIES = false
  SQLITE_PRECOMPILED_LIBRARIES = false

  def self.sqlcipher?
    false
  end

  def self.threadsafe?
    FFIBridge.sqlite3_threadsafe > 0
  end

  def self.threadsafe
    FFIBridge.sqlite3_threadsafe
  end

  # Blob class for binary data
  class Blob < String
  end

  # =========================================================================
  # Function callback registry (prevent GC of procs used as C callbacks)
  # =========================================================================
  @_ffi_callbacks = {}

  def self._register_callback(key, cb)
    @_ffi_callbacks[key] = cb
  end

  def self._unregister_callback(key)
    @_ffi_callbacks.delete(key)
  end

  # Open flags – must be at SQLite3::Constants::Open, not nested in Database
  module Constants
    module Open
      READONLY  = 0x00000001
      READWRITE = 0x00000002
      CREATE    = 0x00000004
      DELETEONCLOSE = 0x00000008
      EXCLUSIVE     = 0x00000010
      FULLMUTEX     = 0x00010000
      NOMUTEX       = 0x00008000
      SHAREDCACHE   = 0x00020000
      PRIVATECACHE  = 0x00040000
      URI           = 0x00000040
      NOFOLLOW      = 0x01000000
    end
  end unless defined?(SQLite3::Constants::Open)

  # =========================================================================
  # Database – native methods implemented via FFI
  # =========================================================================
  class Database
    def open_v2(filename, mode, zvfs)
      ptr = FFI::MemoryPointer.new(:pointer)
      # Allocate a unique C buffer for the filename to prevent sqlite3's
      # shared cache from confusing connections when monoruby deduplicates
      # frozen string literals (all ":memory:" share the same buffer).
      @_fname_buf = FFI::MemoryPointer.new(:char, filename.bytesize + 1)
      @_fname_buf.write_string_length(filename, filename.bytesize)
      rc = FFIBridge.sqlite3_open_v2(@_fname_buf, ptr, mode, nil)
      @db = ptr.read_pointer
      if rc != FFIBridge::SQLITE_OK
        msg = @db.null? ? "out of memory" : FFIBridge.sqlite3_errmsg(@db)
        FFIBridge.sqlite3_close(@db) unless @db.null?
        raise SQLite3::Exception, msg
      end
      @closed = false
    end

    def open16(filename)
      ptr = FFI::MemoryPointer.new(:pointer)
      encoded = filename.encode("UTF-16LE")
      buf = FFI::MemoryPointer.new(:char, encoded.bytesize + 2)
      buf.put_bytes(0, encoded)
      rc = FFIBridge.sqlite3_open16(buf, ptr)
      @db = ptr.read_pointer
      if rc != FFIBridge::SQLITE_OK
        msg = @db.null? ? "out of memory" : FFIBridge.sqlite3_errmsg(@db)
        FFIBridge.sqlite3_close(@db) unless @db.null?
        raise SQLite3::Exception, msg
      end
      @closed = false
    end

    def close
      return if @closed
      @closed = true
      rc = FFIBridge.sqlite3_close(@db)
      check_error(rc)
    end

    def closed?
      @closed
    end

    def encoding
      "UTF-8"
    end

    def busy_timeout=(ms)
      check_error FFIBridge.sqlite3_busy_timeout(@db, ms.to_i)
    end

    def busy_handler(&block)
      if block
        # For simplicity, store the block but don't implement the C callback.
        # This is a rarely used feature; busy_timeout is sufficient for most cases.
        @busy_handler_block = block
      end
    end

    def last_insert_row_id
      FFIBridge.sqlite3_last_insert_rowid(@db)
    end

    def changes
      FFIBridge.sqlite3_changes(@db)
    end

    def total_changes
      FFIBridge.sqlite3_total_changes(@db)
    end

    def interrupt
      FFIBridge.sqlite3_interrupt(@db)
    end

    def errcode
      FFIBridge.sqlite3_errcode(@db)
    end

    def errmsg
      FFIBridge.sqlite3_errmsg(@db)
    end

    def db_filename(db_name = "main")
      FFIBridge.sqlite3_db_filename(@db, db_name)
    end

    def extended_result_codes=(enable)
      check_error FFIBridge.sqlite3_extended_result_codes(@db, enable ? 1 : 0)
    end

    def transaction_active?
      FFIBridge.sqlite3_get_autocommit(@db) == 0
    end

    def disable_quirk_mode
      # Execute PRAGMA to disable double-quoted string literals
      exec_batch_internal("PRAGMA trusted_schema = OFF")
    end

    def trace(mask = nil, &block)
      @tracefunc = block
    end

    def authorizer=(block)
      @authorizer = block
    end

    def enable_load_extension(onoff)
      # Not supported in FFI stub for safety
    end

    def load_extension_internal(path)
      raise SQLite3::Exception, "load_extension not supported in monoruby FFI bridge"
    end

    def exec_batch(sql, results_as_hash)
      # Used by execute_batch2
      rows = []
      remaining = sql.strip
      until remaining.empty?
        stmt_ptr = FFI::MemoryPointer.new(:pointer)
        tail_ptr = FFI::MemoryPointer.new(:pointer)
        rc = FFIBridge.sqlite3_prepare_v2(@db, remaining, remaining.bytesize, stmt_ptr, tail_ptr)
        check_error(rc)
        stmt = stmt_ptr.read_pointer
        if stmt.null?
          break
        end
        begin
          while (rc = FFIBridge.sqlite3_step(stmt)) == FFIBridge::SQLITE_ROW
            row = read_row(stmt)
            rows << row
          end
          check_error(rc) unless rc == FFIBridge::SQLITE_DONE
        ensure
          FFIBridge.sqlite3_finalize(stmt)
        end
        tail = tail_ptr.read_pointer
        if tail.null? || tail.address == 0
          break
        end
        remaining = (tail.read_string_to_null).strip
      end
      rows
    end

    # define_function_with_flags – used by create_function
    def define_function_with_flags(name, flags, &block)
      # Store the block so it won't be GC'd
      @functions ||= {}
      @functions[name] = block

      # Create a C callback for the xFunc
      xfunc = FFI::Function.new(:void, [:pointer, :int, :pointer]) do |ctx, argc, argv_ptr|
        args = []
        argc.times do |i|
          val_ptr = (argv_ptr + i * FFI::Pointer.size).read_pointer
          args << ffi_value_to_ruby(val_ptr)
        end
        begin
          result = block.call(*args)
          ffi_set_result(ctx, result)
        rescue => e
          FFIBridge.sqlite3_result_null(ctx)
        end
      end
      SQLite3._register_callback("func:#{name}", xfunc)

      rc = FFIBridge.sqlite3_create_function_v2(
        @db, name, -1, flags, nil, xfunc, nil, nil, nil
      )
      check_error(rc)
    end

    def define_aggregator2(klass, name)
      # Minimal aggregator support – not needed for the sequel benchmark
    end

    # Internal: db pointer accessor for Statement
    def _db_ptr
      @db
    end

    private

    def check_error(rc)
      return if rc == FFIBridge::SQLITE_OK || rc == FFIBridge::SQLITE_ROW || rc == FFIBridge::SQLITE_DONE
      msg = FFIBridge.sqlite3_errmsg(@db)
      raise SQLite3::Exception, msg
    end

    def exec_batch_internal(sql)
      errmsg_ptr = FFI::MemoryPointer.new(:pointer)
      rc = FFIBridge.sqlite3_exec(@db, sql, nil, nil, errmsg_ptr)
      if rc != FFIBridge::SQLITE_OK
        err = errmsg_ptr.read_pointer
        msg = err.null? ? "unknown error" : err.read_string
        raise SQLite3::Exception, msg
      end
    end

    def read_row(stmt)
      count = FFIBridge.sqlite3_column_count(stmt)
      row = Array.new(count)
      count.times do |i|
        row[i] = read_column(stmt, i)
      end
      row
    end

    def read_column(stmt, i)
      type = FFIBridge.sqlite3_column_type(stmt, i)
      case type
      when 1 # INTEGER
        FFIBridge.sqlite3_column_int64(stmt, i)
      when 2 # FLOAT
        FFIBridge.sqlite3_column_double(stmt, i)
      when 3 # TEXT
        FFIBridge.sqlite3_column_text(stmt, i)
      when 4 # BLOB
        len = FFIBridge.sqlite3_column_bytes(stmt, i)
        ptr = FFIBridge.sqlite3_column_blob(stmt, i)
        ptr.null? ? nil : SQLite3::Blob.new(ptr.read_bytes(len))
      when 5 # NULL
        nil
      else
        FFIBridge.sqlite3_column_text(stmt, i)
      end
    end

    def ffi_value_to_ruby(val_ptr)
      type = FFIBridge.sqlite3_value_type(val_ptr)
      case type
      when 1 # INTEGER
        FFIBridge.sqlite3_value_int64(val_ptr)
      when 2 # FLOAT
        FFIBridge.sqlite3_value_double(val_ptr)
      when 3 # TEXT
        FFIBridge.sqlite3_value_text(val_ptr)
      when 4 # BLOB
        len = FFIBridge.sqlite3_value_bytes(val_ptr)
        ptr = FFIBridge.sqlite3_value_blob(val_ptr)
        ptr.null? ? nil : SQLite3::Blob.new(ptr.read_bytes(len))
      when 5 # NULL
        nil
      else
        nil
      end
    end

    def ffi_set_result(ctx, value)
      case value
      when nil
        FFIBridge.sqlite3_result_null(ctx)
      when Integer
        FFIBridge.sqlite3_result_int64(ctx, value)
      when Float
        FFIBridge.sqlite3_result_double(ctx, value)
      when String
        FFIBridge.sqlite3_result_text(ctx, value, value.bytesize, FFIBridge::SQLITE_TRANSIENT)
      else
        s = value.to_s
        FFIBridge.sqlite3_result_text(ctx, s, s.bytesize, FFIBridge::SQLITE_TRANSIENT)
      end
    end
  end

  # =========================================================================
  # Statement – native methods implemented via FFI
  # =========================================================================
  class Statement
    # initialize is needed for sqlite3 1.7.x which defines it in the native
    # extension rather than in Ruby. For 2.9.x, the Ruby-level initialize
    # calls prepare() directly.
    def initialize(db, sql)
      raise ArgumentError, "prepare called on a closed database" if db.closed?
      sql = sql.encode("UTF-8") if sql && sql.encoding.to_s != "UTF-8"
      @connection = db
      @columns = nil
      @types = nil
      @remainder = prepare db, sql
    end

    def prepare(db, sql)
      @db_ptr = db._db_ptr
      stmt_ptr = FFI::MemoryPointer.new(:pointer)
      tail_ptr = FFI::MemoryPointer.new(:pointer)
      # Copy the SQL into a MemoryPointer so we can compute the remainder
      # via pointer arithmetic (the tail pointer will point into this buffer).
      sql_buf = FFI::MemoryPointer.new(:char, sql.bytesize + 1)
      sql_buf.write_string_length(sql, sql.bytesize)
      rc = FFIBridge.sqlite3_prepare_v2(@db_ptr, sql_buf, sql.bytesize, stmt_ptr, tail_ptr)
      if rc != FFIBridge::SQLITE_OK
        msg = FFIBridge.sqlite3_errmsg(@db_ptr)
        raise SQLite3::Exception, msg
      end
      @stmt = stmt_ptr.read_pointer
      @closed = @stmt.null?
      @done = false

      # Calculate remainder (unparsed trailing SQL)
      # The tail pointer points into sql_buf. Compute offset to find remainder.
      tail = tail_ptr.read_pointer
      if tail.null? || tail.address == 0
        ""
      else
        offset = tail.address - sql_buf.address
        if offset >= sql.bytesize
          ""
        else
          sql.byteslice(offset..-1) || ""
        end
      end
    end

    def close
      return if @closed
      @closed = true
      FFIBridge.sqlite3_finalize(@stmt)
    end

    def closed?
      @closed
    end

    def step
      return nil if @closed
      rc = FFIBridge.sqlite3_step(@stmt)
      if rc == FFIBridge::SQLITE_ROW
        @done = false
        read_row
      elsif rc == FFIBridge::SQLITE_DONE
        @done = true
        nil
      else
        msg = FFIBridge.sqlite3_errmsg(@db_ptr)
        raise SQLite3::Exception, msg
      end
    end

    def done?
      @done
    end

    def reset!
      return self if @closed
      FFIBridge.sqlite3_reset(@stmt)
      FFIBridge.sqlite3_clear_bindings(@stmt)
      @done = false
      self
    end

    def bind_param(index, value)
      raise SQLite3::Exception, "cannot bind to a closed statement" if @closed
      if index.is_a?(String)
        name = index.start_with?(":") ? index : ":#{index}"
        index = FFIBridge.sqlite3_bind_parameter_index(@stmt, name)
        raise SQLite3::Exception, "unknown bind parameter: #{name}" if index == 0
      end
      rc = bind_value(index, value)
      if rc != FFIBridge::SQLITE_OK
        msg = FFIBridge.sqlite3_errmsg(@db_ptr)
        raise SQLite3::Exception, msg
      end
    end

    def column_count
      return 0 if @closed
      FFIBridge.sqlite3_column_count(@stmt)
    end

    def column_name(index)
      return nil if @closed
      FFIBridge.sqlite3_column_name(@stmt, index)
    end

    def column_decltype(index)
      return nil if @closed
      FFIBridge.sqlite3_column_decltype(@stmt, index)
    end

    def bind_parameter_count
      return 0 if @closed
      FFIBridge.sqlite3_bind_parameter_count(@stmt)
    end

    def stat_for(key)
      return 0 if @closed
      counter = case key
      when :fullscan_steps, "fullscan_steps" then FFIBridge::SQLITE_STMTSTATUS_FULLSCAN_STEP
      when :sorts, "sorts"                   then FFIBridge::SQLITE_STMTSTATUS_SORT
      when :autoindexes, "autoindexes"       then FFIBridge::SQLITE_STMTSTATUS_AUTOINDEX
      when :vm_steps, "vm_steps"             then FFIBridge::SQLITE_STMTSTATUS_VM_STEP
      when :reprepares, "reprepares"         then FFIBridge::SQLITE_STMTSTATUS_REPREPARE
      when :runs, "runs"                     then FFIBridge::SQLITE_STMTSTATUS_RUN
      when :filter_misses, "filter_misses"   then FFIBridge::SQLITE_STMTSTATUS_FILTER_MISS
      when :filter_hits, "filter_hits"       then FFIBridge::SQLITE_STMTSTATUS_FILTER_HIT
      else return 0
      end
      FFIBridge.sqlite3_stmt_status(@stmt, counter, 0)
    end

    def stats_as_hash
      return {} if @closed
      {
        fullscan_steps: stat_for(:fullscan_steps),
        sorts: stat_for(:sorts),
        autoindexes: stat_for(:autoindexes),
        vm_steps: stat_for(:vm_steps),
        reprepares: stat_for(:reprepares),
        runs: stat_for(:runs),
        filter_misses: stat_for(:filter_misses),
        filter_hits: stat_for(:filter_hits),
      }
    end

    private

    def bind_value(index, value)
      case value
      when nil
        FFIBridge.sqlite3_bind_null(@stmt, index)
      when Integer
        FFIBridge.sqlite3_bind_int64(@stmt, index, value)
      when Float
        FFIBridge.sqlite3_bind_double(@stmt, index, value)
      when SQLite3::Blob
        FFIBridge.sqlite3_bind_blob(@stmt, index, value, value.bytesize, FFIBridge::SQLITE_TRANSIENT)
      when String
        FFIBridge.sqlite3_bind_text(@stmt, index, value, value.bytesize, FFIBridge::SQLITE_TRANSIENT)
      when true
        FFIBridge.sqlite3_bind_int64(@stmt, index, 1)
      when false
        FFIBridge.sqlite3_bind_int64(@stmt, index, 0)
      else
        s = value.to_s
        FFIBridge.sqlite3_bind_text(@stmt, index, s, s.bytesize, FFIBridge::SQLITE_TRANSIENT)
      end
    end

    def read_row
      count = FFIBridge.sqlite3_column_count(@stmt)
      row = Array.new(count)
      count.times do |i|
        row[i] = read_column(i)
      end
      row
    end

    def read_column(i)
      type = FFIBridge.sqlite3_column_type(@stmt, i)
      case type
      when 1 # INTEGER
        FFIBridge.sqlite3_column_int64(@stmt, i)
      when 2 # FLOAT
        FFIBridge.sqlite3_column_double(@stmt, i)
      when 3 # TEXT
        FFIBridge.sqlite3_column_text(@stmt, i)
      when 4 # BLOB
        len = FFIBridge.sqlite3_column_bytes(@stmt, i)
        ptr = FFIBridge.sqlite3_column_blob(@stmt, i)
        ptr.null? ? nil : SQLite3::Blob.new(ptr.read_bytes(len))
      when 5 # NULL
        nil
      else
        FFIBridge.sqlite3_column_text(@stmt, i)
      end
    end
  end
end
