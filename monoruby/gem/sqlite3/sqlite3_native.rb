# sqlite3_native.rb – monoruby replacement for sqlite3_native.so
#
# When monoruby encounters require "sqlite3/X.Y/sqlite3_native" (a .so),
# it redirects to ~/.monoruby/sqlite3_native.rb (this file via build.rs copy).
# This implements the native C extension methods by calling libsqlite3
# through monoruby's shared native-call primitives (`Fiddle.___dlopen` /
# `___prepare` / `___invoke` / `___read_string` / …, registered in
# src/builtins/fiddle.rs).
#
# Deliberately *not* built on the `ffi` gem. The gem's C extension is
# replaced by monoruby (gem/ffi_c.rb), but `FFI::Library` — i.e.
# `ffi_lib` / `attach_function`, which this bridge would need — lives in
# the gem's pure-Ruby half, so an FFI-based bridge only works on a host
# that has the ffi gem installed. Fiddle ships with monoruby itself, so
# this bridge is self-contained.
#
# Going straight to the primitives is also the faster route. Reaching C via
# FFI::Function#call costs, on *every* invocation, a
# `param_types.zip(args).map { convert_arg }` plus a
# `param_types.map(&:type_code)` — three throw-away arrays and a Type object
# per argument — and wraps every returned pointer in an FFI::Pointer. The
# stubs generated below prepare the libffi CIF once at attach time and then
# pass arguments positionally, so a call allocates nothing, and pointers stay
# plain Integer addresses.

require "fiddle"

module SQLite3
  # =========================================================================
  # FFIBridge – libsqlite3 entry points
  #
  # Pointers are plain Integer addresses throughout (0 = NULL); `:string`
  # returns are decoded to a Ruby String, or nil for NULL.
  # =========================================================================
  module FFIBridge
    # Fiddle type codes (src/builtins/fiddle.rs). `:string` is a char* —
    # as an argument it is VOIDP (a Ruby String hands over its own NUL
    # terminated buffer), as a return value it is decoded below.
    TYPE_CODES = {
      void:    Fiddle::Types::VOID,
      pointer: Fiddle::Types::VOIDP,
      string:  Fiddle::Types::VOIDP,
      int:     Fiddle::Types::INT,
      int64:   Fiddle::Types::LONG_LONG,
      double:  Fiddle::Types::DOUBLE,
    }.freeze

    # libsqlite3.so.0 on Linux, libsqlite3.dylib on macOS. Try the
    # versioned name first: a bare `libsqlite3.so` only exists when the
    # -dev package is installed.
    LIB = begin
      handle = nil
      ["libsqlite3.so.0", "libsqlite3.so", "libsqlite3.dylib"].each do |name|
        begin
          handle = Fiddle::Handle.new(name)
          break
        rescue Fiddle::DLError
          next
        end
      end
      handle or raise LoadError, "could not load libsqlite3"
    end

    # Define `FFIBridge.<name>` as a fixed-arity stub that calls the C
    # function directly.
    #
    # The signature is fixed at attach time, so the libffi CIF is built once
    # by `___prepare` and reused; `___call` would instead run `ffi_prep_cif`
    # plus a malloc on every call, which costs more than most of these C
    # bodies do. The resulting descriptor id is baked into the generated
    # source as an integer literal, and arguments are passed positionally, so
    # a call allocates nothing at all.
    def self.attach_function(name, argtypes, rettype)
      addr = LIB[name.to_s]
      sig  = argtypes.map { |t| TYPE_CODES.fetch(t) }
      id   = Fiddle.___prepare(addr, sig, TYPE_CODES.fetch(rettype))

      params = (0...argtypes.size).map { |i| "a#{i}" }.join(", ")
      call   = "Fiddle.___invoke(#{id}#{params.empty? ? "" : ", #{params}"})"
      # `___read_string` maps NULL to nil, matching FFI's `:string`.
      body   = rettype == :string ? "Fiddle.___read_string(#{call})" : call

      module_eval("def self.#{name}(#{params})\n  #{body}\nend", __FILE__, __LINE__)
    end

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

    # --- Statement functions ---
    attach_function :sqlite3_prepare_v2, [:pointer, :pointer, :int, :pointer, :pointer], :int
    attach_function :sqlite3_step, [:pointer], :int
    attach_function :sqlite3_finalize, [:pointer], :int
    attach_function :sqlite3_reset, [:pointer], :int
    attach_function :sqlite3_clear_bindings, [:pointer], :int

    # --- Column accessors ---
    attach_function :sqlite3_column_count, [:pointer], :int
    attach_function :sqlite3_column_name, [:pointer, :int], :string
    attach_function :sqlite3_column_decltype, [:pointer, :int], :string
    attach_function :sqlite3_column_type, [:pointer, :int], :int
    attach_function :sqlite3_column_int64, [:pointer, :int], :int64
    attach_function :sqlite3_column_double, [:pointer, :int], :double
    attach_function :sqlite3_column_text, [:pointer, :int], :string
    attach_function :sqlite3_column_blob, [:pointer, :int], :pointer
    attach_function :sqlite3_column_bytes, [:pointer, :int], :int

    # --- Parameter binding ---
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

    # --- SQL function results / values ---
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
    SQLITE_TRANSIENT = -1

    SQLITE_OK   = 0
    SQLITE_ROW  = 100
    SQLITE_DONE = 101

    # Primary result code (low 8 bits) -> the gem's exception subclass, as the
    # C extension's `sqlite3_ruby_exception` maps them. Callers rescue these by
    # name — ActiveRecord turns SQLite3::ConstraintException into
    # RecordNotUnique — so raising the base SQLite3::Exception for everything
    # silently breaks that dispatch.
    ERROR_CLASSES = {
      1  => "SQLException",         # SQLITE_ERROR
      2  => "InternalException",
      3  => "PermissionException",
      4  => "AbortException",
      5  => "BusyException",
      6  => "LockedException",
      7  => "MemoryException",
      8  => "ReadOnlyException",
      9  => "InterruptException",
      10 => "IOException",
      11 => "CorruptException",
      12 => "NotFoundException",
      13 => "FullException",
      14 => "CantOpenException",
      15 => "ProtocolException",
      16 => "EmptyException",
      17 => "SchemaChangedException",
      18 => "TooBigException",
      19 => "ConstraintException",
      20 => "MismatchException",
      21 => "MisuseException",
      23 => "AuthorizationException",
      25 => "RangeException",
      26 => "NotADatabaseException",
    }.freeze

    # sqlite3 returns extended codes (e.g. 1555 = SQLITE_CONSTRAINT_PRIMARYKEY)
    # once extended result codes are on; the primary code is the low 8 bits.
    # The subclasses are defined by the gem's errors.rb, which is loaded before
    # this file, but fall back to the base class if one is missing.
    def self.exception_class(code)
      name = ERROR_CLASSES[code & 0xff]
      (name && SQLite3.const_defined?(name)) ? SQLite3.const_get(name) : SQLite3::Exception
    end

    # Statement status counters
    SQLITE_STMTSTATUS_FULLSCAN_STEP = 1
    SQLITE_STMTSTATUS_SORT          = 2
    SQLITE_STMTSTATUS_AUTOINDEX     = 3
    SQLITE_STMTSTATUS_VM_STEP       = 4
    SQLITE_STMTSTATUS_REPREPARE     = 5
    SQLITE_STMTSTATUS_RUN           = 6
    SQLITE_STMTSTATUS_FILTER_MISS   = 7
    SQLITE_STMTSTATUS_FILTER_HIT    = 8

    # --- Native memory helpers (Fiddle has no FFI::MemoryPointer) --------

    # Allocate `size` zeroed bytes and return the address.
    def self.malloc(size)
      addr = Fiddle.___malloc(size, true)
      raise NoMemoryError, "sqlite3 bridge: malloc(#{size}) failed" if addr == 0
      addr
    end

    # Allocate a NUL-terminated copy of `str` in native memory.
    #
    # Passing a Ruby String straight to a `:pointer` parameter would hand
    # sqlite3 the String's own buffer; a private copy is needed wherever
    # the address itself has to stay meaningful after the call (the
    # `prepare` tail pointer) or where two connections must not share one
    # buffer (monoruby deduplicates frozen string literals, so every
    # `":memory:"` in a program is the same object).
    def self.strdup(str)
      addr = malloc(str.bytesize + 1)
      Fiddle.___write_bytes(addr, str)
      addr
    end

    # 8-byte out-parameter slot; yields its address and reads back the
    # pointer the callee stored there.
    def self.out_ptr
      addr = malloc(8)
      begin
        yield addr
        Fiddle.___read(addr, Fiddle::Types::VOIDP)
      ensure
        Fiddle.___free(addr)
      end
    end
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
      # Give sqlite3 a private copy of the filename: monoruby deduplicates
      # frozen string literals, so every ":memory:" in a program is one
      # object and two connections would otherwise share a buffer.
      @_fname_buf = FFIBridge.strdup(filename)
      rc = nil
      @db = FFIBridge.out_ptr do |ptr|
        rc = FFIBridge.sqlite3_open_v2(@_fname_buf, ptr, mode, nil)
      end
      if rc != FFIBridge::SQLITE_OK
        msg = @db == 0 ? "out of memory" : FFIBridge.sqlite3_errmsg(@db)
        FFIBridge.sqlite3_close(@db) unless @db == 0
        raise FFIBridge.exception_class(rc), msg
      end
      @closed = false
    end

    def open16(filename)
      encoded = filename.encode("UTF-16LE")
      # UTF-16 needs a two-byte terminator, so strdup's single NUL is not
      # enough — allocate (and zero) the extra byte explicitly.
      buf = FFIBridge.malloc(encoded.bytesize + 2)
      Fiddle.___write_bytes(buf, encoded)
      rc = nil
      @db = FFIBridge.out_ptr do |ptr|
        rc = FFIBridge.sqlite3_open16(buf, ptr)
      end
      Fiddle.___free(buf)
      if rc != FFIBridge::SQLITE_OK
        msg = @db == 0 ? "out of memory" : FFIBridge.sqlite3_errmsg(@db)
        FFIBridge.sqlite3_close(@db) unless @db == 0
        raise FFIBridge.exception_class(rc), msg
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
        # `remaining` is passed as a :pointer, so sqlite3 sees the Ruby
        # String's own buffer and the tail pointer points back into it —
        # valid as long as `remaining` is still referenced here.
        tail = 0
        stmt = FFIBridge.out_ptr do |stmt_ptr|
          tail = FFIBridge.out_ptr do |tail_ptr|
            rc = FFIBridge.sqlite3_prepare_v2(@db, remaining, remaining.bytesize, stmt_ptr, tail_ptr)
            check_error(rc)
          end
        end
        if stmt == 0
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
        if tail == 0
          break
        end
        remaining = Fiddle.___read_string(tail).strip
      end
      rows
    end

    # define_function_with_flags – used by create_function
    #
    # Registering a user-defined SQL function means handing sqlite3 a C
    # function pointer that calls back into Ruby. That needs a native
    # closure (an executable trampoline), which monoruby does not generate
    # yet — neither Fiddle::Closure nor FFI::Function.new(&block) produces
    # a real callable address. Raise rather than register a NULL xFunc and
    # let sqlite3 report SQLITE_MISUSE.
    def define_function_with_flags(name, flags, &block)
      raise SQLite3::Exception,
        "create_function is not supported by monoruby's sqlite3 bridge (native callbacks are not implemented)"
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
      raise FFIBridge.exception_class(rc), msg
    end

    def exec_batch_internal(sql)
      rc = nil
      err = FFIBridge.out_ptr do |errmsg_ptr|
        rc = FFIBridge.sqlite3_exec(@db, sql, nil, nil, errmsg_ptr)
      end
      if rc != FFIBridge::SQLITE_OK
        raise FFIBridge.exception_class(rc), Fiddle.___read_string(err) || "unknown error"
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
        ptr == 0 ? nil : SQLite3::Blob.new(Fiddle.___read_bytes(ptr, len))
      when 5 # NULL
        nil
      else
        FFIBridge.sqlite3_column_text(stmt, i)
      end
    end

    # Value marshalling for user-defined SQL functions. Unreachable until
    # native closures exist (see define_function_with_flags), but kept as
    # the other half of that feature.
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
        ptr == 0 ? nil : SQLite3::Blob.new(Fiddle.___read_bytes(ptr, len))
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
      # Copy the SQL into native memory so the remainder can be computed by
      # pointer arithmetic (sqlite3 points the tail into this buffer), and
      # so the address stays stable regardless of what the Ruby String does.
      sql_buf = FFIBridge.strdup(sql)
      rc = nil
      tail = 0
      begin
        @stmt = FFIBridge.out_ptr do |stmt_ptr|
          tail = FFIBridge.out_ptr do |tail_ptr|
            rc = FFIBridge.sqlite3_prepare_v2(@db_ptr, sql_buf, sql.bytesize, stmt_ptr, tail_ptr)
          end
        end
        if rc != FFIBridge::SQLITE_OK
          msg = FFIBridge.sqlite3_errmsg(@db_ptr)
          raise FFIBridge.exception_class(rc), msg
        end
        @closed = @stmt == 0
        @done = false

        # Calculate remainder (unparsed trailing SQL) from the tail offset.
        if tail == 0
          ""
        else
          offset = tail - sql_buf
          if offset >= sql.bytesize
            ""
          else
            sql.byteslice(offset..-1) || ""
          end
        end
      ensure
        # sqlite3_prepare_v2 keeps its own copy of the SQL text.
        Fiddle.___free(sql_buf)
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
        raise FFIBridge.exception_class(rc), msg
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
      # The C extension returns self; ActiveRecord's statement pool chains
      # off the return value (`stmt.reset!` then reuse).
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
        raise FFIBridge.exception_class(rc), msg
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
        ptr == 0 ? nil : SQLite3::Blob.new(Fiddle.___read_bytes(ptr, len))
      when 5 # NULL
        nil
      else
        FFIBridge.sqlite3_column_text(@stmt, i)
      end
    end
  end
end
