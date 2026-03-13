# sqlite3/ffi_bindings.rb - Low-level FFI bindings to libsqlite3 via Fiddle
#
# This module wraps the SQLite3 C API functions using monoruby's Fiddle module.
# All functions are exposed as module methods on SQLite3::FFIBindings.

require 'fiddle'

module SQLite3
  module FFIBindings
    # SQLite3 result codes
    SQLITE_OK         = 0
    SQLITE_ERROR      = 1
    SQLITE_INTERNAL   = 2
    SQLITE_PERM       = 3
    SQLITE_ABORT      = 4
    SQLITE_BUSY       = 5
    SQLITE_LOCKED     = 6
    SQLITE_NOMEM      = 7
    SQLITE_READONLY   = 8
    SQLITE_INTERRUPT  = 9
    SQLITE_IOERR      = 10
    SQLITE_CORRUPT    = 11
    SQLITE_NOTFOUND   = 12
    SQLITE_FULL       = 13
    SQLITE_CANTOPEN   = 14
    SQLITE_PROTOCOL   = 15
    SQLITE_EMPTY      = 16
    SQLITE_SCHEMA     = 17
    SQLITE_TOOBIG     = 18
    SQLITE_CONSTRAINT = 19
    SQLITE_MISMATCH   = 20
    SQLITE_MISUSE     = 21
    SQLITE_NOLFS      = 22
    SQLITE_AUTH       = 23
    SQLITE_FORMAT     = 24
    SQLITE_RANGE      = 25
    SQLITE_NOTADB     = 26
    SQLITE_ROW        = 100
    SQLITE_DONE       = 101

    # SQLite3 column types
    SQLITE_INTEGER = 1
    SQLITE_FLOAT   = 2
    SQLITE_TEXT    = 3
    SQLITE_BLOB    = 4
    SQLITE_NULL    = 5

    # Special destructor value for sqlite3_bind_text
    SQLITE_TRANSIENT = -1

    # Type aliases for readability
    VOIDP      = Fiddle::Types::VOIDP
    INT        = Fiddle::Types::INT
    LONG_LONG  = Fiddle::Types::LONG_LONG
    DOUBLE     = Fiddle::Types::DOUBLE
    VOID       = Fiddle::Types::VOID

    @lib = nil
    @functions = {}

    def self.lib
      return @lib if @lib
      # Try common library names
      lib_names = ["libsqlite3.so", "libsqlite3.so.0"]
      lib_names.each do |name|
        begin
          @lib = Fiddle.dlopen(name)
          return @lib
        rescue Fiddle::DLError
          next
        end
      end
      raise LoadError, "Could not find libsqlite3. Please install sqlite3 development libraries."
    end

    def self.func(name, args, ret)
      @functions[name] ||= Fiddle::Function.new(lib[name], args, ret)
    end

    # -----------------------------------------------------------------------
    # Database connection functions
    # -----------------------------------------------------------------------

    # int sqlite3_open(const char *filename, sqlite3 **ppDb)
    def self.sqlite3_open(filename)
      f = func("sqlite3_open", [VOIDP, VOIDP], INT)
      # Allocate 8 bytes to hold the sqlite3* pointer
      pp_db = Fiddle::Pointer.new(Kernel.___malloc(8, true), 8)
      # Ensure filename is null-terminated
      filename_buf = filename + "\0"
      rc = f.call(filename_buf, pp_db.to_i)
      db_ptr = pp_db.read_pointer.to_i
      Fiddle.free(pp_db)
      [rc, db_ptr]
    end

    # int sqlite3_close(sqlite3 *db)
    def self.sqlite3_close(db)
      f = func("sqlite3_close", [VOIDP], INT)
      f.call(db)
    end

    # const char *sqlite3_errmsg(sqlite3 *db)
    def self.sqlite3_errmsg(db)
      f = func("sqlite3_errmsg", [VOIDP], VOIDP)
      ptr = f.call(db)
      if ptr == 0 || ptr.nil?
        "unknown error"
      else
        Fiddle.___read_string(ptr)
      end
    end

    # -----------------------------------------------------------------------
    # Statement functions
    # -----------------------------------------------------------------------

    # int sqlite3_prepare_v2(sqlite3 *db, const char *zSql, int nByte,
    #                        sqlite3_stmt **ppStmt, const char **pzTail)
    def self.sqlite3_prepare_v2(db, sql)
      f = func("sqlite3_prepare_v2", [VOIDP, VOIDP, INT, VOIDP, VOIDP], INT)
      pp_stmt = Fiddle::Pointer.new(Kernel.___malloc(8, true), 8)
      sql_buf = sql + "\0"
      rc = f.call(db, sql_buf, sql.bytesize, pp_stmt.to_i, 0)
      stmt_ptr = pp_stmt.read_pointer.to_i
      Fiddle.free(pp_stmt)
      [rc, stmt_ptr]
    end

    # int sqlite3_step(sqlite3_stmt *pStmt)
    def self.sqlite3_step(stmt)
      f = func("sqlite3_step", [VOIDP], INT)
      f.call(stmt)
    end

    # int sqlite3_finalize(sqlite3_stmt *pStmt)
    def self.sqlite3_finalize(stmt)
      f = func("sqlite3_finalize", [VOIDP], INT)
      f.call(stmt)
    end

    # int sqlite3_reset(sqlite3_stmt *pStmt)
    def self.sqlite3_reset(stmt)
      f = func("sqlite3_reset", [VOIDP], INT)
      f.call(stmt)
    end

    # int sqlite3_clear_bindings(sqlite3_stmt *pStmt)
    def self.sqlite3_clear_bindings(stmt)
      f = func("sqlite3_clear_bindings", [VOIDP], INT)
      f.call(stmt)
    end

    # -----------------------------------------------------------------------
    # Binding functions (parameter indices start at 1)
    # -----------------------------------------------------------------------

    # int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64)
    def self.sqlite3_bind_int64(stmt, idx, value)
      f = func("sqlite3_bind_int64", [VOIDP, INT, LONG_LONG], INT)
      f.call(stmt, idx, value)
    end

    # int sqlite3_bind_double(sqlite3_stmt*, int, double)
    def self.sqlite3_bind_double(stmt, idx, value)
      f = func("sqlite3_bind_double", [VOIDP, INT, DOUBLE], INT)
      f.call(stmt, idx, value)
    end

    # int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int, void(*)(void*))
    def self.sqlite3_bind_text(stmt, idx, text)
      f = func("sqlite3_bind_text", [VOIDP, INT, VOIDP, INT, VOIDP], INT)
      # We pass SQLITE_TRANSIENT (-1) so SQLite copies the string
      text_buf = text + "\0"
      f.call(stmt, idx, text_buf, text.bytesize, SQLITE_TRANSIENT)
    end

    # int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int, void(*)(void*))
    def self.sqlite3_bind_blob(stmt, idx, data)
      f = func("sqlite3_bind_blob", [VOIDP, INT, VOIDP, INT, VOIDP], INT)
      f.call(stmt, idx, data, data.bytesize, SQLITE_TRANSIENT)
    end

    # int sqlite3_bind_null(sqlite3_stmt*, int)
    def self.sqlite3_bind_null(stmt, idx)
      f = func("sqlite3_bind_null", [VOIDP, INT], INT)
      f.call(stmt, idx)
    end

    # int sqlite3_bind_parameter_count(sqlite3_stmt*)
    def self.sqlite3_bind_parameter_count(stmt)
      f = func("sqlite3_bind_parameter_count", [VOIDP], INT)
      f.call(stmt)
    end

    # -----------------------------------------------------------------------
    # Column result functions
    # -----------------------------------------------------------------------

    # int sqlite3_column_count(sqlite3_stmt *pStmt)
    def self.sqlite3_column_count(stmt)
      f = func("sqlite3_column_count", [VOIDP], INT)
      f.call(stmt)
    end

    # int sqlite3_column_type(sqlite3_stmt*, int iCol)
    def self.sqlite3_column_type(stmt, col)
      f = func("sqlite3_column_type", [VOIDP, INT], INT)
      f.call(stmt, col)
    end

    # sqlite3_int64 sqlite3_column_int64(sqlite3_stmt*, int iCol)
    def self.sqlite3_column_int64(stmt, col)
      f = func("sqlite3_column_int64", [VOIDP, INT], LONG_LONG)
      f.call(stmt, col)
    end

    # double sqlite3_column_double(sqlite3_stmt*, int iCol)
    def self.sqlite3_column_double(stmt, col)
      f = func("sqlite3_column_double", [VOIDP, INT], DOUBLE)
      f.call(stmt, col)
    end

    # const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol)
    def self.sqlite3_column_text(stmt, col)
      f = func("sqlite3_column_text", [VOIDP, INT], VOIDP)
      ptr = f.call(stmt, col)
      if ptr == 0 || ptr.nil?
        nil
      else
        Fiddle.___read_string(ptr)
      end
    end

    # const void *sqlite3_column_blob(sqlite3_stmt*, int iCol)
    # int sqlite3_column_bytes(sqlite3_stmt*, int iCol)
    def self.sqlite3_column_blob(stmt, col)
      f_blob  = func("sqlite3_column_blob",  [VOIDP, INT], VOIDP)
      f_bytes = func("sqlite3_column_bytes", [VOIDP, INT], INT)
      ptr = f_blob.call(stmt, col)
      if ptr == 0 || ptr.nil?
        nil
      else
        len = f_bytes.call(stmt, col)
        if len > 0
          Fiddle.___read_bytes(ptr, len)
        else
          ""
        end
      end
    end

    # const char *sqlite3_column_name(sqlite3_stmt*, int N)
    def self.sqlite3_column_name(stmt, col)
      f = func("sqlite3_column_name", [VOIDP, INT], VOIDP)
      ptr = f.call(stmt, col)
      if ptr == 0 || ptr.nil?
        nil
      else
        Fiddle.___read_string(ptr)
      end
    end

    # -----------------------------------------------------------------------
    # Exec and utility functions
    # -----------------------------------------------------------------------

    # int sqlite3_exec(sqlite3*, const char *sql, callback, void*, char **errmsg)
    # We don't use the callback - just pass NULL
    def self.sqlite3_exec(db, sql)
      f = func("sqlite3_exec", [VOIDP, VOIDP, VOIDP, VOIDP, VOIDP], INT)
      sql_buf = sql + "\0"
      pp_errmsg = Fiddle::Pointer.new(Kernel.___malloc(8, true), 8)
      pp_errmsg.write_pointer(Fiddle::Pointer.new(0))
      rc = f.call(db, sql_buf, 0, 0, pp_errmsg.to_i)
      errmsg = nil
      if rc != SQLITE_OK
        errmsg_ptr = pp_errmsg.read_pointer.to_i
        if errmsg_ptr != 0
          errmsg = Fiddle.___read_string(errmsg_ptr)
          # sqlite3_free the error message
          sqlite3_free(errmsg_ptr)
        end
      end
      Fiddle.free(pp_errmsg)
      [rc, errmsg]
    end

    # void sqlite3_free(void*)
    def self.sqlite3_free(ptr)
      f = func("sqlite3_free", [VOIDP], VOID)
      f.call(ptr)
    end

    # int sqlite3_changes(sqlite3*)
    def self.sqlite3_changes(db)
      f = func("sqlite3_changes", [VOIDP], INT)
      f.call(db)
    end

    # sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*)
    def self.sqlite3_last_insert_rowid(db)
      f = func("sqlite3_last_insert_rowid", [VOIDP], LONG_LONG)
      f.call(db)
    end
  end
end
