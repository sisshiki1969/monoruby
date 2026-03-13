# sqlite3/statement.rb - SQLite3::Statement implementation
#
# Wraps a prepared SQLite3 statement (sqlite3_stmt*).
# API compatible with the CRuby sqlite3 gem's Statement class.

module SQLite3
  class Statement
    include Enumerable

    attr_reader :remainder

    def initialize(db, sql)
      @db = db
      @sql = sql
      @closed = false
      @remainder = nil

      rc, @stmt_ptr = FFIBindings.sqlite3_prepare_v2(db.instance_variable_get(:@db_ptr), sql)
      if rc != FFIBindings::SQLITE_OK
        raise SQLException, "Could not prepare statement: #{db.errmsg}"
      end
      if @stmt_ptr == 0
        raise SQLException, "Could not prepare statement (empty statement)"
      end
    end

    # Bind parameters to the statement.
    # Parameters can be given as an Array (positional) or Hash (named).
    def bind_params(*params)
      if params.length == 1 && params[0].is_a?(Array)
        params = params[0]
      end
      if params.length == 1 && params[0].is_a?(Hash)
        raise NotImplementedError, "Named parameters are not yet supported"
      end

      params.each_with_index do |param, i|
        bind_param(i + 1, param)
      end
    end

    # Bind a single parameter by index (1-based).
    def bind_param(index, value)
      check_closed!
      rc = case value
      when nil
        FFIBindings.sqlite3_bind_null(@stmt_ptr, index)
      when Integer
        FFIBindings.sqlite3_bind_int64(@stmt_ptr, index, value)
      when Float
        FFIBindings.sqlite3_bind_double(@stmt_ptr, index, value)
      when String
        if value.encoding == Encoding::BINARY || value.encoding == Encoding::ASCII_8BIT
          FFIBindings.sqlite3_bind_blob(@stmt_ptr, index, value)
        else
          FFIBindings.sqlite3_bind_text(@stmt_ptr, index, value)
        end
      when true
        FFIBindings.sqlite3_bind_int64(@stmt_ptr, index, 1)
      when false
        FFIBindings.sqlite3_bind_int64(@stmt_ptr, index, 0)
      else
        FFIBindings.sqlite3_bind_text(@stmt_ptr, index, value.to_s)
      end

      if rc != FFIBindings::SQLITE_OK
        raise SQLException, "Could not bind parameter #{index}: #{@db.errmsg}"
      end
    end

    # Execute the statement with optional parameters.
    # Returns a ResultSet for iteration.
    def execute(*params)
      check_closed!
      reset!
      bind_params(*params) unless params.empty?
      ResultSet.new(self)
    end

    # Step through one row. Returns an Array of column values, or nil when done.
    def step
      check_closed!
      rc = FFIBindings.sqlite3_step(@stmt_ptr)
      case rc
      when FFIBindings::SQLITE_ROW
        row = []
        count = column_count
        count.times do |i|
          row << read_column(i)
        end
        row
      when FFIBindings::SQLITE_DONE
        nil
      else
        raise SQLException, "Step failed: #{@db.errmsg}"
      end
    end

    # Return the column names for this statement.
    def columns
      check_closed!
      count = column_count
      names = []
      count.times do |i|
        names << FFIBindings.sqlite3_column_name(@stmt_ptr, i)
      end
      names
    end

    # Return the number of columns in the result set.
    def column_count
      check_closed!
      FFIBindings.sqlite3_column_count(@stmt_ptr)
    end

    # Reset the statement for re-execution.
    def reset!
      check_closed!
      FFIBindings.sqlite3_reset(@stmt_ptr)
      FFIBindings.sqlite3_clear_bindings(@stmt_ptr)
    end

    # Close and finalize the statement.
    def close
      return if @closed
      @closed = true
      FFIBindings.sqlite3_finalize(@stmt_ptr) if @stmt_ptr != 0
      @stmt_ptr = 0
    end

    def closed?
      @closed
    end

    # Enumerate all result rows.
    def each
      check_closed!
      while (row = step)
        yield row
      end
    end

    private

    def check_closed!
      raise SQLException, "Statement is closed" if @closed
    end

    # Read a column value with automatic type detection.
    def read_column(col)
      col_type = FFIBindings.sqlite3_column_type(@stmt_ptr, col)
      case col_type
      when FFIBindings::SQLITE_INTEGER
        FFIBindings.sqlite3_column_int64(@stmt_ptr, col)
      when FFIBindings::SQLITE_FLOAT
        FFIBindings.sqlite3_column_double(@stmt_ptr, col)
      when FFIBindings::SQLITE_TEXT
        FFIBindings.sqlite3_column_text(@stmt_ptr, col)
      when FFIBindings::SQLITE_BLOB
        FFIBindings.sqlite3_column_blob(@stmt_ptr, col)
      when FFIBindings::SQLITE_NULL
        nil
      else
        FFIBindings.sqlite3_column_text(@stmt_ptr, col)
      end
    end
  end
end
