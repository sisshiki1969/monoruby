# sqlite3/database.rb - SQLite3::Database implementation
#
# Main interface for interacting with a SQLite3 database.
# API compatible with the CRuby sqlite3 gem's Database class.

module SQLite3
  class Database
    attr_reader :results_as_hash

    def initialize(file, options = {})
      @results_as_hash = options.fetch(:results_as_hash, false)
      rc, @db_ptr = FFIBindings.sqlite3_open(file)
      if rc != FFIBindings::SQLITE_OK
        msg = if @db_ptr != 0
          FFIBindings.sqlite3_errmsg(@db_ptr)
        else
          "unable to open database"
        end
        raise SQLException, "Could not open database: #{msg}"
      end
      @closed = false
    end

    # Execute SQL with optional bind parameters.
    # Returns an Array of row Arrays (or row Hashes if results_as_hash is true).
    def execute(sql, *bind_params)
      # Flatten if a single array argument is given
      if bind_params.length == 1 && bind_params[0].is_a?(Array)
        bind_params = bind_params[0]
      end

      check_closed!
      stmt = prepare(sql)
      begin
        stmt.bind_params(*bind_params) unless bind_params.empty?
        rows = []
        if @results_as_hash
          cols = stmt.columns
          while (row = stmt.step)
            h = {}
            cols.each_with_index do |name, i|
              h[name] = row[i]
            end
            rows << h
          end
        else
          while (row = stmt.step)
            rows << row
          end
        end
        if block_given?
          rows.each { |row| yield row }
        end
        rows
      ensure
        stmt.close
      end
    end

    # Execute multiple SQL statements separated by semicolons.
    # This uses sqlite3_exec for simple non-parameterized execution.
    def execute_batch(sql, *bind_params)
      check_closed!
      if bind_params.empty?
        rc, errmsg = FFIBindings.sqlite3_exec(@db_ptr, sql)
        if rc != FFIBindings::SQLITE_OK
          raise SQLException, errmsg || errmsg(@db_ptr)
        end
      else
        # For parameterized batch, split and execute individually
        sql.split(";").each do |single_sql|
          single_sql = single_sql.strip
          next if single_sql.empty?
          execute(single_sql, *bind_params)
        end
      end
      nil
    end

    # Prepare a statement for execution.
    def prepare(sql)
      check_closed!
      Statement.new(self, sql)
    end

    # Close the database connection.
    def close
      return if @closed
      @closed = true
      FFIBindings.sqlite3_close(@db_ptr) if @db_ptr != 0
      @db_ptr = 0
    end

    def closed?
      @closed
    end

    # Return the number of rows changed by the most recent INSERT/UPDATE/DELETE.
    def changes
      check_closed!
      FFIBindings.sqlite3_changes(@db_ptr)
    end

    # Return the rowid of the most recently inserted row.
    def last_insert_row_id
      check_closed!
      FFIBindings.sqlite3_last_insert_rowid(@db_ptr)
    end

    # Return the error message for the most recent failed operation.
    def errmsg
      FFIBindings.sqlite3_errmsg(@db_ptr)
    end

    # Execute SQL and return the first row as an Array.
    def get_first_row(sql, *bind_params)
      execute(sql, *bind_params).first
    end

    # Execute SQL and return the first value of the first row.
    def get_first_value(sql, *bind_params)
      row = get_first_row(sql, *bind_params)
      row ? row[0] : nil
    end

    # Transaction support
    def transaction(mode = :deferred)
      execute "BEGIN #{mode.to_s.upcase} TRANSACTION"
      begin
        result = yield self
        execute "COMMIT TRANSACTION"
        result
      rescue Exception => e
        execute "ROLLBACK TRANSACTION"
        raise
      end
    end

    # Enable/disable results as hash
    def results_as_hash=(value)
      @results_as_hash = value
    end

    # Table info helper - returns column information for a table
    def table_info(table_name)
      execute("PRAGMA table_info(#{table_name})")
    end

    private

    def check_closed!
      raise SQLException, "Database is closed" if @closed
    end
  end
end
