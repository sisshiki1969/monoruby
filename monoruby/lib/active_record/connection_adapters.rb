# frozen_string_literal: true
#
# ActiveRecord::ConnectionAdapters - Database connection management.

module ActiveRecord
  module ConnectionAdapters
    class Column
      attr_reader :name, :sql_type, :default, :null, :primary

      def initialize(name, default, sql_type, null = true, primary = false)
        @name = name
        @default = default
        @sql_type = sql_type
        @null = null
        @primary = primary
        @type_object = Type.lookup(sql_type || "string")
      end

      def type
        @type_object.type
      end

      def type_cast(value)
        @type_object.cast(value)
      end

      def serialize(value)
        @type_object.serialize(value)
      end
    end

    class AbstractAdapter
      attr_reader :connection

      def initialize(connection)
        @connection = connection
      end

      def execute(sql, binds = [])
        raise NotImplementedError
      end

      def exec_query(sql, binds = [])
        raise NotImplementedError
      end

      def columns(table_name)
        raise NotImplementedError
      end

      def tables
        raise NotImplementedError
      end

      def table_exists?(table_name)
        tables.include?(table_name.to_s)
      end
    end

    class SQLite3Adapter < AbstractAdapter
      def initialize(config)
        database = config[:database] || config["database"] || ":memory:"
        @connection = ::SQLite3::Database.new(database)
        @connection.results_as_hash = false
      end

      def execute(sql, binds = [])
        @connection.execute(sql, binds)
      rescue ::SQLite3::SQLException => e
        raise StatementInvalid, e.message
      end

      def exec_query(sql, binds = [])
        stmt = @connection.prepare(sql)
        begin
          stmt.bind_params(*binds) unless binds.empty?
          columns = stmt.columns
          rows = []
          while (row = stmt.step)
            rows << row
          end
          QueryResult.new(columns, rows)
        ensure
          stmt.close
        end
      rescue ::SQLite3::SQLException => e
        raise StatementInvalid, e.message
      end

      def last_insert_row_id
        @connection.last_insert_row_id
      end

      def columns(table_name)
        result = []
        raw = @connection.execute("PRAGMA table_info(\"#{table_name}\")")
        raw.each do |row|
          # PRAGMA table_info returns: cid, name, type, notnull, dflt_value, pk
          cid = row[0]
          name = row[1]
          type = row[2]
          notnull = row[3]
          dflt_value = row[4]
          pk = row[5]
          result << Column.new(name, dflt_value, type, notnull == 0, pk == 1)
        end
        result
      end

      def tables
        rows = @connection.execute(
          "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'"
        )
        rows.map { |r| r[0] }
      end

      def create_table(table_name, options = {})
        td = TableDefinition.new(table_name, options)
        yield td if block_given?
        sql = td.to_sql
        execute(sql)
      end

      def add_column(table_name, column_name, type, options = {})
        sql_type = _type_to_sql(type, options)
        sql = "ALTER TABLE \"#{table_name}\" ADD COLUMN \"#{column_name}\" #{sql_type}"
        if options[:null] == false
          sql = sql + " NOT NULL"
        end
        if options.key?(:default)
          default_val = _quote_default(options[:default])
          sql = sql + " DEFAULT #{default_val}"
        end
        execute(sql)
      end

      def add_index(table_name, column_names, options = {})
        column_names = Array(column_names)
        index_name = options[:name] || "index_#{table_name}_on_#{column_names.join('_')}"
        unique = options[:unique] ? "UNIQUE " : ""
        cols = column_names.map { |c| "\"#{c}\"" }.join(", ")
        sql = "CREATE #{unique}INDEX \"#{index_name}\" ON \"#{table_name}\" (#{cols})"
        execute(sql)
      end

      def remove_column(table_name, column_name)
        # SQLite doesn't support DROP COLUMN in older versions; for simplicity we do it if supported
        execute("ALTER TABLE \"#{table_name}\" DROP COLUMN \"#{column_name}\"")
      end

      def drop_table(table_name)
        execute("DROP TABLE IF EXISTS \"#{table_name}\"")
      end

      def transaction
        @connection.transaction { yield }
      end

      def _type_to_sql(type, options = {})
        case type.to_sym
        when :string, :text
          limit = options[:limit]
          if limit
            "VARCHAR(#{limit})"
          else
            "TEXT"
          end
        when :integer, :bigint
          "INTEGER"
        when :float, :real
          "REAL"
        when :decimal, :numeric
          "NUMERIC"
        when :boolean
          "INTEGER"
        when :datetime, :timestamp
          "DATETIME"
        when :date
          "DATE"
        when :time
          "TIME"
        when :binary, :blob
          "BLOB"
        when :primary_key
          "INTEGER PRIMARY KEY AUTOINCREMENT"
        else
          type.to_s.upcase
        end
      end

      def _quote_default(value)
        case value
        when nil
          "NULL"
        when ::String
          "'#{value.gsub("'", "''")}'"
        when ::Integer, ::Float
          value.to_s
        when true
          "1"
        when false
          "0"
        else
          "'#{value.to_s.gsub("'", "''")}'"
        end
      end
    end

    class QueryResult
      attr_reader :columns, :rows

      def initialize(columns, rows)
        @columns = columns || []
        @rows = rows || []
      end

      def each
        @rows.each { |row| yield row }
      end

      def to_a
        @rows
      end

      def empty?
        @rows.empty?
      end

      def length
        @rows.length
      end
    end

    class TableDefinition
      attr_reader :name, :columns_sql

      def initialize(name, options = {})
        @name = name
        @options = options
        @columns_sql = []
        @id = options.fetch(:id, true)
        if @id
          pk = options[:primary_key] || "id"
          @columns_sql << "\"#{pk}\" INTEGER PRIMARY KEY AUTOINCREMENT"
        end
      end

      def column(name, type, options = {})
        sql_type = _type_to_sql(type, options)
        parts = ["\"#{name}\" #{sql_type}"]
        if options[:null] == false
          parts << "NOT NULL"
        end
        if options.key?(:default)
          parts << "DEFAULT #{_quote_default(options[:default])}"
        end
        @columns_sql << parts.join(" ")
      end

      def string(name, options = {})
        column(name, :string, options)
      end

      def text(name, options = {})
        column(name, :text, options)
      end

      def integer(name, options = {})
        column(name, :integer, options)
      end

      def float(name, options = {})
        column(name, :float, options)
      end

      def decimal(name, options = {})
        column(name, :decimal, options)
      end

      def boolean(name, options = {})
        column(name, :boolean, options)
      end

      def datetime(name, options = {})
        column(name, :datetime, options)
      end

      def timestamp(name, options = {})
        column(name, :datetime, options)
      end

      def date(name, options = {})
        column(name, :date, options)
      end

      def time(name, options = {})
        column(name, :time, options)
      end

      def binary(name, options = {})
        column(name, :binary, options)
      end

      def references(name, options = {})
        column("#{name}_id", :integer, options)
        if options[:index] != false
          @_pending_indexes ||= []
          @_pending_indexes << "#{name}_id"
        end
      end
      alias belongs_to references

      def timestamps(options = {})
        column(:created_at, :datetime, options)
        column(:updated_at, :datetime, options)
      end

      def to_sql
        "CREATE TABLE \"#{@name}\" (#{@columns_sql.join(', ')})"
      end

      private

      def _type_to_sql(type, options = {})
        case type.to_sym
        when :string
          limit = options[:limit]
          limit ? "VARCHAR(#{limit})" : "TEXT"
        when :text then "TEXT"
        when :integer, :bigint then "INTEGER"
        when :float, :real then "REAL"
        when :decimal, :numeric then "NUMERIC"
        when :boolean then "INTEGER"
        when :datetime, :timestamp then "DATETIME"
        when :date then "DATE"
        when :time then "TIME"
        when :binary, :blob then "BLOB"
        else type.to_s.upcase
        end
      end

      def _quote_default(value)
        case value
        when nil then "NULL"
        when ::String then "'#{value.gsub("'", "''")}'"
        when ::Integer, ::Float then value.to_s
        when true then "1"
        when false then "0"
        else "'#{value.to_s.gsub("'", "''")}'"
        end
      end
    end

    # Connection pool - simplified single-connection version
    class ConnectionHandler
      def initialize
        @connections = {}
      end

      def establish_connection(config)
        adapter = config[:adapter] || config["adapter"] || "sqlite3"
        case adapter
        when "sqlite3"
          @connections[adapter] = SQLite3Adapter.new(config)
        else
          raise "Unknown adapter: #{adapter}"
        end
        @connections[adapter]
      end

      def retrieve_connection(klass = nil)
        # For simplicity, return the first (and likely only) connection
        conn = @connections.values.first
        unless conn
          raise ConnectionNotEstablished, "No connection pool for ActiveRecord::Base"
        end
        conn
      end

      def connected?
        !@connections.empty?
      end

      def clear_all_connections!
        @connections.clear
      end
    end
  end
end
