# frozen_string_literal: true
#
# ActiveRecord::Relation - Lazy-evaluated query builder with chainable methods.

module ActiveRecord
  class Relation
    include Enumerable

    attr_reader :klass, :loaded, :records

    def initialize(klass)
      @klass = klass
      @where_clauses = []
      @where_binds = []
      @order_clauses = []
      @limit_value = nil
      @offset_value = nil
      @includes_values = []
      @joins_clauses = []
      @select_values = []
      @group_values = []
      @loaded = false
      @records = []
    end

    # --- Chainable query methods ---

    def where(conditions = nil, *values)
      spawn._where!(conditions, *values)
    end

    def _where!(conditions, *values)
      case conditions
      when Hash
        conditions.each do |key, value|
          col = key.to_s
          if value.nil?
            @where_clauses << "\"#{col}\" IS NULL"
          elsif value.is_a?(Array)
            placeholders = value.map { "?" }.join(", ")
            @where_clauses << "\"#{col}\" IN (#{placeholders})"
            @where_binds.concat(value)
          else
            @where_clauses << "\"#{col}\" = ?"
            @where_binds << value
          end
        end
      when ::String
        @where_clauses << conditions
        @where_binds.concat(values)
      when nil
        # no-op, return self for chaining
      end
      self
    end

    def not(conditions)
      spawn._not!(conditions)
    end

    def _not!(conditions)
      case conditions
      when Hash
        conditions.each do |key, value|
          col = key.to_s
          if value.nil?
            @where_clauses << "\"#{col}\" IS NOT NULL"
          elsif value.is_a?(Array)
            placeholders = value.map { "?" }.join(", ")
            @where_clauses << "\"#{col}\" NOT IN (#{placeholders})"
            @where_binds.concat(value)
          else
            @where_clauses << "\"#{col}\" != ?"
            @where_binds << value
          end
        end
      end
      self
    end

    def order(*args)
      spawn._order!(*args)
    end

    def _order!(*args)
      args.each do |arg|
        case arg
        when ::String, Symbol
          @order_clauses << arg.to_s
        when Hash
          arg.each do |col, dir|
            @order_clauses << "\"#{col}\" #{dir.to_s.upcase}"
          end
        end
      end
      self
    end

    def limit(value)
      spawn._limit!(value)
    end

    def _limit!(value)
      @limit_value = value
      self
    end

    def offset(value)
      spawn._offset!(value)
    end

    def _offset!(value)
      @offset_value = value
      self
    end

    def includes(*associations)
      spawn._includes!(*associations)
    end

    def _includes!(*associations)
      @includes_values.concat(associations.flatten)
      self
    end

    def joins(association_or_sql)
      spawn._joins!(association_or_sql)
    end

    def _joins!(association_or_sql)
      case association_or_sql
      when ::String
        @joins_clauses << association_or_sql
      when Symbol
        # Resolve association to JOIN clause
        assoc = @klass._ar_associations[association_or_sql]
        if assoc
          case assoc[:type]
          when :belongs_to
            foreign_key = assoc[:foreign_key]
            other_table = assoc[:class_name].tableize
            @joins_clauses << "INNER JOIN \"#{other_table}\" ON \"#{other_table}\".\"id\" = \"#{@klass.table_name}\".\"#{foreign_key}\""
          when :has_many, :has_one
            foreign_key = assoc[:foreign_key]
            other_table = assoc[:class_name].tableize
            @joins_clauses << "INNER JOIN \"#{other_table}\" ON \"#{other_table}\".\"#{foreign_key}\" = \"#{@klass.table_name}\".\"id\""
          end
        end
      end
      self
    end

    def select(*fields)
      spawn._select!(*fields)
    end

    def _select!(*fields)
      @select_values.concat(fields.flatten.map(&:to_s))
      self
    end

    def group(*fields)
      spawn._group!(*fields)
    end

    def _group!(*fields)
      @group_values.concat(fields.flatten.map(&:to_s))
      self
    end

    # --- Terminal methods ---

    def to_a
      load
      @records
    end

    def all
      load
      @records
    end

    def each(&block)
      load
      @records.each(&block)
    end

    def map(&block)
      load
      @records.map(&block)
    end

    def first(limit_count = nil)
      if limit_count
        limit(limit_count).load.records
      else
        r = limit(1).load.records
        r.first
      end
    end

    def last(limit_count = nil)
      rel = order("\"#{@klass.primary_key}\" DESC")
      if limit_count
        rel.limit(limit_count).load.records.reverse
      else
        r = rel.limit(1).load.records
        r.first
      end
    end

    def find(id)
      record = where(@klass.primary_key.to_sym => id).first
      unless record
        raise RecordNotFound, "Couldn't find #{@klass.name} with '#{@klass.primary_key}'=#{id}"
      end
      record
    end

    def find_by(conditions)
      where(conditions).first
    end

    def exists?(conditions = nil)
      if conditions
        where(conditions).limit(1).load.records.length > 0
      else
        limit(1).load.records.length > 0
      end
    end

    def count(column = nil)
      col_expr = column ? "\"#{column}\"" : "*"
      sql = "SELECT COUNT(#{col_expr}) FROM \"#{@klass.table_name}\""
      sql = sql + _joins_sql
      sql = sql + _where_sql
      sql = sql + _group_sql
      result = @klass.connection.execute(sql, @where_binds)
      if result && result.first
        result.first[0].to_i
      else
        0
      end
    end

    def sum(column)
      sql = "SELECT SUM(\"#{column}\") FROM \"#{@klass.table_name}\""
      sql = sql + _joins_sql
      sql = sql + _where_sql
      result = @klass.connection.execute(sql, @where_binds)
      if result && result.first && result.first[0]
        result.first[0]
      else
        0
      end
    end

    def minimum(column)
      sql = "SELECT MIN(\"#{column}\") FROM \"#{@klass.table_name}\""
      sql = sql + _where_sql
      result = @klass.connection.execute(sql, @where_binds)
      result && result.first ? result.first[0] : nil
    end

    def maximum(column)
      sql = "SELECT MAX(\"#{column}\") FROM \"#{@klass.table_name}\""
      sql = sql + _where_sql
      result = @klass.connection.execute(sql, @where_binds)
      result && result.first ? result.first[0] : nil
    end

    def average(column)
      sql = "SELECT AVG(\"#{column}\") FROM \"#{@klass.table_name}\""
      sql = sql + _where_sql
      result = @klass.connection.execute(sql, @where_binds)
      result && result.first ? result.first[0] : nil
    end

    def pluck(*columns)
      cols = columns.map { |c| "\"#{c}\"" }.join(", ")
      sql = "SELECT #{cols} FROM \"#{@klass.table_name}\""
      sql = sql + _joins_sql
      sql = sql + _where_sql
      sql = sql + _order_sql
      sql = sql + _limit_sql
      sql = sql + _offset_sql
      rows = @klass.connection.execute(sql, @where_binds)
      if columns.length == 1
        rows.map { |r| r[0] }
      else
        rows
      end
    end

    def destroy_all
      load
      @records.each { |r| r.destroy }
      @records
    end

    def update_all(updates)
      set_parts = []
      binds = []
      updates.each do |col, val|
        set_parts << "\"#{col}\" = ?"
        binds << val
      end
      sql = "UPDATE \"#{@klass.table_name}\" SET #{set_parts.join(', ')}"
      sql = sql + _where_sql
      binds.concat(@where_binds)
      @klass.connection.execute(sql, binds)
    end

    def delete_all
      sql = "DELETE FROM \"#{@klass.table_name}\""
      sql = sql + _where_sql
      @klass.connection.execute(sql, @where_binds)
    end

    # Create a record scoped to this relation's conditions
    def create(attributes = {})
      # Merge where conditions that are simple equality into attributes
      merged = {}
      @where_clauses.each_with_index do |clause, i|
        # Try to extract "col" = ? patterns for simple equality
        if clause =~ /\A"(\w+)" = \?\z/
          col_name = $1
          merged[col_name.to_sym] = @where_binds[i] if @where_binds[i]
        end
      end
      merged.merge!(attributes)
      @klass.create(merged)
    end

    def build(attributes = {})
      merged = {}
      @where_clauses.each_with_index do |clause, i|
        if clause =~ /\A"(\w+)" = \?\z/
          col_name = $1
          merged[col_name.to_sym] = @where_binds[i] if @where_binds[i]
        end
      end
      merged.merge!(attributes)
      @klass.new(merged)
    end
    alias new build

    def empty?
      count == 0
    end

    def any?
      if block_given?
        load
        @records.any? { |r| yield r }
      else
        count > 0
      end
    end

    def none?
      if block_given?
        load
        @records.none? { |r| yield r }
      else
        count == 0
      end
    end

    def size
      if @loaded
        @records.length
      else
        count
      end
    end

    def length
      load
      @records.length
    end

    def inspect
      load
      "#<ActiveRecord::Relation [%s]>" % [@records.map(&:inspect).join(", ")]
    end

    def to_sql
      _build_select_sql
    end

    def load
      return self if @loaded
      sql = _build_select_sql
      rows = @klass.connection.execute(sql, @where_binds)
      column_names = @klass.column_names
      @records = rows.map { |row| @klass._instantiate_from_row(row, column_names) }
      # Handle eager loading
      _perform_eager_loading if @includes_values.any?
      @loaded = true
      self
    end

    def reload
      @loaded = false
      @records = []
      load
    end

    # Scoping support - allow relation to act as scope
    def scoping
      previous = @klass.current_scope
      @klass.current_scope = self
      yield
    ensure
      @klass.current_scope = previous
    end

    def respond_to_missing?(method_name, include_private = false)
      @klass.respond_to?(method_name) || super
    end

    private

    def method_missing(method_name, *args, &block)
      if @klass.respond_to?(method_name)
        scoping { @klass.send(method_name, *args, &block) }
      else
        super
      end
    end

    def spawn
      rel = Relation.new(@klass)
      rel.instance_variable_set(:@where_clauses, @where_clauses.dup)
      rel.instance_variable_set(:@where_binds, @where_binds.dup)
      rel.instance_variable_set(:@order_clauses, @order_clauses.dup)
      rel.instance_variable_set(:@limit_value, @limit_value)
      rel.instance_variable_set(:@offset_value, @offset_value)
      rel.instance_variable_set(:@includes_values, @includes_values.dup)
      rel.instance_variable_set(:@joins_clauses, @joins_clauses.dup)
      rel.instance_variable_set(:@select_values, @select_values.dup)
      rel.instance_variable_set(:@group_values, @group_values.dup)
      rel
    end

    def _build_select_sql
      cols = if @select_values.any?
        @select_values.join(", ")
      else
        "\"#{@klass.table_name}\".*"
      end
      sql = "SELECT #{cols} FROM \"#{@klass.table_name}\""
      sql = sql + _joins_sql
      sql = sql + _where_sql
      sql = sql + _group_sql
      sql = sql + _order_sql
      sql = sql + _limit_sql
      sql = sql + _offset_sql
      sql
    end

    def _where_sql
      return "" if @where_clauses.empty?
      " WHERE " + @where_clauses.join(" AND ")
    end

    def _order_sql
      return "" if @order_clauses.empty?
      " ORDER BY " + @order_clauses.join(", ")
    end

    def _limit_sql
      return "" unless @limit_value
      " LIMIT #{@limit_value.to_i}"
    end

    def _offset_sql
      return "" unless @offset_value
      " OFFSET #{@offset_value.to_i}"
    end

    def _joins_sql
      return "" if @joins_clauses.empty?
      " " + @joins_clauses.join(" ")
    end

    def _group_sql
      return "" if @group_values.empty?
      " GROUP BY " + @group_values.map { |g| "\"#{g}\"" }.join(", ")
    end

    def _perform_eager_loading
      @includes_values.each do |assoc_name|
        assoc = @klass._ar_associations[assoc_name.to_sym]
        next unless assoc

        case assoc[:type]
        when :has_many
          _eager_load_has_many(assoc_name, assoc)
        when :has_one
          _eager_load_has_one(assoc_name, assoc)
        when :belongs_to
          _eager_load_belongs_to(assoc_name, assoc)
        end
      end
    end

    def _eager_load_has_many(assoc_name, assoc)
      return if @records.empty?
      ids = @records.map { |r| r.id }
      return if ids.empty?

      foreign_key = assoc[:foreign_key]
      assoc_class = _resolve_class(assoc[:class_name])
      return unless assoc_class

      placeholders = ids.map { "?" }.join(", ")
      sql = "SELECT * FROM \"#{assoc_class.table_name}\" WHERE \"#{foreign_key}\" IN (#{placeholders})"
      rows = assoc_class.connection.execute(sql, ids)
      col_names = assoc_class.column_names

      # Group by foreign key
      grouped = {}
      rows.each do |row|
        record = assoc_class._instantiate_from_row(row, col_names)
        fk_val = record.send(foreign_key)
        (grouped[fk_val] ||= []) << record
      end

      @records.each do |parent|
        children = grouped[parent.id] || []
        parent.instance_variable_set(:"@_association_cache_#{assoc_name}", children)
      end
    end

    def _eager_load_has_one(assoc_name, assoc)
      return if @records.empty?
      ids = @records.map { |r| r.id }
      return if ids.empty?

      foreign_key = assoc[:foreign_key]
      assoc_class = _resolve_class(assoc[:class_name])
      return unless assoc_class

      placeholders = ids.map { "?" }.join(", ")
      sql = "SELECT * FROM \"#{assoc_class.table_name}\" WHERE \"#{foreign_key}\" IN (#{placeholders})"
      rows = assoc_class.connection.execute(sql, ids)
      col_names = assoc_class.column_names

      grouped = {}
      rows.each do |row|
        record = assoc_class._instantiate_from_row(row, col_names)
        fk_val = record.send(foreign_key)
        grouped[fk_val] = record
      end

      @records.each do |parent|
        child = grouped[parent.id]
        parent.instance_variable_set(:"@_association_cache_#{assoc_name}", child)
      end
    end

    def _eager_load_belongs_to(assoc_name, assoc)
      return if @records.empty?
      foreign_key = assoc[:foreign_key]
      ids = @records.map { |r| r.send(foreign_key) }.compact.uniq
      return if ids.empty?

      assoc_class = _resolve_class(assoc[:class_name])
      return unless assoc_class

      placeholders = ids.map { "?" }.join(", ")
      sql = "SELECT * FROM \"#{assoc_class.table_name}\" WHERE \"#{assoc_class.primary_key}\" IN (#{placeholders})"
      rows = assoc_class.connection.execute(sql, ids)
      col_names = assoc_class.column_names

      indexed = {}
      rows.each do |row|
        record = assoc_class._instantiate_from_row(row, col_names)
        indexed[record.id] = record
      end

      @records.each do |parent|
        fk_val = parent.send(foreign_key)
        parent.instance_variable_set(:"@_association_cache_#{assoc_name}", indexed[fk_val])
      end
    end

    def _resolve_class(class_name)
      if class_name.is_a?(::String)
        begin
          Object.const_get(class_name)
        rescue NameError
          nil
        end
      else
        class_name
      end
    end
  end
end
