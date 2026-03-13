# frozen_string_literal: true
#
# ActiveRecord::Base - The main base class for all ActiveRecord models.
#
# Provides CRUD operations, attribute management, validations, callbacks,
# dirty tracking, associations, scopes, and query interface.

module ActiveRecord
  class Base
    # Include ActiveModel modules for validations and dirty tracking
    include ActiveModel::Validations
    include ActiveModel::Dirty
    include ActiveModel::Naming
    include ActiveModel::Conversion
    include ActiveRecord::Associations

    # --- Simple callback system (avoids define_method on singleton) ---

    class << self
      def _ar_callbacks
        @_ar_callbacks ||= {}
      end

      def inherited(subclass)
        super
        subclass.instance_variable_set(:@_ar_columns, nil)
        subclass.instance_variable_set(:@_ar_column_names, nil)
        # Copy scopes
        parent_scopes = instance_variable_get(:@_ar_scopes) || {}
        subclass.instance_variable_set(:@_ar_scopes, parent_scopes.dup)
        # Copy callbacks
        parent_cbs = instance_variable_get(:@_ar_callbacks) || {}
        new_cbs = {}
        parent_cbs.each do |name, hooks|
          new_cbs[name] = {
            before: (hooks[:before] || []).dup,
            after: (hooks[:after] || []).dup
          }
        end
        subclass.instance_variable_set(:@_ar_callbacks, new_cbs)
      end

      def before_save(*methods, &block)
        _register_ar_callback(:save, :before, methods, block)
      end

      def after_save(*methods, &block)
        _register_ar_callback(:save, :after, methods, block)
      end

      def before_create(*methods, &block)
        _register_ar_callback(:create, :before, methods, block)
      end

      def after_create(*methods, &block)
        _register_ar_callback(:create, :after, methods, block)
      end

      def before_update(*methods, &block)
        _register_ar_callback(:update, :before, methods, block)
      end

      def after_update(*methods, &block)
        _register_ar_callback(:update, :after, methods, block)
      end

      def before_destroy(*methods, &block)
        _register_ar_callback(:destroy, :before, methods, block)
      end

      def after_destroy(*methods, &block)
        _register_ar_callback(:destroy, :after, methods, block)
      end

      def before_validation(*methods, &block)
        _register_ar_callback(:validation, :before, methods, block)
      end

      def after_validation(*methods, &block)
        _register_ar_callback(:validation, :after, methods, block)
      end

      private

      def _register_ar_callback(event, kind, methods, block)
        _ar_callbacks[event] ||= { before: [], after: [] }
        methods.each do |m|
          _ar_callbacks[event][kind] << { method: m }
        end
        if block
          _ar_callbacks[event][kind] << { block: block }
        end
      end
    end

    # Instance-level callback runner
    def _run_ar_callbacks(event)
      cbs = self.class._ar_callbacks[event]
      if cbs
        (cbs[:before] || []).each do |cb|
          if cb[:method]
            result = send(cb[:method])
            return false if result == false
          elsif cb[:block]
            result = cb[:block].call(self)
            return false if result == false
          end
        end
      end

      result = yield if block_given?

      if cbs
        (cbs[:after] || []).reverse.each do |cb|
          if cb[:method]
            send(cb[:method])
          elsif cb[:block]
            cb[:block].call(self)
          end
        end
      end

      result
    end

    # --- Connection management ---

    class << self
      attr_writer :table_name, :primary_key

      def connection_handler
        @@connection_handler ||= ConnectionAdapters::ConnectionHandler.new
      end

      def establish_connection(config)
        config = config.is_a?(Hash) ? config : { adapter: config.to_s }
        connection_handler.establish_connection(config)
      end

      def connection
        connection_handler.retrieve_connection(self)
      end

      def connected?
        connection_handler.connected?
      end

      # --- Table / Column introspection ---

      def table_name
        @table_name ||= _compute_table_name
      end

      def primary_key
        @primary_key ||= "id"
      end

      def columns
        @_ar_columns ||= connection.columns(table_name)
      end

      def column_names
        @_ar_column_names ||= columns.map { |c| c.name }
      end

      def column_for_attribute(name)
        columns.find { |c| c.name == name.to_s }
      end

      def reset_column_information
        @_ar_columns = nil
        @_ar_column_names = nil
      end

      # --- Scopes ---

      def current_scope
        @_current_scope
      end

      def current_scope=(scope)
        @_current_scope = scope
      end

      def scope(name, body)
        name = name.to_sym
        (@_ar_scopes ||= {})[name] = body

        # Define scope as class method using class_eval
        sc = (class << self; self; end)
        sc.define_method(name) do |*args|
          result = body.call(*args)
          if result.is_a?(Relation)
            result
          else
            all
          end
        end
      end

      # --- Query interface (class-level, delegates to Relation) ---

      def all
        if current_scope
          current_scope
        else
          _base_relation
        end
      end

      def where(conditions = nil, *values)
        all.where(conditions, *values)
      end

      def not(conditions)
        all.not(conditions)
      end

      def order(*args)
        all.order(*args)
      end

      def limit(value)
        all.limit(value)
      end

      def offset(value)
        all.offset(value)
      end

      def select(*fields)
        all.select(*fields)
      end

      def group(*fields)
        all.group(*fields)
      end

      def joins(association_or_sql)
        all.joins(association_or_sql)
      end

      def includes(*associations)
        all.includes(*associations)
      end

      def find(id)
        if id.is_a?(Array)
          records = id.map { |i| find(i) }
          return records
        end
        record = where(primary_key.to_sym => id).first
        unless record
          raise RecordNotFound, "Couldn't find #{name} with '#{primary_key}'=#{id}"
        end
        record
      end

      def find_by(conditions)
        where(conditions).first
      end

      def find_by!(conditions)
        record = find_by(conditions)
        unless record
          raise RecordNotFound, "Couldn't find #{name}"
        end
        record
      end

      def first(limit_count = nil)
        if limit_count
          all.order("\"#{primary_key}\" ASC").limit(limit_count).to_a
        else
          all.order("\"#{primary_key}\" ASC").limit(1).first
        end
      end

      def last(limit_count = nil)
        if limit_count
          all.order("\"#{primary_key}\" DESC").limit(limit_count).to_a.reverse
        else
          all.order("\"#{primary_key}\" DESC").limit(1).first
        end
      end

      def count(column = nil)
        all.count(column)
      end

      def sum(column)
        all.sum(column)
      end

      def minimum(column)
        all.minimum(column)
      end

      def maximum(column)
        all.maximum(column)
      end

      def average(column)
        all.average(column)
      end

      def pluck(*columns)
        all.pluck(*columns)
      end

      def exists?(conditions = nil)
        if conditions.is_a?(Hash)
          where(conditions).exists?
        elsif conditions
          where(primary_key.to_sym => conditions).exists?
        else
          all.exists?
        end
      end

      def destroy_all
        all.destroy_all
      end

      def delete_all
        all.delete_all
      end

      def update_all(updates)
        all.update_all(updates)
      end

      # --- CRUD class methods ---

      def create(attributes = {})
        record = new(attributes)
        record.save
        record
      end

      def create!(attributes = {})
        record = new(attributes)
        record.save!
        record
      end

      # --- Instantiation from DB rows ---

      def _instantiate_from_row(row, col_names = nil)
        col_names ||= column_names
        attrs = {}
        col_names.each_with_index do |col, i|
          attrs[col] = row[i]
        end
        _instantiate(attrs)
      end

      def _instantiate(attrs)
        record = allocate
        record.send(:_init_from_db, attrs)
        record
      end

      private

      def _base_relation
        Relation.new(self)
      end

      def _compute_table_name
        n = self.name || ""
        base = ActiveSupport::Inflector.demodulize(n)
        ActiveSupport::Inflector.tableize(base)
      end
    end

    # --- Instance methods ---

    def initialize(attributes = {})
      @_ar_attributes = {}
      @_ar_new_record = true
      @_ar_destroyed = false
      @_ar_persisted = false
      @_changes = {}
      @_previous_changes = {}
      @_association_cache = {}

      _set_defaults
      assign_attributes(attributes) if attributes && !attributes.empty?
    end

    def assign_attributes(attrs)
      return unless attrs
      attrs.each do |key, value|
        key_s = key.to_s
        if respond_to?(:"#{key_s}=")
          send(:"#{key_s}=", value)
        elsif self.class.column_names.include?(key_s)
          _write_attribute(key_s, value)
        end
      end
    end
    alias attributes= assign_attributes

    # --- Attribute access ---

    def id
      _read_attribute(self.class.primary_key)
    end

    def id=(value)
      _write_attribute(self.class.primary_key, value)
    end

    def [](attr_name)
      _read_attribute(attr_name.to_s)
    end

    def []=(attr_name, value)
      _write_attribute(attr_name.to_s, value)
    end

    def attributes
      result = {}
      self.class.column_names.each do |col|
        result[col] = _read_attribute(col)
      end
      result
    end

    def attribute_names
      self.class.column_names
    end

    def has_attribute?(attr_name)
      self.class.column_names.include?(attr_name.to_s)
    end

    def read_attribute(attr_name)
      _read_attribute(attr_name.to_s)
    end

    def write_attribute(attr_name, value)
      _write_attribute(attr_name.to_s, value)
    end

    # --- Persistence state ---

    def new_record?
      @_ar_new_record
    end

    def persisted?
      !@_ar_new_record && !@_ar_destroyed
    end

    def destroyed?
      @_ar_destroyed
    end

    def changed?
      !_changes.empty?
    end

    # --- CRUD instance methods ---

    def save(options = {})
      begin
        result = _run_ar_callbacks(:save) do
          if new_record?
            _create_record
          else
            _update_record
          end
        end
        result != false
      rescue StandardError
        false
      end
    end

    def save!(options = {})
      result = _run_ar_callbacks(:save) do
        if new_record?
          _create_record
        else
          _update_record
        end
      end
      if result == false
        raise RecordNotSaved
      end
      true
    end

    def update(attrs)
      assign_attributes(attrs)
      save
    end

    def update!(attrs)
      assign_attributes(attrs)
      save!
    end

    def destroy
      return self if destroyed?
      _run_ar_callbacks(:destroy) do
        pk = self.class.primary_key
        pk_val = _read_attribute(pk)
        if pk_val
          sql = "DELETE FROM \"#{self.class.table_name}\" WHERE \"#{pk}\" = ?"
          self.class.connection.execute(sql, [pk_val])
        end
        @_ar_destroyed = true
        @_ar_new_record = false
      end
      self
    end

    def destroy!
      destroy || raise(RecordNotDestroyed)
    end

    def delete
      pk = self.class.primary_key
      pk_val = _read_attribute(pk)
      if pk_val
        sql = "DELETE FROM \"#{self.class.table_name}\" WHERE \"#{pk}\" = ?"
        self.class.connection.execute(sql, [pk_val])
      end
      @_ar_destroyed = true
      @_ar_new_record = false
      self
    end

    def reload
      pk = self.class.primary_key
      pk_val = _read_attribute(pk)
      raise "can't reload a new record" unless pk_val

      sql = "SELECT * FROM \"#{self.class.table_name}\" WHERE \"#{pk}\" = ? LIMIT 1"
      rows = self.class.connection.execute(sql, [pk_val])
      if rows.empty?
        raise RecordNotFound, "Couldn't find #{self.class.name} with '#{pk}'=#{pk_val}"
      end
      col_names = self.class.column_names
      row = rows.first
      col_names.each_with_index do |col, i|
        @_ar_attributes[col] = _type_cast_from_db(col, row[i])
      end
      @_changes = {}
      @_previous_changes = {}
      self
    end

    # --- Comparison ---

    def ==(other)
      other.is_a?(self.class) && !id.nil? && id == other.id
    end
    alias eql? ==

    def hash
      if id
        id.hash
      else
        super
      end
    end

    # --- Inspection ---

    def inspect
      attrs = self.class.column_names.map do |col|
        val = _read_attribute(col)
        "%s: %s" % [col, val.inspect]
      end
      "#<%s %s>" % [self.class.name, attrs.join(", ")]
    end

    def to_s
      inspect
    end

    # --- Dynamic attribute and association methods ---

    def respond_to_missing?(method_name, include_private = false)
      name_s = method_name.to_s
      if name_s.end_with?("=")
        col = name_s[0..-2]
        return true if self.class.column_names.include?(col)
        return true if self.class._ar_associations.key?(col.to_sym)
      else
        return true if self.class.column_names.include?(name_s)
        return true if self.class._ar_associations.key?(name_s.to_sym)
      end
      super
    end

    def method_missing(method_name, *args, &block)
      name_s = method_name.to_s
      if name_s.end_with?("=")
        col = name_s[0..-2]
        if self.class.column_names.include?(col)
          _write_attribute(col, args[0])
          return args[0]
        end
        if self.class._ar_associations.key?(col.to_sym)
          return _set_association(col.to_sym, args[0])
        end
      else
        if self.class.column_names.include?(name_s)
          return _read_attribute(name_s)
        end
        if self.class._ar_associations.key?(name_s.to_sym)
          return _get_association(name_s.to_sym)
        end
      end
      super
    end

    private

    # --- Internal attribute management ---

    def _read_attribute(name)
      @_ar_attributes[name.to_s]
    end

    def _write_attribute(name, value)
      name_s = name.to_s
      old_value = @_ar_attributes[name_s]
      casted = _type_cast_for_write(name_s, value)
      @_ar_attributes[name_s] = casted
      # Track dirty changes
      _track_attribute_change(name_s.to_sym, old_value, casted)
      casted
    end

    def _type_cast_for_write(col_name, value)
      column = self.class.column_for_attribute(col_name)
      if column
        column.type_cast(value)
      else
        value
      end
    end

    def _type_cast_from_db(col_name, value)
      column = self.class.column_for_attribute(col_name)
      if column
        column.type_cast(value)
      else
        value
      end
    end

    def _set_defaults
      self.class.columns.each do |col|
        if col.default
          @_ar_attributes[col.name] = _type_cast_from_db(col.name, col.default)
        end
      end
    end

    def _init_from_db(attrs)
      @_ar_attributes = {}
      @_ar_new_record = false
      @_ar_destroyed = false
      @_ar_persisted = true
      @_changes = {}
      @_previous_changes = {}
      @_association_cache = {}

      attrs.each do |key, value|
        @_ar_attributes[key.to_s] = _type_cast_from_db(key.to_s, value)
      end
    end

    def _create_record
      _run_ar_callbacks(:create) do
        columns = self.class.column_names
        pk = self.class.primary_key

        # Collect non-nil attributes (excluding auto-increment pk)
        insert_cols = []
        insert_vals = []
        columns.each do |col|
          next if col == pk && _read_attribute(col).nil?
          val = _read_attribute(col)
          next if val.nil? && col == pk
          insert_cols << col
          col_obj = self.class.column_for_attribute(col)
          insert_vals << (col_obj ? col_obj.serialize(val) : val)
        end

        if insert_cols.empty?
          sql = "INSERT INTO \"#{self.class.table_name}\" DEFAULT VALUES"
          self.class.connection.execute(sql)
        else
          placeholders = insert_cols.map { "?" }.join(", ")
          col_list = insert_cols.map { |c| "\"#{c}\"" }.join(", ")
          sql = "INSERT INTO \"#{self.class.table_name}\" (#{col_list}) VALUES (#{placeholders})"
          self.class.connection.execute(sql, insert_vals)
        end

        new_id = self.class.connection.last_insert_row_id
        @_ar_attributes[pk] = new_id

        @_ar_new_record = false
        @_ar_persisted = true
        changes_applied
      end
      true
    end

    def _update_record
      _run_ar_callbacks(:update) do
        pk = self.class.primary_key
        pk_val = _read_attribute(pk)

        changed_attrs = _changes
        return true if changed_attrs.empty?

        set_parts = []
        binds = []
        changed_attrs.each do |attr, change|
          col_name = attr.to_s
          next if col_name == pk
          new_val = change[1]
          col_obj = self.class.column_for_attribute(col_name)
          serialized = col_obj ? col_obj.serialize(new_val) : new_val
          set_parts << "\"#{col_name}\" = ?"
          binds << serialized
        end

        return true if set_parts.empty?

        binds << pk_val
        sql = "UPDATE \"#{self.class.table_name}\" SET #{set_parts.join(', ')} WHERE \"#{pk}\" = ?"
        self.class.connection.execute(sql, binds)

        changes_applied
      end
      true
    end
  end
end
