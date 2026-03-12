# frozen_string_literal: true
#
# ActiveRecord::Associations - has_many, belongs_to, has_one DSL.
#
# Uses method_missing instead of define_method to avoid monoruby issues
# with closures in define_method on included modules.

module ActiveRecord
  module Associations
    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_ar_associations, {})
    end

    module ClassMethods
      def inherited(subclass)
        super
        parent_assocs = instance_variable_get(:@_ar_associations) || {}
        subclass.instance_variable_set(:@_ar_associations, parent_assocs.dup)
      end

      def _ar_associations
        @_ar_associations ||= {}
      end

      # has_many :posts
      def has_many(name, options = {})
        name = name.to_sym
        class_name = options[:class_name] || _classify_association(name)
        foreign_key = options[:foreign_key] || _default_foreign_key
        dependent = options[:dependent]

        _ar_associations[name] = {
          type: :has_many,
          class_name: class_name,
          foreign_key: foreign_key,
          dependent: dependent
        }
      end

      # belongs_to :user
      def belongs_to(name, options = {})
        name = name.to_sym
        class_name = options[:class_name] || _classify_association_singular(name)
        foreign_key = options[:foreign_key] || "#{name}_id"
        optional = options.fetch(:optional, false)

        _ar_associations[name] = {
          type: :belongs_to,
          class_name: class_name,
          foreign_key: foreign_key,
          optional: optional
        }
      end

      # has_one :profile
      def has_one(name, options = {})
        name = name.to_sym
        class_name = options[:class_name] || _classify_association_singular(name)
        foreign_key = options[:foreign_key] || _default_foreign_key
        dependent = options[:dependent]

        _ar_associations[name] = {
          type: :has_one,
          class_name: class_name,
          foreign_key: foreign_key,
          dependent: dependent
        }
      end

      private

      def _classify_association(name)
        word = name.to_s
        singular = ActiveSupport::Inflector.singularize(word)
        s = singular
        s = s[0].upcase + s[1..-1] if s.length > 0
        s
      end

      def _classify_association_singular(name)
        s = name.to_s
        s = s[0].upcase + s[1..-1] if s.length > 0
        s
      end

      def _default_foreign_key
        n = self.name || ""
        base = ActiveSupport::Inflector.underscore(ActiveSupport::Inflector.demodulize(n))
        "#{base}_id"
      end
    end

    # --- Instance-level association access via method_missing ---

    def _get_association(assoc_name)
      assoc_name = assoc_name.to_sym
      assoc_def = self.class._ar_associations[assoc_name]
      return nil unless assoc_def

      # Use a hash for association caching instead of dynamic ivars
      @_association_cache ||= {}

      case assoc_def[:type]
      when :has_many
        cached = @_association_cache[assoc_name]
        if cached
          return cached
        end
        proxy = CollectionProxy.new(self, assoc_name, assoc_def)
        @_association_cache[assoc_name] = proxy
        proxy

      when :belongs_to
        if @_association_cache.key?(assoc_name)
          return @_association_cache[assoc_name]
        end
        fk_value = _read_attribute(assoc_def[:foreign_key])
        return nil if fk_value.nil?
        assoc_class = _resolve_association_class(assoc_def[:class_name])
        return nil unless assoc_class
        result = assoc_class.find_by(assoc_class.primary_key.to_sym => fk_value)
        @_association_cache[assoc_name] = result
        result

      when :has_one
        if @_association_cache.key?(assoc_name)
          return @_association_cache[assoc_name]
        end
        assoc_class = _resolve_association_class(assoc_def[:class_name])
        return nil unless assoc_class
        result = assoc_class.find_by(assoc_def[:foreign_key].to_sym => self.id)
        @_association_cache[assoc_name] = result
        result
      end
    end

    def _set_association(assoc_name, value)
      assoc_name = assoc_name.to_sym
      assoc_def = self.class._ar_associations[assoc_name]
      return unless assoc_def

      @_association_cache ||= {}

      case assoc_def[:type]
      when :belongs_to
        if value
          _write_attribute(assoc_def[:foreign_key], value.id)
        else
          _write_attribute(assoc_def[:foreign_key], nil)
        end
        @_association_cache[assoc_name] = value

      when :has_one
        if value
          value.send(:_write_attribute, assoc_def[:foreign_key], self.id)
          value.save
        end
        @_association_cache[assoc_name] = value

      when :has_many
        @_association_cache[assoc_name] = value
      end
    end

    def respond_to_missing?(method_name, include_private = false)
      name_s = method_name.to_s
      if name_s.end_with?("=")
        assoc = name_s[0..-2].to_sym
        return true if self.class._ar_associations.key?(assoc)
      else
        return true if self.class._ar_associations.key?(method_name.to_sym)
      end
      super
    end

    private

    def _resolve_association_class(class_name)
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

  # CollectionProxy wraps a has_many relationship
  class CollectionProxy
    include Enumerable

    def initialize(owner, name, association_def)
      @owner = owner
      @name = name
      @association_def = association_def
      @loaded = false
      @records = []
    end

    def load_target
      return @records if @loaded
      assoc_class = _resolve_class(@association_def[:class_name])
      return [] unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      @records = assoc_class.where(fk_sym => @owner.id).to_a
      @loaded = true
      @records
    end

    def each(&block)
      load_target
      @records.each(&block)
    end

    def to_a
      load_target
      @records
    end

    def map(&block)
      load_target
      @records.map(&block)
    end

    def size
      if @loaded
        @records.length
      else
        count
      end
    end
    alias length size

    def count
      assoc_class = _resolve_class(@association_def[:class_name])
      return 0 unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).count
    end

    def empty?
      size == 0
    end

    def any?
      if block_given?
        load_target
        @records.any? { |r| yield r }
      else
        size > 0
      end
    end

    def first
      load_target
      @records.first
    end

    def last
      load_target
      @records.last
    end

    def include?(record)
      load_target
      @records.include?(record)
    end

    def create(attributes = {})
      assoc_class = _resolve_class(@association_def[:class_name])
      return nil unless assoc_class
      fk = @association_def[:foreign_key]
      merged = {}
      attributes.each { |k, v| merged[k] = v }
      merged[fk.to_s] = @owner.id
      record = assoc_class.create(merged)
      if record.persisted?
        @records << record if @loaded
      end
      record
    end

    def build(attributes = {})
      assoc_class = _resolve_class(@association_def[:class_name])
      return nil unless assoc_class
      fk = @association_def[:foreign_key]
      merged = {}
      attributes.each { |k, v| merged[k] = v }
      merged[fk.to_s] = @owner.id
      record = assoc_class.new(merged)
      @records << record if @loaded
      record
    end
    alias new build

    def <<(record)
      fk = @association_def[:foreign_key]
      record.send(:_write_attribute, fk, @owner.id)
      record.save
      @records << record if @loaded
      self
    end
    alias push <<

    def delete(record)
      fk = @association_def[:foreign_key]
      record.send(:_write_attribute, fk, nil)
      record.save
      @records.delete(record) if @loaded
    end

    def destroy_all
      load_target
      @records.each { |r| r.destroy }
      @records = []
    end

    def delete_all
      assoc_class = _resolve_class(@association_def[:class_name])
      return unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).delete_all
      @records = []
      @loaded = true
    end

    def where(conditions = nil, *values)
      assoc_class = _resolve_class(@association_def[:class_name])
      return ActiveRecord::Relation.new(assoc_class) unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).where(conditions, *values)
    end

    def order(*args)
      assoc_class = _resolve_class(@association_def[:class_name])
      return ActiveRecord::Relation.new(assoc_class) unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).order(*args)
    end

    def limit(value)
      assoc_class = _resolve_class(@association_def[:class_name])
      return ActiveRecord::Relation.new(assoc_class) unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).limit(value)
    end

    def pluck(*columns)
      assoc_class = _resolve_class(@association_def[:class_name])
      return [] unless assoc_class
      fk = @association_def[:foreign_key]
      fk_sym = fk.to_sym
      assoc_class.where(fk_sym => @owner.id).pluck(*columns)
    end

    def reload
      @loaded = false
      @records = []
      load_target
      self
    end

    def inspect
      load_target
      @records.inspect
    end

    private

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
