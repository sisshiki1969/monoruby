# frozen_string_literal: true
#
# ActiveRecord::Associations - has_many, belongs_to, has_one DSL.

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
      # has_many :posts, class_name: "BlogPost", foreign_key: "author_id"
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

        # Define getter
        define_method(name) do
          cache_ivar = :"@_association_cache_#{name}"
          if instance_variable_defined?(cache_ivar)
            cached = instance_variable_get(cache_ivar)
            # If it's already an array from eager loading, wrap it in a CollectionProxy
            if cached.is_a?(Array)
              proxy = CollectionProxy.new(self, name, self.class._ar_associations[name])
              proxy.instance_variable_set(:@loaded, true)
              proxy.instance_variable_set(:@records, cached)
              instance_variable_set(cache_ivar, proxy)
              return proxy
            end
            return cached
          end
          proxy = CollectionProxy.new(self, name, self.class._ar_associations[name])
          instance_variable_set(cache_ivar, proxy)
          proxy
        end

        # Define setter
        define_method(:"#{name}=") do |records|
          cache_ivar = :"@_association_cache_#{name}"
          instance_variable_set(cache_ivar, records)
        end
      end

      # belongs_to :user
      # belongs_to :author, class_name: "User", foreign_key: "author_id"
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

        # Define getter
        define_method(name) do
          cache_ivar = :"@_association_cache_#{name}"
          if instance_variable_defined?(cache_ivar)
            return instance_variable_get(cache_ivar)
          end
          fk_value = send(foreign_key)
          return nil if fk_value.nil?
          assoc_def = self.class._ar_associations[name]
          assoc_class = _resolve_association_class(assoc_def[:class_name])
          return nil unless assoc_class
          result = assoc_class.find_by(assoc_class.primary_key.to_sym => fk_value)
          instance_variable_set(cache_ivar, result)
          result
        end

        # Define setter
        define_method(:"#{name}=") do |record|
          cache_ivar = :"@_association_cache_#{name}"
          if record
            send(:"#{foreign_key}=", record.id)
          else
            send(:"#{foreign_key}=", nil)
          end
          instance_variable_set(cache_ivar, record)
        end
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

        # Define getter
        define_method(name) do
          cache_ivar = :"@_association_cache_#{name}"
          if instance_variable_defined?(cache_ivar)
            return instance_variable_get(cache_ivar)
          end
          assoc_def = self.class._ar_associations[name]
          assoc_class = _resolve_association_class(assoc_def[:class_name])
          return nil unless assoc_class
          result = assoc_class.find_by(assoc_def[:foreign_key].to_sym => self.id)
          instance_variable_set(cache_ivar, result)
          result
        end

        # Define setter
        define_method(:"#{name}=") do |record|
          cache_ivar = :"@_association_cache_#{name}"
          if record
            assoc_def = self.class._ar_associations[name]
            record.send(:"#{assoc_def[:foreign_key]}=", self.id)
            record.save
          end
          instance_variable_set(cache_ivar, record)
        end
      end

      private

      def _classify_association(name)
        # has_many :posts -> "Post"
        word = name.to_s
        # singularize then classify
        singular = ActiveSupport::Inflector.singularize(word)
        s = singular
        s = s[0].upcase + s[1..-1] if s.length > 0
        s
      end

      def _classify_association_singular(name)
        # belongs_to :user -> "User"
        s = name.to_s
        s = s[0].upcase + s[1..-1] if s.length > 0
        s
      end

      def _default_foreign_key
        # User -> user_id
        n = self.name || ""
        base = ActiveSupport::Inflector.underscore(ActiveSupport::Inflector.demodulize(n))
        "#{base}_id"
      end
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
      foreign_key = @association_def[:foreign_key]
      @records = assoc_class.where(foreign_key.to_sym => @owner.id).to_a
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
        _build_relation.count
      end
    end
    alias length size

    def count
      _build_relation.count
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
      foreign_key = @association_def[:foreign_key]
      record.send(:"#{foreign_key}=", @owner.id)
      record.save
      @records << record if @loaded
      self
    end
    alias push <<

    def delete(record)
      foreign_key = @association_def[:foreign_key]
      record.send(:"#{foreign_key}=", nil)
      record.save
      @records.delete(record) if @loaded
    end

    def destroy_all
      load_target
      @records.each { |r| r.destroy }
      @records = []
    end

    def delete_all
      _build_relation.delete_all
      @records = []
      @loaded = true
    end

    def where(conditions = nil, *values)
      _build_relation.where(conditions, *values)
    end

    def order(*args)
      _build_relation.order(*args)
    end

    def limit(value)
      _build_relation.limit(value)
    end

    def pluck(*columns)
      _build_relation.pluck(*columns)
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

    def _build_relation
      assoc_class = _resolve_class(@association_def[:class_name])
      return Relation.new(assoc_class) unless assoc_class
      foreign_key = @association_def[:foreign_key]
      assoc_class.where(foreign_key.to_sym => @owner.id)
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
