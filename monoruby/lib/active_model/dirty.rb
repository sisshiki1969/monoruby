# frozen_string_literal: true
#
# ActiveModel::Dirty - Track attribute changes in models.
#
# Provides methods like changed?, changes, *_changed?, *_was, etc.

module ActiveModel
  module Dirty
    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_dirty_attributes, [])
    end

    module ClassMethods
      def inherited(subclass)
        super
        parent_attrs = instance_variable_get(:@_dirty_attributes) || []
        subclass.instance_variable_set(:@_dirty_attributes, parent_attrs.dup)
      end

      def _dirty_attributes
        @_dirty_attributes ||= []
      end

      # Define which attributes are tracked for changes.
      # If using ActiveModel::Attributes, this is called automatically.
      # Otherwise call: define_attribute_methods :name, :age
      def define_attribute_methods(*attrs)
        attrs.flatten.each do |attr|
          attr = attr.to_sym
          _dirty_attributes << attr unless _dirty_attributes.include?(attr)

          # Define *_changed?
          define_method(:"#{attr}_changed?") do
            _changes.key?(attr)
          end

          # Define *_was
          define_method(:"#{attr}_was") do
            if _changes.key?(attr)
              _changes[attr][0]
            else
              send(attr)
            end
          end

          # Define *_change
          define_method(:"#{attr}_change") do
            _changes[attr]
          end

          # Define *_previously_changed?
          define_method(:"#{attr}_previously_changed?") do
            _previous_changes.key?(attr)
          end

          # Define *_previous_change
          define_method(:"#{attr}_previous_change") do
            _previous_changes[attr]
          end

          # Define *_previously_was
          define_method(:"#{attr}_previously_was") do
            if _previous_changes.key?(attr)
              _previous_changes[attr][0]
            else
              nil
            end
          end

          # Define *_will_change! to manually mark as changed
          define_method(:"#{attr}_will_change!") do
            _attribute_will_change!(attr)
          end

          # Define restore_*!
          define_method(:"restore_#{attr}!") do
            if _changes.key?(attr)
              # Use the writer to set back to original value
              send(:"#{attr}=", _changes[attr][0]) if respond_to?(:"#{attr}=")
              _changes.delete(attr)
            end
          end
        end
      end
    end

    # --- Instance methods ---

    def changed?
      !_changes.empty?
    end

    def changed
      _changes.keys.map(&:to_s)
    end

    def changes
      result = {}
      _changes.each { |k, v| result[k.to_s] = v }
      result
    end

    def changed_attributes
      result = {}
      _changes.each { |k, v| result[k.to_s] = v[0] }
      result
    end

    def previous_changes
      result = {}
      _previous_changes.each { |k, v| result[k.to_s] = v }
      result
    end

    def previously_changed?
      !_previous_changes.empty?
    end

    def changes_applied
      @_previous_changes = _changes.dup
      @_changes = {}
    end

    def clear_changes_information
      @_changes = {}
      @_previous_changes = {}
    end

    def restore_attributes(attrs = nil)
      attrs = attrs ? attrs.map(&:to_sym) : _changes.keys
      attrs.each do |attr|
        method_name = :"restore_#{attr}!"
        send(method_name) if respond_to?(method_name)
      end
    end

    private

    def _changes
      @_changes ||= {}
    end

    def _previous_changes
      @_previous_changes ||= {}
    end

    # Record that an attribute is about to change
    def _attribute_will_change!(attr)
      attr = attr.to_sym
      return if _changes.key?(attr)
      value = respond_to?(attr) ? send(attr) : nil
      _set_attribute_was(attr, value)
    end

    def _set_attribute_was(attr, old_value)
      _changes[attr] = [old_value, nil] unless _changes.key?(attr)
    end

    # Call this from attribute setters to track changes.
    # old_value: the value before the change
    # new_value: the value after the change
    def _track_attribute_change(attr, old_value, new_value)
      attr = attr.to_sym
      if _changes.key?(attr)
        original = _changes[attr][0]
        if original == new_value
          _changes.delete(attr)
        else
          _changes[attr] = [original, new_value]
        end
      else
        unless old_value == new_value
          _changes[attr] = [old_value, new_value]
        end
      end
    end
  end
end
