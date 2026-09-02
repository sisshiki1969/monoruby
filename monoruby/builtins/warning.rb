module Warning
  # `Warning.[]`, `Warning.[]=` and `Warning.categories` are builtins
  # (src/builtins/warning.rs): the category switches live in the Rust
  # runtime so its own gated warnings read a flag rather than calling
  # back into Ruby.

  # The default `Warning.warn`. A categorised warning is suppressed when
  # its category is switched off (`Warning[:deprecated] = false`), which
  # is what `rb_warning_s_warn` checks before writing anything; an
  # uncategorised one always prints.
  def warn(msg, category: nil)
    unless category.nil?
      unless category.is_a?(Symbol)
        raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
      end
      unless Warning.categories.include?(category)
        raise ArgumentError, "unknown category: #{category}"
      end
      return nil unless Warning[category]
    end
    $stderr.write(msg)
    nil
  end
  extend self
end
