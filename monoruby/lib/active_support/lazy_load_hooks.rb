# frozen_string_literal: true
#
# ActiveSupport::LazyLoadHooks for monoruby.
# Provides on_load / run_load_hooks for deferred execution.
#

module ActiveSupport
  @_lazy_load_hooks = {}
  @_load_hooks_run = {}

  def self.on_load(name, options = {}, &block)
    @_lazy_load_hooks ||= {}
    @_load_hooks_run ||= {}

    @_lazy_load_hooks[name] ||= []

    if @_load_hooks_run[name]
      # Hook already ran, execute immediately
      @_load_hooks_run[name].each do |base|
        _execute_hook(name, base, options, block)
      end
    end

    @_lazy_load_hooks[name] << [block, options]
  end

  def self.run_load_hooks(name, base = Object)
    @_lazy_load_hooks ||= {}
    @_load_hooks_run ||= {}

    @_load_hooks_run[name] ||= []
    @_load_hooks_run[name] << base

    if @_lazy_load_hooks[name]
      @_lazy_load_hooks[name].each do |hook_block, options|
        _execute_hook(name, base, options, hook_block)
      end
    end
  end

  def self._execute_hook(name, base, options, block)
    if options[:yield]
      block.call(base)
    else
      if base.is_a?(Module)
        base.class_eval(&block)
      else
        block.call(base)
      end
    end
  rescue => e
    # Silently ignore errors in hooks during load
  end

  private_class_method :_execute_hook
end
