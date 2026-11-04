# Driven on CRuby and on monoruby by tests/capi.rs; the outputs must match.
require "capi_vars"
V = CapiVars

def t = (yield; :ok) rescue [$!.class, $!.message]

# rb_define_variable: one storage, read and written from both sides
p $v_plain, V.plain
$v_plain = [1, 2]
p V.plain
V.plain = :from_c
p $v_plain
p defined?($v_plain), global_variables.include?(:$v_plain)
$v_plain = "kept across a collection"
GC.start
p $v_plain, V.plain

# rb_define_readonly_variable: C changes it, Ruby cannot
p $v_fixed, defined?($v_fixed)
p t { $v_fixed = 1 }
V.set_fixed("changed")
p $v_fixed

# rb_define_virtual_variable: the getter answers every read
p $v_counter, $v_counter
$v_counter = 10
p $v_counter, V.counter
p defined?($v_counter)
p t { $v_readonly_virtual = 1 }
p $v_readonly_virtual

# rb_define_hooked_variable: the hooks wrap the storage
p $v_hooked
$v_hooked = 42
p $v_hooked, V.hooked_store
p t { $v_hooked = nil }
p $v_hooked

# aliases and trace_var reach the hooks
alias $v_alias $v_plain
$v_alias = "via alias"
p $v_plain, V.plain
trace_var(:$v_plain) { |v| puts "traced #{v.inspect}" }
$v_plain = 7
untrace_var(:$v_plain)
p $v_plain

# rb_gv_get / rb_gv_set go through the hooks too
p V.gv_get("$v_hooked"), V.gv_get("v_counter")
V.gv_set("$v_plain", "gv")
p $v_plain
p t { V.gv_set("$v_fixed", "gv") }

# instance_eval / instance_exec / module_eval / module_exec from C
class Box
  def initialize = @secret = 21
end
b = Box.new
p V.ieval_str(b, "@secret * 2")
p V.ieval_block(b) { @secret + 1 }
p V.iexec(b, 3, 4) { |x, y| [@secret, x, y, self.class] }
p V.meval_str(Box, "def peek = @secret")
p b.peek
p V.mexec(Box, :zzz) { |n| define_method(n) { n } }
p b.zzz

# what the alloc function set is on the instance `new` returns
tg = V::Tagged.new(5)
p tg.x, tg.tag, tg.instance_variables
class Sub < V::Tagged
  def initialize(x)
    super
    @mine = x * 2
  end
end
s = Sub.new(6)
p s.x, s.tag, s.instance_variables, s.instance_variable_get(:@mine)
