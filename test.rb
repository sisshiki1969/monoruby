class Foo
  def initialize data
    @key = data
  end
  private
  def do_fuga
    p 'secret'
  end
end

some = Foo.new 'XXX'
some.instance_eval{p @key} #=> "XXX"
some.instance_eval{do_fuga } #=> "secret" # private メソッドも呼び出せる

#some.instance_eval 'raise 100' # ..:10: (eval):1:  (RuntimeError)
#messg = 'unknown'
#some.instance_eval 'raise messg','file.rb' # file.rb:999: unknown (RuntimeError)