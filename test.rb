class F
  def hello
    "Bonjour"
  end
end

class D
  private
  def hello
    "Guten Tag"
  end
end
list = [F.new, D.new]
res = []

list.each{|it| res << it.hello if it.respond_to?(:hello)}
list.each{|it| it.instance_eval("res << hello if it.respond_to?(:hello, true)")}

puts res.inspect
