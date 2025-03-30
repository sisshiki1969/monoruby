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

100.times do
  list.each{|it| puts it.hello if it.respond_to?(:hello)}
  list.each{|it| it.instance_eval("puts hello if it.respond_to?(:hello, true)")}
end