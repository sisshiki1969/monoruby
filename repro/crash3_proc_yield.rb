obj = Object.new
def obj.create
  Proc.new do |&b|
    yield
  end
end
a_proc = obj.create { 7 }
p a_proc.call { 3 }
