class Foo
  def to_proc
    Proc.new {|v| p v}
  end
end

[1,2,3].each(&Foo.new)