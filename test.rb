puts __FILE__

class C
  autoload :D, File.expand_path("./monoruby/a.rb")
end
puts C::D