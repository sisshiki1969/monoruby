#!/usr/bin/env ruby

FRAMES = 2999
arg = ARGV.join(" ")

list = []
list[0] = ["frame"]
for i in 0..FRAMES
  list[i + 1] = [i]
end

system("cargo install --path monoruby")

TEMPLATE = if arg.length == 0
  "../optcarrot/bin/optcarrot -b --print-fps-history -f 3000 ../optcarrot/examples/Lan_Master.nes"
else
  "../optcarrot/bin/optcarrot -b --print-fps-history -f 3000 " + arg + " ../optcarrot/examples/Lan_Master.nes"
end

puts TEMPLATE

def read(ruby, version, command, list)
    pre = if ruby.nil?
      ''
    else 
      "eval \"$(rbenv init -)\"; rbenv shell #{ruby}; "
    end
    s = `#{pre}#{version}; #{command} #{TEMPLATE}`.lines
    name = s.shift.chomp.delete(",")
    list[0] << name
    s.shift
    s.each{|line|
      c1, c2 = line.split(',')
      frame = c1.to_i
      fps = c2.to_f
      list[frame + 1] << fps
      if frame >= FRAMES
        break
      end
    }
    puts name
    puts s[FRAMES + 1].chomp
    puts s[FRAMES + 2].chomp
end

read(nil, "monoruby -v", 'monoruby', list)
puts

read(nil, "monoruby -v", 'monoruby --no-jit', list)
puts

read("3.4.2", "ruby --jit -v", 'ruby --jit', list)
puts

read(nil, "ruby -v", 'ruby', list)
puts

read("truffleruby+graalvm-24.2.0", "ruby -v", 'ruby', list)
puts

read("truffleruby-24.2.0", "ruby -v", 'ruby', list)
puts

f = list.map do |line|
  line.join(",") + "\n"
end.join

File.write("result.csv", f)
