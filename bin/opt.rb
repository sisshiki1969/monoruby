FRAMES = 2999
arg = ARGV.join(" ")

list = []
for i in 0..FRAMES
  list[i] = [i]
end

#system("cargo install --path monoruby")

TEMPLATE = if arg.length == 0
  "../optcarrot/bin/optcarrot -b --print-fps-history -f 3000 ../optcarrot/examples/Lan_Master.nes"
else
  "../optcarrot/bin/optcarrot -b --print-fps-history -f 3000 " + arg + " ../optcarrot/examples/Lan_Master.nes"
end

puts TEMPLATE

def read(command, list)
    s = `#{command} #{TEMPLATE}`.lines
    s.shift
    s.each{|line|
      c1, c2 = line.split(',')
      frame = c1.to_i
      fps = c2.to_f
      list[frame] << fps
      if frame >= FRAMES
        break
      end
    }
    puts s[FRAMES + 1].chomp
    puts s[FRAMES + 2].chomp
end

puts monoruby_version = `monoruby -v`.chomp
read('monoruby', list)
puts

system("rbenv local 3.4-dev")
puts ruby_version = `ruby -v`.chomp
read('ruby --yjit', list)
puts

system("rbenv local truffleruby+graalvm-24.0.0")
puts truffle_version = `ruby -v`.chomp
read('ruby', list)
puts

system("rbenv local 3.3.0")

f = "frame,\"#{monoruby_version}\",\"#{ruby_version}\",\"#{truffle_version}\"\n"
for line in list
  frame, monoruby, ruby, truffle = line
  f << "#{frame},#{monoruby},#{ruby},#{truffle}\n"
end

File.write("result.csv", f)

