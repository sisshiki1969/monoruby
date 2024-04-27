FRAMES = 2999

list = []
for i in 0..FRAMES
  list[i] = [i]
end

system("cargo install --path monoruby")

TEMPLATE = "../optcarrot/bin/optcarrot -b --print-fps-history -f 3000 ../optcarrot/examples/Lan_Master.nes"

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
end

monoruby_version = `monoruby -v`.chomp
read('monoruby', list)
puts "monoruby finished"

system("rbenv local 3.3.0")
ruby_version = `ruby -v`.chomp
read('ruby --yjit', list)
puts "ruby finished"

system("rbenv local truffleruby-24.0.0")
truffle_version = `ruby -v`.chomp
read('ruby', list)
puts "truffleruby finished"

system("rbenv local 3.3.0")

f = "frame,'#{monoruby_version}','#{ruby_version}','#{truffle_version}'\n"
for line in list
  frame, monoruby, ruby, truffle = line
  f << "#{frame},#{monoruby},#{ruby},#{truffle}\n"
end

File.write("result.csv", f)

