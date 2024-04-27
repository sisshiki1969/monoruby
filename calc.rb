FRAMES = 2999

list = []
for i in 0..FRAMES
  list[i] = [i]
end

def read(file, list)
  File.open(file){|f|
    f.readline
    f.each_line{|line|
      c1, c2 = line.split(',')
      frame = c1.to_i
      fps = c2.to_f
      list[frame] << fps
      if frame >= FRAMES
        break
      end
    }
  }
end

read("monoruby.csv", list)
read("ruby.csv", list)
read("truffle.csv", list)

File.open("result.csv", "w"){|f|
  f.puts "frame,monoruby,ruby,truffle"
  for line in list
    frame, monoruby, ruby, truffle = line
    f.puts "#{frame},#{monoruby},#{ruby},#{truffle}"
  end
}

