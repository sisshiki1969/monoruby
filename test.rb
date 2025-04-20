SEPARATOR_PAT = /#{Regexp.quote File::SEPARATOR}/

def chop_basename(path) # :nodoc:
  base = File.basename(path)
  if /\A#{SEPARATOR_PAT}?\z/o.match?(base)
    puts "#{base} matched! '/'"
    return nil
  else
    return path[0, path.rindex(base)], base
  end
end

prefix2 = ".."
while r2 = chop_basename(prefix2)
  puts "#{r2}"
  gets
  prefix2, basename2 = r2
end