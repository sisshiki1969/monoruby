def tarai(x,y,z)
  if x > y
    tarai(tarai(x-1, y, z), tarai(y-1, z, x), tarai(z-1, x, y))
  else
    y
  end
end

puts tarai(14, 7, 0)