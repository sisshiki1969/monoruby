      str0 = "bar"
      res = (str0[2,1])
      for __i in 0..7 do
          res2 = (str0[2,1])
          __assert(res, res2)
      end
      res