require "capi_roots"

class CapiRoots
  # Collect, then allocate enough to reuse every freed cell.
  def churn
    2.times do
      GC.start
      i = 0
      while i < 100_000
        "z" * 40
        i += 1
      end
    end
    nil
  end
end

# Each value is built in a method of its own, so that no frame of the
# caller still holds it.
def fresh(tag) = "#{tag}-" + "x" * 31
def ary1 = [fresh("entry")]
def ary3 = [fresh("first"), fresh("second"), fresh("third")]
def set_gv = ($capi_roots = fresh("gvar")) && nil

o = CapiRoots.new
p o.entry(ary1)
p o.cptr(ary3)
set_gv
p o.gv

def backref(o)
  fresh("matched") =~ /(m\w+)-(x+)/
  o.backref
end
p backref(o)

def lastline(o)
  $_ = fresh("line")
  o.lastline
end
p lastline(o)

p o.protect
o.churn
p o.errinfo
