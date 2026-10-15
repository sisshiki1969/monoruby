require "capi_st"

p CapiSt.numtable
p CapiSt.strtable
p CapiSt.custom
p CapiSt.hashes("")
p CapiSt.hashes("a")
p CapiSt.hashes("hello, world")
p CapiSt.hashes("x" * 1000)
p CapiSt.hashes("\x00\xff\x7f".b)

# VALUE keys in an identity table, kept alive by the mark function.
t = CapiSt::Table.new
keys = (1..200).map { |i| "k#{i}" }
keys.each_with_index { |k, i| t[k] = "v#{i}" * 3 }
t[:sym] = [1, 2, 3]
t[42] = nil
GC.start
p t.size
p t[keys[0]], t[keys[199]], t[:sym], t[42], t["k1"]
p t.delete(keys[5]), t.delete(keys[5]), t.size
seen = []
t.each { |k, v| seen << k }
p seen.first(3), seen.last(2), seen.size
t = nil
GC.start
p CapiSt.tables_freed >= 0
