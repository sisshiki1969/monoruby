# Required by cases.rb: frames in a second file.
def lib_call(x) = x.lib_undefined
def lib_arity(a, b) = a
def lib_loc = caller_locations(0).first
