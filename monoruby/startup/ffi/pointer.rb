module FFI

  # typedef struct Pointer {
  #     AbstractMemory memory;
  #     VALUE rbParent;
  #     char* storage; /* start of malloc area */
  #     bool autorelease;
  #     bool allocated;
  # } Pointer;

  class Pointer < AbstractMemory
    def initialize(type, address = type)
      @type = type
      @address = address
    end
  end
end
