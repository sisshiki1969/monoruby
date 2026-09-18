# The sliver of OpenGL `Gosu.gl` needs for itself.
#
# The block a caller passes to `Gosu.gl` talks to OpenGL directly --
# through the `opengl` / `opengl-bindings` gem, or anything else that can
# reach the current context -- and it draws into the very context SDL's
# `opengl` render driver uses for `Gosu::Image#draw` and friends. Whatever
# the block enables, binds or multiplies onto a matrix would therefore
# still be in force when SDL draws the next 2D frame, and SDL's renderer
# caches what it believes the GL state to be. So the state is saved before
# the block and restored after it, exactly as upstream Gosu's
# `Graphics::begin_gl` / `end_gl` do.
#
# Only those save/restore entry points are bound here: this is not a GL
# binding for general use, and callers are expected to bring their own.
# The library is opened lazily and its absence is not fatal -- a host
# without libGL has no GL context for the block to draw into either, and
# the block itself will say so far more precisely than we could.
require "ffi"

module Gosu
  module GL
    extend FFI::Library

    ALL_ATTRIB_BITS        = 0x000F_FFFF
    CLIENT_ALL_ATTRIB_BITS = 0xFFFF_FFFF

    # glMatrixMode
    MODELVIEW  = 0x1700
    PROJECTION = 0x1701
    TEXTURE    = 0x1702

    # glBindBuffer targets
    ARRAY_BUFFER         = 0x8892
    ELEMENT_ARRAY_BUFFER = 0x8893
    PIXEL_PACK_BUFFER    = 0x88EB
    PIXEL_UNPACK_BUFFER  = 0x88EC

    # glPixelStorei parameters, paired with the value a context starts
    # out with: what a block reading or writing pixels is entitled to
    # assume, and what SDL does not leave behind (see `reset_transfer`).
    PIXEL_STORE_DEFAULTS = {
      0x0CF0 => 0, # GL_UNPACK_SWAP_BYTES
      0x0CF1 => 0, # GL_UNPACK_LSB_FIRST
      0x0CF2 => 0, # GL_UNPACK_ROW_LENGTH
      0x0CF3 => 0, # GL_UNPACK_SKIP_ROWS
      0x0CF4 => 0, # GL_UNPACK_SKIP_PIXELS
      0x0CF5 => 4, # GL_UNPACK_ALIGNMENT
      0x806D => 0, # GL_UNPACK_SKIP_IMAGES
      0x806E => 0, # GL_UNPACK_IMAGE_HEIGHT
      0x0D00 => 0, # GL_PACK_SWAP_BYTES
      0x0D01 => 0, # GL_PACK_LSB_FIRST
      0x0D02 => 0, # GL_PACK_ROW_LENGTH
      0x0D03 => 0, # GL_PACK_SKIP_ROWS
      0x0D04 => 0, # GL_PACK_SKIP_PIXELS
      0x0D05 => 4, # GL_PACK_ALIGNMENT
      0x806B => 0, # GL_PACK_SKIP_IMAGES
      0x806C => 0, # GL_PACK_IMAGE_HEIGHT
    }.freeze

    class << self
      # True once the entry points below are callable. Tried once; a
      # failure is remembered, not retried per frame.
      def available?
        return @available if defined?(@available)

        @available = _attach
      end

      # Runs `block` with the drawing state saved, and puts it back
      # afterwards. The attribute stacks cover the enable bits, blending,
      # colour, viewport, scissor and texture bindings; the matrix stacks
      # are per mode and have to be pushed one mode at a time.
      def bracket
        return yield unless available?

        push_attrib(ALL_ATTRIB_BITS)
        push_client_attrib(CLIENT_ALL_ATTRIB_BITS)
        matrix_mode(TEXTURE)
        push_matrix
        matrix_mode(PROJECTION)
        push_matrix
        matrix_mode(MODELVIEW)
        push_matrix
        reset_transfer
        begin
          yield
        ensure
          matrix_mode(MODELVIEW)
          pop_matrix
          matrix_mode(PROJECTION)
          pop_matrix
          matrix_mode(TEXTURE)
          pop_matrix
          # SDL's GL renderer feeds glVertexPointer from plain client
          # memory, which a buffer left bound to GL_ARRAY_BUFFER would
          # turn into an offset into that buffer. Whether the bindings
          # are part of the client attribute stack is not something every
          # implementation agrees on, so clear them by hand first.
          unbind_buffers
          pop_client_attrib
          pop_attrib
        end
      end

      private

      # Puts pixel transfers back to what a fresh context does, because
      # SDL's renderer does not: uploading a texture leaves
      # GL_UNPACK_ROW_LENGTH set to *that* texture's width and
      # GL_UNPACK_ALIGNMENT set to 1. A block that then uploads a
      # narrower image of its own reads that many pixels per row out of a
      # buffer sized for its own width, runs off the end of it, and takes
      # the process down inside the driver's memcpy.
      #
      # The values are saved by the client attribute stack (they are what
      # GL_CLIENT_PIXEL_STORE_BIT covers), so SDL gets its own back when
      # the block is over.
      def reset_transfer
        PIXEL_STORE_DEFAULTS.each { |pname, value| pixel_store_i(pname, value) }
        return unless @bind_buffer

        # Same story for the pixel buffers: bound, they would turn the
        # block's pixel pointers into offsets into them.
        bind_buffer(PIXEL_PACK_BUFFER, 0)
        bind_buffer(PIXEL_UNPACK_BUFFER, 0)
      end

      def unbind_buffers
        return unless @bind_buffer

        bind_buffer(ARRAY_BUFFER, 0)
        bind_buffer(ELEMENT_ARRAY_BUFFER, 0)
      end

      def _attach
        # Candidates in FFI.map_library_name order: the plain name covers
        # Linux/BSD, the soname covers runtime-only installs, and the
        # framework covers macOS, where OpenGL lives outside the library
        # search path.
        ffi_lib ["GL", "libGL.so.1",
                 "/System/Library/Frameworks/OpenGL.framework/OpenGL"]
        attach_function :push_attrib,        :glPushAttrib,       [:uint32], :void
        attach_function :pop_attrib,         :glPopAttrib,        [], :void
        attach_function :push_client_attrib, :glPushClientAttrib, [:uint32], :void
        attach_function :pop_client_attrib,  :glPopClientAttrib,  [], :void
        attach_function :matrix_mode,        :glMatrixMode,       [:uint32], :void
        attach_function :push_matrix,        :glPushMatrix,       [], :void
        attach_function :pop_matrix,         :glPopMatrix,        [], :void
        attach_function :pixel_store_i,      :glPixelStorei,      [:uint32, :int32], :void
        # GL 1.5; absent from a GL 1.4 library, where nothing can have
        # bound a buffer either.
        begin
          attach_function :bind_buffer, :glBindBuffer, [:uint32, :uint32], :void
          @bind_buffer = true
        rescue LoadError
          @bind_buffer = false
        end
        true
      rescue LoadError, RuntimeError
        false
      end
    end
  end
end
