# SDL2 FFI bindings used by Gosu::Window / Gosu.draw_* to drive a real
# window + event loop under monoruby. Only the subset of libSDL2-2.0
# actually needed is bound; we avoid pulling in SDL2_image / SDL2_ttf
# / SDL2_mixer until image / font / audio support is added.
#
# The bindings are intentionally Ruby-style (snake_case) because
# monoruby's constant vs method lookup rules clash with uppercase
# `SDL_*` names; module callers do `Gosu::SDL2.init(...)` rather than
# `SDL2::SDL_Init(...)`.

require "ffi"

module Gosu
  module SDL2
    extend FFI::Library
    ffi_lib "libSDL2-2.0.so.0"

    # --- SDL_Init flags -------------------------------------------------
    INIT_TIMER          = 0x00000001
    INIT_AUDIO          = 0x00000010
    INIT_VIDEO          = 0x00000020
    INIT_JOYSTICK       = 0x00000200
    INIT_HAPTIC         = 0x00001000
    INIT_GAMECONTROLLER = 0x00002000
    INIT_EVENTS         = 0x00004000
    INIT_EVERYTHING     = INIT_TIMER | INIT_AUDIO | INIT_VIDEO |
                          INIT_EVENTS | INIT_JOYSTICK | INIT_HAPTIC |
                          INIT_GAMECONTROLLER

    # --- Window flags ---------------------------------------------------
    WINDOW_FULLSCREEN         = 0x00000001
    WINDOW_OPENGL             = 0x00000002
    WINDOW_SHOWN              = 0x00000004
    WINDOW_HIDDEN             = 0x00000008
    WINDOW_BORDERLESS         = 0x00000010
    WINDOW_RESIZABLE          = 0x00000020
    WINDOW_FULLSCREEN_DESKTOP = 0x00001001
    WINDOWPOS_CENTERED        = 0x2FFF0000

    # --- Renderer flags -------------------------------------------------
    RENDERER_SOFTWARE     = 0x00000001
    RENDERER_ACCELERATED  = 0x00000002
    RENDERER_PRESENTVSYNC = 0x00000004
    RENDERER_TARGETTEXTURE = 0x00000008

    # --- Event types ----------------------------------------------------
    EVENT_QUIT          = 0x100
    EVENT_WINDOW        = 0x200
    EVENT_KEYDOWN       = 0x300
    EVENT_KEYUP         = 0x301
    EVENT_MOUSEMOTION   = 0x400
    EVENT_MOUSEBUTTONDOWN = 0x401
    EVENT_MOUSEBUTTONUP   = 0x402
    EVENT_MOUSEWHEEL      = 0x403
    EVENT_CONTROLLERBUTTONDOWN = 0x650
    EVENT_CONTROLLERBUTTONUP   = 0x651
    EVENT_TEXTINPUT = 0x303

    # --- Core -----------------------------------------------------------
    attach_function :init,             :SDL_Init,             [:uint32], :int
    attach_function :init_sub_system,  :SDL_InitSubSystem,    [:uint32], :int
    attach_function :quit,             :SDL_Quit,             [], :void
    attach_function :get_error,        :SDL_GetError,         [], :string
    attach_function :get_ticks,        :SDL_GetTicks,         [], :uint32
    attach_function :delay,            :SDL_Delay,            [:uint32], :void

    # --- Window / Renderer ---------------------------------------------
    attach_function :create_window,       :SDL_CreateWindow,
      [:string, :int, :int, :int, :int, :uint32], :pointer
    attach_function :destroy_window,      :SDL_DestroyWindow,    [:pointer], :void
    attach_function :set_window_title,    :SDL_SetWindowTitle,   [:pointer, :string], :void
    attach_function :set_window_size,     :SDL_SetWindowSize,    [:pointer, :int, :int], :void
    attach_function :create_renderer,     :SDL_CreateRenderer,
      [:pointer, :int, :uint32], :pointer
    attach_function :destroy_renderer,    :SDL_DestroyRenderer,  [:pointer], :void
    attach_function :set_draw_color,      :SDL_SetRenderDrawColor,
      [:pointer, :uint8, :uint8, :uint8, :uint8], :int
    attach_function :set_draw_blend_mode, :SDL_SetRenderDrawBlendMode,
      [:pointer, :int], :int
    attach_function :render_clear,        :SDL_RenderClear,      [:pointer], :int
    attach_function :render_present,      :SDL_RenderPresent,    [:pointer], :void
    attach_function :render_fill_rect,    :SDL_RenderFillRect,   [:pointer, :pointer], :int
    attach_function :render_draw_line,    :SDL_RenderDrawLine,
      [:pointer, :int, :int, :int, :int], :int
    attach_function :render_draw_point,   :SDL_RenderDrawPoint,  [:pointer, :int, :int], :int
    attach_function :render_set_clip_rect,:SDL_RenderSetClipRect,[:pointer, :pointer], :int

    # --- Event pump -----------------------------------------------------
    # SDL_Event is a 56-byte union on x86_64. We stash it in a
    # MemoryPointer and pull fields out by byte offset (see
    # Gosu::Window#_pump_events below) rather than mapping the full
    # union, which keeps this binding small and avoids FFI::Union
    # quirks.
    EVENT_SIZE = 56
    attach_function :poll_event,  :SDL_PollEvent,  [:pointer], :int
    attach_function :wait_event,  :SDL_WaitEvent,  [:pointer], :int
    attach_function :get_mouse_state, :SDL_GetMouseState, [:pointer, :pointer], :uint32
    attach_function :get_keyboard_state, :SDL_GetKeyboardState, [:pointer], :pointer

    # --- Small helper: a 4-int Rect we reuse for draw_rect / clip_to ---
    class Rect < FFI::Struct
      layout :x, :int32,
             :y, :int32,
             :w, :int32,
             :h, :int32
    end
  end
end
