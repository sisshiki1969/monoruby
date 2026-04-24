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
    EVENT_CONTROLLERAXISMOTION    = 0x650
    EVENT_CONTROLLERBUTTONDOWN    = 0x651
    EVENT_CONTROLLERBUTTONUP      = 0x652
    EVENT_CONTROLLERDEVICEADDED   = 0x653
    EVENT_CONTROLLERDEVICEREMOVED = 0x654
    EVENT_TEXTINPUT               = 0x303

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

    # --- Surface / Texture ---------------------------------------------
    SDL_FLIP_NONE       = 0x00000000
    SDL_FLIP_HORIZONTAL = 0x00000001
    SDL_FLIP_VERTICAL   = 0x00000002

    attach_function :free_surface,            :SDL_FreeSurface,
      [:pointer], :void
    attach_function :create_texture_from_surface, :SDL_CreateTextureFromSurface,
      [:pointer, :pointer], :pointer
    attach_function :destroy_texture,         :SDL_DestroyTexture,
      [:pointer], :void
    attach_function :query_texture,           :SDL_QueryTexture,
      [:pointer, :pointer, :pointer, :pointer, :pointer], :int
    attach_function :set_texture_color_mod,   :SDL_SetTextureColorMod,
      [:pointer, :uint8, :uint8, :uint8], :int
    attach_function :set_texture_alpha_mod,   :SDL_SetTextureAlphaMod,
      [:pointer, :uint8], :int
    attach_function :set_texture_blend_mode,  :SDL_SetTextureBlendMode,
      [:pointer, :int], :int
    attach_function :render_copy,             :SDL_RenderCopy,
      [:pointer, :pointer, :pointer, :pointer], :int
    attach_function :render_copy_ex,          :SDL_RenderCopyEx,
      [:pointer, :pointer, :pointer, :pointer, :double, :pointer, :int], :int

    # SDL_Surface starts with `Uint32 flags; SDL_PixelFormat *format;
    # int w; int h; int pitch;` — we only need w/h/pitch, which sit at
    # offsets 16/20/24 on x86_64.
    SURFACE_W_OFFSET     = 16
    SURFACE_H_OFFSET     = 20
    SURFACE_PITCH_OFFSET = 24

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

    # --- 2-int Point used by SDL_RenderCopyEx for the rotation center ---
    class Point < FFI::Struct
      layout :x, :int32,
             :y, :int32
    end

    # --- Float point / rect / vertex used by transformed drawing ---
    class FPoint < FFI::Struct
      layout :x, :float,
             :y, :float
    end

    class FRect < FFI::Struct
      layout :x, :float,
             :y, :float,
             :w, :float,
             :h, :float
    end

    # SDL_Vertex: { SDL_FPoint position; SDL_Color color; SDL_FPoint tex_coord; }
    # 20 bytes total (2 floats + 4 bytes + 2 floats).
    class Vertex < FFI::Struct
      layout :pos_x,  :float,
             :pos_y,  :float,
             :r,      :uint8,
             :g,      :uint8,
             :b,      :uint8,
             :a,      :uint8,
             :uv_x,   :float,
             :uv_y,   :float
    end

    attach_function :render_draw_line_f,    :SDL_RenderDrawLineF,
      [:pointer, :float, :float, :float, :float], :int
    attach_function :render_fill_rect_f,    :SDL_RenderFillRectF,
      [:pointer, :pointer], :int
    attach_function :render_copy_f,         :SDL_RenderCopyF,
      [:pointer, :pointer, :pointer, :pointer], :int
    attach_function :render_copy_ex_f,      :SDL_RenderCopyExF,
      [:pointer, :pointer, :pointer, :pointer, :double, :pointer, :int], :int
    attach_function :render_geometry,       :SDL_RenderGeometry,
      [:pointer, :pointer, :pointer, :int, :pointer, :int], :int

    # --- Game controller / joystick ------------------------------------
    # `SDL_NumJoysticks` gives the total attached joystick count;
    # `SDL_IsGameController(index)` tells whether a given slot has a
    # GameController mapping (so it can be opened via
    # `SDL_GameControllerOpen`). Buttons 0..14 map to SDL's standard
    # layout (A, B, X, Y, Back, Guide, Start, LStick, RStick,
    # LShoulder, RShoulder, DPadUp, DPadDown, DPadLeft, DPadRight);
    # axes 0..5 cover the two analog sticks and the two triggers.
    attach_function :num_joysticks,             :SDL_NumJoysticks,        [], :int
    attach_function :is_game_controller,        :SDL_IsGameController,    [:int], :int
    attach_function :game_controller_open,      :SDL_GameControllerOpen,  [:int], :pointer
    attach_function :game_controller_close,     :SDL_GameControllerClose, [:pointer], :void
    attach_function :game_controller_get_attached, :SDL_GameControllerGetAttached,
      [:pointer], :int
    attach_function :game_controller_get_button, :SDL_GameControllerGetButton,
      [:pointer, :int], :uint8
    attach_function :game_controller_get_axis,   :SDL_GameControllerGetAxis,
      [:pointer, :int], :int16
    attach_function :game_controller_get_joystick, :SDL_GameControllerGetJoystick,
      [:pointer], :pointer
    attach_function :joystick_instance_id,       :SDL_JoystickInstanceID,  [:pointer], :int32

    # --- Text input -----------------------------------------------------
    attach_function :start_text_input,  :SDL_StartTextInput,  [], :void
    attach_function :stop_text_input,   :SDL_StopTextInput,   [], :void
    attach_function :is_text_input_active, :SDL_IsTextInputActive, [], :int
  end

  # ----------------------------------------------------------------------
  # SDL2_image bindings -- loaded lazily on first `Gosu::Image.new(path)`
  # so monoruby installs without libSDL2_image present can still
  # `require "gosu"`.
  # ----------------------------------------------------------------------
  module SDL2_image
    extend FFI::Library
    ffi_lib "libSDL2_image-2.0.so.0"

    INIT_JPG  = 0x01
    INIT_PNG  = 0x02
    INIT_TIF  = 0x04
    INIT_WEBP = 0x08
    INIT_DEFAULT = INIT_JPG | INIT_PNG | INIT_TIF | INIT_WEBP

    attach_function :img_init,  :IMG_Init,    [:int], :int
    attach_function :img_quit,  :IMG_Quit,    [], :void
    attach_function :img_load,  :IMG_Load,    [:string], :pointer

    # SDL2_image 2.x routes errors through SDL_GetError (it's a #define
    # in SDL_image.h), so use the core SDL function rather than a
    # non-existent IMG_GetError symbol.
    def self.img_get_error
      Gosu::SDL2.get_error
    end
  end

  # ----------------------------------------------------------------------
  # SDL2_mixer bindings -- loaded lazily on first
  # `Gosu::Sample.new` / `Gosu::Song.new`. Audio init happens on demand
  # so windowed-only programs don't open an audio device.
  # ----------------------------------------------------------------------
  module SDL2_mixer
    extend FFI::Library
    ffi_lib "libSDL2_mixer-2.0.so.0"

    INIT_FLAC = 0x01
    INIT_MOD  = 0x02
    INIT_MP3  = 0x08
    INIT_OGG  = 0x10
    INIT_MID  = 0x20
    INIT_OPUS = 0x40

    DEFAULT_FREQUENCY = 44100
    AUDIO_S16LSB      = 0x8010
    DEFAULT_CHANNELS  = 2
    DEFAULT_CHUNKSIZE = 1024

    attach_function :mix_init,           :Mix_Init,           [:int], :int
    attach_function :mix_quit,           :Mix_Quit,           [], :void
    attach_function :mix_open_audio,     :Mix_OpenAudio,
      [:int, :uint16, :int, :int], :int
    attach_function :mix_close_audio,    :Mix_CloseAudio,     [], :void
    attach_function :mix_allocate_channels, :Mix_AllocateChannels, [:int], :int

    attach_function :mix_load_wav,       :Mix_LoadWAV,        [:string], :pointer
    attach_function :mix_free_chunk,     :Mix_FreeChunk,      [:pointer], :void
    attach_function :mix_play_channel,   :Mix_PlayChannel,
      [:int, :pointer, :int], :int
    attach_function :mix_volume,         :Mix_Volume,         [:int, :int], :int
    attach_function :mix_volume_chunk,   :Mix_VolumeChunk,    [:pointer, :int], :int
    attach_function :mix_halt_channel,   :Mix_HaltChannel,    [:int], :int
    attach_function :mix_pause,          :Mix_Pause,          [:int], :void
    attach_function :mix_resume,         :Mix_Resume,         [:int], :void
    attach_function :mix_paused,         :Mix_Paused,         [:int], :int
    attach_function :mix_playing,        :Mix_Playing,        [:int], :int
    attach_function :mix_set_panning,    :Mix_SetPanning,
      [:int, :uint8, :uint8], :int

    attach_function :mix_load_mus,       :Mix_LoadMUS,        [:string], :pointer
    attach_function :mix_free_music,     :Mix_FreeMusic,      [:pointer], :void
    attach_function :mix_play_music,     :Mix_PlayMusic,      [:pointer, :int], :int
    attach_function :mix_halt_music,     :Mix_HaltMusic,      [], :int
    attach_function :mix_pause_music,    :Mix_PauseMusic,     [], :void
    attach_function :mix_resume_music,   :Mix_ResumeMusic,    [], :void
    attach_function :mix_paused_music,   :Mix_PausedMusic,    [], :int
    attach_function :mix_playing_music,  :Mix_PlayingMusic,   [], :int
    attach_function :mix_volume_music,   :Mix_VolumeMusic,    [:int], :int

    # Errors flow through SDL_GetError (Mix_GetError is a #define).
    def self.mix_get_error
      Gosu::SDL2.get_error
    end
  end

  # ----------------------------------------------------------------------
  # SDL2_ttf bindings -- loaded lazily on first `Gosu::Font.new` /
  # `Gosu::Image.from_text`. `TTF_Init` is called once per process and
  # cleaned up in `at_exit`.
  # ----------------------------------------------------------------------
  module SDL2_ttf
    extend FFI::Library
    ffi_lib "libSDL2_ttf-2.0.so.0"

    # SDL_Color in big-endian order: r, g, b, a.
    class Color < FFI::Struct
      layout :r, :uint8,
             :g, :uint8,
             :b, :uint8,
             :a, :uint8
    end

    attach_function :ttf_init,              :TTF_Init,              [], :int
    attach_function :ttf_quit,              :TTF_Quit,              [], :void
    attach_function :ttf_open_font,         :TTF_OpenFont,          [:string, :int], :pointer
    attach_function :ttf_close_font,        :TTF_CloseFont,         [:pointer], :void
    attach_function :ttf_render_utf8_blended, :TTF_RenderUTF8_Blended,
      [:pointer, :string, :uint32], :pointer
    # NOTE: SDL_Color is a 4-byte struct that libTTF expects by value.
    # FFI's `:uint32` ABI-matches the little-endian byte layout
    # `r | (g<<8) | (b<<16) | (a<<24)`.
    attach_function :ttf_size_utf8,         :TTF_SizeUTF8,
      [:pointer, :string, :pointer, :pointer], :int
    attach_function :ttf_font_height,       :TTF_FontHeight,        [:pointer], :int
    attach_function :ttf_font_ascent,       :TTF_FontAscent,        [:pointer], :int

    def self.ttf_get_error
      Gosu::SDL2.get_error
    end
  end
end
