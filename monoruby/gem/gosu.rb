# gosu.rb -- monoruby replacement for the `gosu` gem's C extension
# (gosu.so).
#
# Provides enough Gosu API surface that
#
#   require "gosu"
#
# succeeds and a typical `Gosu::Window` subclass can be instantiated
# without error. Rendering, audio, and input are stubbed out:
# callbacks such as `Gosu::Window#show` return immediately without
# running a real frame loop, and primitives such as `Gosu::Image.new`
# accept any path but never produce pixels. Real gameplay / drawing
# is not supported -- this is a compatibility shim so that Ruby code
# using Gosu at least loads, parses, and fails gracefully instead of
# raising `LoadError` on `require`.
#
# The pure-Ruby parts of the gem (swig_patches.rb, patches.rb,
# compat.rb) are shipped here under `monoruby/gem/gosu/` and required
# at the bottom of this file; they add the CamelCase button aliases
# (`KbLeft`, `Gp0Button0`, ...), deprecation helpers, and `Numeric`
# angle-conversion extensions, matching the real gem's public API.

require "gosu/sdl2"

module Gosu
  GP_0_BUTTON_0 = 293
  GP_0_BUTTON_1 = 294
  GP_0_BUTTON_10 = 303
  GP_0_BUTTON_11 = 304
  GP_0_BUTTON_12 = 305
  GP_0_BUTTON_13 = 306
  GP_0_BUTTON_14 = 307
  GP_0_BUTTON_15 = 308
  GP_0_BUTTON_2 = 295
  GP_0_BUTTON_3 = 296
  GP_0_BUTTON_4 = 297
  GP_0_BUTTON_5 = 298
  GP_0_BUTTON_6 = 299
  GP_0_BUTTON_7 = 300
  GP_0_BUTTON_8 = 301
  GP_0_BUTTON_9 = 302
  GP_0_DOWN = 376
  GP_0_DPAD_DOWN = 292
  GP_0_DPAD_LEFT = 289
  GP_0_DPAD_RIGHT = 290
  GP_0_DPAD_UP = 291
  GP_0_LEFT = 373
  GP_0_LEFT_STICK_X_AXIS = 6
  GP_0_LEFT_STICK_Y_AXIS = 7
  GP_0_LEFT_TRIGGER_AXIS = 10
  GP_0_RIGHT = 374
  GP_0_RIGHT_STICK_X_AXIS = 8
  GP_0_RIGHT_STICK_Y_AXIS = 9
  GP_0_RIGHT_TRIGGER_AXIS = 11
  GP_0_UP = 375
  GP_1_BUTTON_0 = 313
  GP_1_BUTTON_1 = 314
  GP_1_BUTTON_10 = 323
  GP_1_BUTTON_11 = 324
  GP_1_BUTTON_12 = 325
  GP_1_BUTTON_13 = 326
  GP_1_BUTTON_14 = 327
  GP_1_BUTTON_15 = 328
  GP_1_BUTTON_2 = 315
  GP_1_BUTTON_3 = 316
  GP_1_BUTTON_4 = 317
  GP_1_BUTTON_5 = 318
  GP_1_BUTTON_6 = 319
  GP_1_BUTTON_7 = 320
  GP_1_BUTTON_8 = 321
  GP_1_BUTTON_9 = 322
  GP_1_DOWN = 380
  GP_1_DPAD_DOWN = 312
  GP_1_DPAD_LEFT = 309
  GP_1_DPAD_RIGHT = 310
  GP_1_DPAD_UP = 311
  GP_1_LEFT = 377
  GP_1_LEFT_STICK_X_AXIS = 12
  GP_1_LEFT_STICK_Y_AXIS = 13
  GP_1_LEFT_TRIGGER_AXIS = 16
  GP_1_RIGHT = 378
  GP_1_RIGHT_STICK_X_AXIS = 14
  GP_1_RIGHT_STICK_Y_AXIS = 15
  GP_1_RIGHT_TRIGGER_AXIS = 17
  GP_1_UP = 379
  GP_2_BUTTON_0 = 333
  GP_2_BUTTON_1 = 334
  GP_2_BUTTON_10 = 343
  GP_2_BUTTON_11 = 344
  GP_2_BUTTON_12 = 345
  GP_2_BUTTON_13 = 346
  GP_2_BUTTON_14 = 347
  GP_2_BUTTON_15 = 348
  GP_2_BUTTON_2 = 335
  GP_2_BUTTON_3 = 336
  GP_2_BUTTON_4 = 337
  GP_2_BUTTON_5 = 338
  GP_2_BUTTON_6 = 339
  GP_2_BUTTON_7 = 340
  GP_2_BUTTON_8 = 341
  GP_2_BUTTON_9 = 342
  GP_2_DOWN = 384
  GP_2_DPAD_DOWN = 332
  GP_2_DPAD_LEFT = 329
  GP_2_DPAD_RIGHT = 330
  GP_2_DPAD_UP = 331
  GP_2_LEFT = 381
  GP_2_LEFT_STICK_X_AXIS = 18
  GP_2_LEFT_STICK_Y_AXIS = 19
  GP_2_LEFT_TRIGGER_AXIS = 22
  GP_2_RIGHT = 382
  GP_2_RIGHT_STICK_X_AXIS = 20
  GP_2_RIGHT_STICK_Y_AXIS = 21
  GP_2_RIGHT_TRIGGER_AXIS = 23
  GP_2_UP = 383
  GP_3_BUTTON_0 = 353
  GP_3_BUTTON_1 = 354
  GP_3_BUTTON_10 = 363
  GP_3_BUTTON_11 = 364
  GP_3_BUTTON_12 = 365
  GP_3_BUTTON_13 = 366
  GP_3_BUTTON_14 = 367
  GP_3_BUTTON_15 = 368
  GP_3_BUTTON_2 = 355
  GP_3_BUTTON_3 = 356
  GP_3_BUTTON_4 = 357
  GP_3_BUTTON_5 = 358
  GP_3_BUTTON_6 = 359
  GP_3_BUTTON_7 = 360
  GP_3_BUTTON_8 = 361
  GP_3_BUTTON_9 = 362
  GP_3_DOWN = 388
  GP_3_DPAD_DOWN = 352
  GP_3_DPAD_LEFT = 349
  GP_3_DPAD_RIGHT = 350
  GP_3_DPAD_UP = 351
  GP_3_LEFT = 385
  GP_3_LEFT_STICK_X_AXIS = 24
  GP_3_LEFT_STICK_Y_AXIS = 25
  GP_3_LEFT_TRIGGER_AXIS = 28
  GP_3_RIGHT = 386
  GP_3_RIGHT_STICK_X_AXIS = 26
  GP_3_RIGHT_STICK_Y_AXIS = 27
  GP_3_RIGHT_TRIGGER_AXIS = 29
  GP_3_UP = 387
  GP_BUTTON_0 = 273
  GP_BUTTON_1 = 274
  GP_BUTTON_10 = 283
  GP_BUTTON_11 = 284
  GP_BUTTON_12 = 285
  GP_BUTTON_13 = 286
  GP_BUTTON_14 = 287
  GP_BUTTON_15 = 288
  GP_BUTTON_2 = 275
  GP_BUTTON_3 = 276
  GP_BUTTON_4 = 277
  GP_BUTTON_5 = 278
  GP_BUTTON_6 = 279
  GP_BUTTON_7 = 280
  GP_BUTTON_8 = 281
  GP_BUTTON_9 = 282
  GP_DOWN = 372
  GP_DPAD_DOWN = 272
  GP_DPAD_LEFT = 269
  GP_DPAD_RIGHT = 270
  GP_DPAD_UP = 271
  GP_LEFT = 369
  GP_LEFT_STICK_X_AXIS = 0
  GP_LEFT_STICK_Y_AXIS = 1
  GP_LEFT_TRIGGER_AXIS = 4
  GP_RIGHT = 370
  GP_RIGHT_STICK_X_AXIS = 2
  GP_RIGHT_STICK_Y_AXIS = 3
  GP_RIGHT_TRIGGER_AXIS = 5
  GP_UP = 371
  KB_0 = 39
  KB_1 = 30
  KB_2 = 31
  KB_3 = 32
  KB_4 = 33
  KB_5 = 34
  KB_6 = 35
  KB_7 = 36
  KB_8 = 37
  KB_9 = 38
  KB_A = 4
  KB_APOSTROPHE = 52
  KB_B = 5
  KB_BACKSLASH = 49
  KB_BACKSPACE = 42
  KB_BACKTICK = 53
  KB_C = 6
  KB_CAPS_LOCK = 57
  KB_COMMA = 54
  KB_D = 7
  KB_DELETE = 76
  KB_DOWN = 81
  KB_E = 8
  KB_END = 77
  KB_ENTER = 88
  KB_EQUALS = 46
  KB_ESCAPE = 41
  KB_F = 9
  KB_F1 = 58
  KB_F10 = 67
  KB_F11 = 68
  KB_F12 = 69
  KB_F2 = 59
  KB_F3 = 60
  KB_F4 = 61
  KB_F5 = 62
  KB_F6 = 63
  KB_F7 = 64
  KB_F8 = 65
  KB_F9 = 66
  KB_G = 10
  KB_H = 11
  KB_HOME = 74
  KB_I = 12
  KB_INSERT = 73
  KB_ISO = 100
  KB_J = 13
  KB_K = 14
  KB_L = 15
  KB_LEFT = 80
  KB_LEFT_ALT = 226
  KB_LEFT_BRACKET = 47
  KB_LEFT_CONTROL = 224
  KB_LEFT_META = 227
  KB_LEFT_SHIFT = 225
  KB_M = 16
  KB_MINUS = 45
  KB_N = 17
  KB_NUMPAD_0 = 98
  KB_NUMPAD_1 = 89
  KB_NUMPAD_2 = 90
  KB_NUMPAD_3 = 91
  KB_NUMPAD_4 = 92
  KB_NUMPAD_5 = 93
  KB_NUMPAD_6 = 94
  KB_NUMPAD_7 = 95
  KB_NUMPAD_8 = 96
  KB_NUMPAD_9 = 97
  KB_NUMPAD_DELETE = 99
  KB_NUMPAD_DIVIDE = 84
  KB_NUMPAD_MINUS = 86
  KB_NUMPAD_MULTIPLY = 85
  KB_NUMPAD_PLUS = 87
  KB_O = 18
  KB_P = 19
  KB_PAGE_DOWN = 78
  KB_PAGE_UP = 75
  KB_PAUSE = 72
  KB_PERIOD = 55
  KB_PRINT_SCREEN = 70
  KB_Q = 20
  KB_R = 21
  KB_RETURN = 40
  KB_RIGHT = 79
  KB_RIGHT_ALT = 230
  KB_RIGHT_BRACKET = 48
  KB_RIGHT_CONTROL = 228
  KB_RIGHT_META = 231
  KB_RIGHT_SHIFT = 229
  KB_S = 22
  KB_SCROLL_LOCK = 71
  KB_SEMICOLON = 51
  KB_SLASH = 56
  KB_SPACE = 44
  KB_T = 23
  KB_TAB = 43
  KB_U = 24
  KB_UP = 82
  KB_V = 25
  KB_W = 26
  KB_X = 27
  KB_Y = 28
  KB_Z = 29
  LICENSES = "This software may utilize code from the following third-party libraries:\n\nGosu, https://www.libgosu.org, MIT License, https://opensource.org/licenses/MIT\nSDL 2, https://www.libsdl.org, MIT License, https://opensource.org/licenses/MIT\nlibsndfile, http://www.mega-nerd.com/libsndfile, GNU LGPL 3, https://www.gnu.org/copyleft/lesser.html\nmpg123, https://mpg123.de, GNU LGPL 3, https://www.gnu.org/copyleft/lesser.html\n"
  MAJOR_VERSION = 1
  MAX_TEXTURE_SIZE = 1024
  MINOR_VERSION = 4
  MS_LEFT = 256
  MS_MIDDLE = 257
  MS_OTHER_0 = 261
  MS_OTHER_1 = 262
  MS_OTHER_2 = 263
  MS_OTHER_3 = 264
  MS_OTHER_4 = 265
  MS_OTHER_5 = 266
  MS_OTHER_6 = 267
  MS_OTHER_7 = 268
  MS_RIGHT = 258
  MS_WHEEL_DOWN = 260
  MS_WHEEL_UP = 259
  POINT_VERSION = 6
  VERSION = "1.4.6"

  # ---------------------------------------------------------------------
  # Gosu::Color -- 32-bit ARGB colour value.
  # ---------------------------------------------------------------------
  class Color
    attr_accessor :alpha, :red, :green, :blue

    def initialize(*args)
      case args.size
      when 1
        argb_packed = args[0].to_i
        @alpha = (argb_packed >> 24) & 0xff
        @red   = (argb_packed >> 16) & 0xff
        @green = (argb_packed >>  8) & 0xff
        @blue  =  argb_packed        & 0xff
      when 3
        @alpha = 255
        @red, @green, @blue = args.map(&:to_i)
      when 4
        @alpha, @red, @green, @blue = args.map(&:to_i)
      else
        raise ArgumentError,
              "wrong number of arguments (given #{args.size}, expected 1, 3 or 4)"
      end
    end

    def self.rgb(r, g, b)
      new(255, r, g, b)
    end

    def self.rgba(r, g, b, a)
      new(a, r, g, b)
    end

    def self.argb(a_or_packed, r = nil, g = nil, b = nil)
      r.nil? ? new(a_or_packed) : new(a_or_packed, r, g, b)
    end

    def self.from_hsv(h, s, v)
      new(255, *_hsv_to_rgb(h, s, v))
    end

    def self.from_ahsv(a, h, s, v)
      new(a, *_hsv_to_rgb(h, s, v))
    end

    def self._hsv_to_rgb(h, s, v)
      h = h.to_f % 360.0
      s = s.to_f.clamp(0.0, 1.0)
      v = v.to_f.clamp(0.0, 1.0)
      c = v * s
      x = c * (1 - ((h / 60.0) % 2 - 1).abs)
      m = v - c
      r1, g1, b1 =
        case (h / 60).to_i
        when 0 then [c, x, 0]
        when 1 then [x, c, 0]
        when 2 then [0, c, x]
        when 3 then [0, x, c]
        when 4 then [x, 0, c]
        else        [c, 0, x]
        end
      [((r1 + m) * 255).round, ((g1 + m) * 255).round, ((b1 + m) * 255).round]
    end

    def hue; 0.0; end
    def saturation; 0.0; end
    def value; 0.0; end
    def hue=(_); end
    def saturation=(_); end
    def value=(_); end

    def to_i
      (@alpha << 24) | (@red << 16) | (@green << 8) | @blue
    end
    alias_method :argb, :to_i

    def bgr
      (@blue << 16) | (@green << 8) | @red
    end

    def abgr
      (@alpha << 24) | (@blue << 16) | (@green << 8) | @red
    end

    # GL (little-endian RGBA) layout matches ABGR byte order.
    alias_method :gl, :abgr

    def dup
      Color.new(@alpha, @red, @green, @blue)
    end

    def ==(other)
      other.is_a?(Color) &&
        @alpha == other.alpha && @red == other.red &&
        @green == other.green && @blue == other.blue
    end
  end

  # ---------------------------------------------------------------------
  # Window -- real SDL2-backed game window. `show` runs an event loop
  # that pumps SDL events, calls `update` / `draw`, and renders to the
  # window until `close` is called (or the user closes the window).
  # ---------------------------------------------------------------------
  class Window
    attr_accessor :caption, :width, :height, :mouse_x, :mouse_y,
                  :update_interval, :text_input

    def initialize(width, height, fullscreen_or_flags = 0,
                   update_interval = 16.666666)
      @width  = width
      @height = height
      @caption = ""
      @mouse_x = 0.0
      @mouse_y = 0.0
      @update_interval = update_interval
      @text_input = nil
      @fullscreen = fullscreen_or_flags != 0
      @resizable = false
      @borderless = false
      @_sdl_window = nil
      @_sdl_renderer = nil
      @_closing = false
      @_event_buf = FFI::MemoryPointer.new(:uint8, SDL2::EVENT_SIZE)
    end

    def show
      _lazy_sdl_init
      _create_window unless @_sdl_window
      Gosu._current_window = self
      last_tick = SDL2.get_ticks
      until @_closing
        _pump_events
        now = SDL2.get_ticks
        if now - last_tick >= @update_interval
          update
          last_tick = now
        end
        draw
        SDL2.render_present(@_sdl_renderer)
        SDL2.delay(1)
      end
      close!
      self
    end

    def close
      @_closing = true
    end

    def close!
      if @_sdl_renderer
        SDL2.destroy_renderer(@_sdl_renderer)
        @_sdl_renderer = nil
      end
      if @_sdl_window
        SDL2.destroy_window(@_sdl_window)
        @_sdl_window = nil
      end
      Gosu._current_window = nil if Gosu._current_window == self
    end

    def tick; false; end

    def caption=(title)
      @caption = title.to_s
      SDL2.set_window_title(@_sdl_window, @caption) if @_sdl_window
    end

    def fullscreen?; @fullscreen; end
    def fullscreen=(v); @fullscreen = !!v; end
    def resizable?; @resizable; end
    def resizable=(v); @resizable = !!v; end
    def borderless?; @borderless; end
    def borderless=(v); @borderless = !!v; end

    # Internal: exposed so `Gosu.draw_rect` / `Gosu.draw_line` can reach
    # the SDL renderer attached to the currently-showing window.
    def _sdl_renderer; @_sdl_renderer; end

    # Callbacks default to no-op so subclasses override only what they
    # actually use.
    def update; end
    def draw; end
    def needs_redraw?; true; end
    def needs_cursor?; false; end
    def button_down(_id); end
    def button_up(_id); end
    def gain_focus; end
    def lose_focus; end
    def gamepad_connected(_index); end
    def gamepad_disconnected(_index); end
    def drop(_filename); end

    private

    @@_sdl_inited = false

    def _lazy_sdl_init
      return if @@_sdl_inited
      if SDL2.init(SDL2::INIT_VIDEO | SDL2::INIT_EVENTS) != 0
        raise RuntimeError, "SDL_Init failed: #{SDL2.get_error}"
      end
      @@_sdl_inited = true
      at_exit { SDL2.quit }
    end

    def _create_window
      flags = SDL2::WINDOW_SHOWN
      flags |= SDL2::WINDOW_FULLSCREEN_DESKTOP if @fullscreen
      flags |= SDL2::WINDOW_RESIZABLE          if @resizable
      flags |= SDL2::WINDOW_BORDERLESS         if @borderless
      @_sdl_window = SDL2.create_window(
        @caption.empty? ? "Gosu" : @caption,
        SDL2::WINDOWPOS_CENTERED, SDL2::WINDOWPOS_CENTERED,
        @width, @height, flags
      )
      if @_sdl_window.null?
        raise RuntimeError, "SDL_CreateWindow failed: #{SDL2.get_error}"
      end
      @_sdl_renderer = SDL2.create_renderer(
        @_sdl_window, -1,
        SDL2::RENDERER_ACCELERATED | SDL2::RENDERER_PRESENTVSYNC
      )
      if @_sdl_renderer.null?
        # Fall back to software renderer (e.g. under SDL_VIDEODRIVER=dummy).
        @_sdl_renderer = SDL2.create_renderer(@_sdl_window, -1,
                                              SDL2::RENDERER_SOFTWARE)
      end
      if @_sdl_renderer.null?
        raise RuntimeError, "SDL_CreateRenderer failed: #{SDL2.get_error}"
      end
      SDL2.set_draw_color(@_sdl_renderer, 0, 0, 0, 255)
      SDL2.render_clear(@_sdl_renderer)
    end

    # SDL_Event field offsets we care about (x86_64 / little-endian):
    #   event.type             uint32 @ 0
    #   SDL_KeyboardEvent:
    #     .keysym.scancode     int32  @ 16
    #   SDL_MouseMotionEvent:
    #     .x                   int32  @ 20
    #     .y                   int32  @ 24
    #   SDL_MouseButtonEvent:
    #     .button              uint8  @ 16
    # All other fields are ignored for now.
    def _pump_events
      while SDL2.poll_event(@_event_buf) != 0
        type = @_event_buf.get_uint32(0)
        case type
        when SDL2::EVENT_QUIT
          close
        when SDL2::EVENT_KEYDOWN
          scancode = @_event_buf.get_int32(16)
          button_down(scancode)
        when SDL2::EVENT_KEYUP
          scancode = @_event_buf.get_int32(16)
          button_up(scancode)
        when SDL2::EVENT_MOUSEMOTION
          @mouse_x = @_event_buf.get_int32(20).to_f
          @mouse_y = @_event_buf.get_int32(24).to_f
        when SDL2::EVENT_MOUSEBUTTONDOWN
          btn = @_event_buf.get_uint8(16)
          button_down(255 + btn)
        when SDL2::EVENT_MOUSEBUTTONUP
          btn = @_event_buf.get_uint8(16)
          button_up(255 + btn)
        end
      end
    end

    public

    def minimize; end
    def restore; end
    def minimized?; false; end

    def set_mouse_position(_x, _y); end

    def self.button_id_to_char(_id); nil; end
    def self.char_to_button_id(_c); nil; end
  end

  # ---------------------------------------------------------------------
  # Image / Font / Sample / Song / Channel / TextInput / SampleInstance /
  # GLTexInfo -- stubs that accept the CRuby constructors and expose the
  # documented accessors.
  # ---------------------------------------------------------------------
  class Image
    attr_reader :width, :height, :columns, :rows

    def initialize(source = nil, *_opts)
      @_texture = nil
      @_renderer = nil  # the renderer the texture was created against
      @_owns_texture = true

      if source.is_a?(String)
        _load_from_path(source)
      elsif source.respond_to?(:columns) && source.respond_to?(:rows)
        # `Image::BlobHelper`-shaped object from `Image.from_blob`.
        # Texture creation is deferred until first `#draw`.
        @width  = @columns = source.columns.to_i
        @height = @rows    = source.rows.to_i
        @_blob_rgba = source.to_blob if source.respond_to?(:to_blob)
      elsif source.nil?
        @width = @columns = 0
        @height = @rows   = 0
      else
        @width = @columns = 0
        @height = @rows   = 0
      end
    end

    def self.from_text(text, height, opts = {})
      Gosu._lazy_ttf_init
      path = (opts.is_a?(Hash) && opts[:font_name].is_a?(String) &&
              File.file?(opts[:font_name])) ? opts[:font_name] : DEFAULT_FONT_PATH
      font = SDL2_ttf.ttf_open_font(path, height.to_i)
      if font.null?
        raise RuntimeError,
              "TTF_OpenFont failed: #{SDL2_ttf.ttf_get_error}"
      end
      # Render white; tint via `#draw`'s colour argument.
      surface = SDL2_ttf.ttf_render_utf8_blended(font, text.to_s,
                                                  0xffffffff)
      SDL2_ttf.ttf_close_font(font)
      if surface.null?
        raise RuntimeError,
              "TTF_RenderUTF8_Blended failed: #{SDL2_ttf.ttf_get_error}"
      end
      img = allocate
      img._init_from_surface(surface)
      img
    end
    def self.from_text_without_window(text, height, opts = {})
      from_text(text, height, opts)
    end
    def self.from_markup(_text, _height, *_opts); new; end
    def self.load_tiles(_source, _tile_w, _tile_h, *_opts); []; end

    # Image#draw(x, y, z, scale_x = 1, scale_y = 1, color = WHITE,
    #            mode = :default)
    def draw(x, y, _z = 0, scale_x = 1.0, scale_y = 1.0,
             color = nil, _mode = :default)
      return unless _ensure_texture
      _apply_color_mod(color)
      tx, ty, msx, msy, angle = Gosu._decompose
      px, py = Gosu._apply_point(x, y)
      w = (@width  * scale_x * msx).to_f
      h = (@height * scale_y * msy).to_f
      dst = SDL2::FRect.new
      dst[:x] = px.to_f
      dst[:y] = py.to_f
      dst[:w] = w
      dst[:h] = h
      if angle == 0.0
        SDL2.render_copy_f(@_renderer, @_texture, nil, dst.pointer)
      else
        center = SDL2::FPoint.new
        center[:x] = 0.0
        center[:y] = 0.0
        SDL2.render_copy_ex_f(@_renderer, @_texture, nil, dst.pointer,
                            angle, center.pointer, SDL2::SDL_FLIP_NONE)
      end
    end

    # Image#draw_rot(x, y, z, angle, center_x = 0.5, center_y = 0.5,
    #                scale_x = 1, scale_y = 1, color = WHITE,
    #                mode = :default)
    def draw_rot(x, y, _z = 0, angle = 0,
                 center_x = 0.5, center_y = 0.5,
                 scale_x = 1.0, scale_y = 1.0,
                 color = nil, _mode = :default)
      return unless _ensure_texture
      _apply_color_mod(color)
      _, _, msx, msy, extra_angle = Gosu._decompose
      px, py = Gosu._apply_point(x, y)
      w = (@width  * scale_x * msx).to_f
      h = (@height * scale_y * msy).to_f
      dst = SDL2::FRect.new
      dst[:x] = (px - w * center_x).to_f
      dst[:y] = (py - h * center_y).to_f
      dst[:w] = w
      dst[:h] = h
      center = SDL2::FPoint.new
      center[:x] = (w * center_x).to_f
      center[:y] = (h * center_y).to_f
      SDL2.render_copy_ex_f(@_renderer, @_texture, nil, dst.pointer,
                          angle.to_f + extra_angle, center.pointer,
                          SDL2::SDL_FLIP_NONE)
    end

    def draw_as_quad(*); end
    def draw_mod(*); end
    def to_blob; "\0\0\0\0" * (width * height); end
    def save(_path); end
    def insert(_img, _x, _y); self; end
    def subimage(_x, _y, _w, _h); Image.new; end
    def gl_tex_info; nil; end

    private

    def _load_from_path(path)
      @_image_path = path
      surface = SDL2_image.img_load(path)
      if surface.null?
        raise RuntimeError,
              "IMG_Load(#{path.inspect}) failed: #{SDL2_image.img_get_error}"
      end
      _init_from_surface(surface)
    end

    public

    # Adopt an already-loaded SDL surface. Used by `Image.from_text` to
    # feed a TTF-rendered surface into a fresh Image instance.
    def _init_from_surface(surface)
      @width  = @columns = surface.get_int32(SDL2::SURFACE_W_OFFSET)
      @height = @rows    = surface.get_int32(SDL2::SURFACE_H_OFFSET)
      @_pending_surface  = surface  # converted to texture on first draw
      @_texture = nil
      @_renderer = nil
      @_owns_texture = true
    end

    private

    def _ensure_texture
      win = Gosu._current_window
      ren = win && win._sdl_renderer
      return false unless ren

      # If we already have a texture for THIS renderer, reuse it.
      return true if @_texture && @_renderer == ren

      _destroy_texture
      surface = @_pending_surface
      surface = nil unless surface && !surface.null?

      if surface
        @_texture = SDL2.create_texture_from_surface(ren, surface)
        SDL2.free_surface(surface)
        @_pending_surface = nil
      else
        return false
      end
      if @_texture.null?
        @_texture = nil
        return false
      end
      SDL2.set_texture_blend_mode(@_texture, 1)  # SDL_BLENDMODE_BLEND
      @_renderer = ren
      true
    end

    def _apply_color_mod(color)
      return unless @_texture
      if color.is_a?(Color)
        SDL2.set_texture_color_mod(@_texture, color.red, color.green, color.blue)
        SDL2.set_texture_alpha_mod(@_texture, color.alpha)
      elsif color.is_a?(Integer)
        SDL2.set_texture_color_mod(@_texture,
                                    (color >> 16) & 0xff,
                                    (color >>  8) & 0xff,
                                    color         & 0xff)
        SDL2.set_texture_alpha_mod(@_texture, (color >> 24) & 0xff)
      else
        SDL2.set_texture_color_mod(@_texture, 255, 255, 255)
        SDL2.set_texture_alpha_mod(@_texture, 255)
      end
    end

    def _destroy_texture
      if @_texture && !@_texture.null?
        SDL2.destroy_texture(@_texture)
      end
      @_texture = nil
      @_renderer = nil
    end
  end

  # ---------------------------------------------------------------------
  # Fonts: lazy SDL2_ttf init.
  # ---------------------------------------------------------------------
  @@_ttf_inited = false
  def self._lazy_ttf_init
    return if @@_ttf_inited
    if SDL2_ttf.ttf_init != 0
      raise RuntimeError, "TTF_Init failed: #{SDL2_ttf.ttf_get_error}"
    end
    @@_ttf_inited = true
    at_exit { SDL2_ttf.ttf_quit }
  end

  # Default font path; falls back to a well-known DejaVu file present
  # on most Linux distros. Can be overridden with `Gosu::DEFAULT_FONT_PATH`.
  DEFAULT_FONT_PATH =
    (ENV["GOSU_DEFAULT_FONT"].to_s.empty? ?
       "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" :
       ENV["GOSU_DEFAULT_FONT"]).freeze

  class Font
    attr_accessor :name, :height

    def initialize(height, *opts)
      Gosu._lazy_ttf_init
      @height = height.to_i
      @_options = opts.first.is_a?(Hash) ? opts.first : {}
      @name = @_options[:name] || ""
      path = _resolve_font_path(@name)
      @_ttf_font = SDL2_ttf.ttf_open_font(path, @height)
      if @_ttf_font.null?
        raise RuntimeError,
              "TTF_OpenFont(#{path.inspect}, #{@height}) failed: #{SDL2_ttf.ttf_get_error}"
      end
      # Cache textures per (text, color, renderer) so redraws are cheap.
      @_cache = {}
    end

    # draw_text(text, x, y, z = 0, scale_x = 1, scale_y = 1,
    #           color = WHITE, mode = :default)
    def draw_text(text, x, y, _z = 0, scale_x = 1.0, scale_y = 1.0,
                  color = 0xffffffff, _mode = :default)
      tex, w, h = _texture_for(text.to_s, color)
      return unless tex
      ren = Gosu._current_window&._sdl_renderer
      return unless ren
      dst = SDL2::Rect.new
      dst[:x] = x.to_i
      dst[:y] = y.to_i
      dst[:w] = (w * scale_x).to_i
      dst[:h] = (h * scale_y).to_i
      SDL2.render_copy(ren, tex, nil, dst.pointer)
    end

    # draw_text_rel(text, x, y, z, rel_x, rel_y, scale_x, scale_y,
    #               color, mode) -- relative anchor.
    def draw_text_rel(text, x, y, z = 0, rel_x = 0.0, rel_y = 0.0,
                      scale_x = 1.0, scale_y = 1.0,
                      color = 0xffffffff, mode = :default)
      w = text_width(text) * scale_x
      h = @height * scale_y
      draw_text(text, x - w * rel_x, y - h * rel_y, z,
                scale_x, scale_y, color, mode)
    end

    def draw_markup(*args)
      draw_text(*args)
    end

    def draw_markup_rel(*args)
      draw_text_rel(*args)
    end

    def text_width(text, scale_x = 1.0)
      return 0 if text.to_s.empty?
      w_ptr = FFI::MemoryPointer.new(:int)
      h_ptr = FFI::MemoryPointer.new(:int)
      SDL2_ttf.ttf_size_utf8(@_ttf_font, text.to_s, w_ptr, h_ptr)
      (w_ptr.get_int32(0) * scale_x).to_i
    end

    def markup_width(text, *opts); text_width(text, *opts); end

    def set_image(_codepoint, _image); end
    def []=(*); end

    private

    def _resolve_font_path(name)
      return name if name.is_a?(String) && File.file?(name)
      DEFAULT_FONT_PATH
    end

    # Returns [SDL_Texture*, width, height] or nil if the renderer is
    # not yet available (e.g. Font.text_width before Window#show).
    def _texture_for(text, color)
      return [nil, 0, 0] if text.empty?
      ren = Gosu._current_window&._sdl_renderer
      return [nil, 0, 0] unless ren

      argb = _color_to_argb(color)
      key = [text, argb, ren.address]
      if (entry = @_cache[key])
        return entry
      end

      r = (argb >> 16) & 0xff
      g = (argb >>  8) & 0xff
      b =  argb        & 0xff
      a = (argb >> 24) & 0xff
      # SDL_Color packed little-endian: r | g<<8 | b<<16 | a<<24.
      packed = r | (g << 8) | (b << 16) | (a << 24)
      surface = SDL2_ttf.ttf_render_utf8_blended(@_ttf_font, text, packed)
      return [nil, 0, 0] if surface.null?
      w = surface.get_int32(SDL2::SURFACE_W_OFFSET)
      h = surface.get_int32(SDL2::SURFACE_H_OFFSET)
      tex = SDL2.create_texture_from_surface(ren, surface)
      SDL2.free_surface(surface)
      return [nil, 0, 0] if tex.null?
      SDL2.set_texture_blend_mode(tex, 1)  # BLEND
      @_cache[key] = [tex, w, h]
    end

    def _color_to_argb(color)
      if color.is_a?(Color)
        color.to_i
      else
        color.to_i & 0xffffffff
      end
    end
  end

  # ---------------------------------------------------------------------
  # Audio: lazy SDL2_mixer init shared by Sample / Song.
  # ---------------------------------------------------------------------
  @@_mixer_inited = false
  def self._lazy_mixer_init
    return if @@_mixer_inited
    if SDL2.init_sub_system(SDL2::INIT_AUDIO) != 0
      raise RuntimeError, "SDL_InitSubSystem(AUDIO) failed: #{SDL2.get_error}"
    end
    if SDL2_mixer.mix_open_audio(SDL2_mixer::DEFAULT_FREQUENCY,
                                 SDL2_mixer::AUDIO_S16LSB,
                                 SDL2_mixer::DEFAULT_CHANNELS,
                                 SDL2_mixer::DEFAULT_CHUNKSIZE) != 0
      raise RuntimeError,
            "Mix_OpenAudio failed: #{SDL2_mixer.mix_get_error}"
    end
    SDL2_mixer.mix_allocate_channels(16)
    @@_mixer_inited = true
    at_exit do
      SDL2_mixer.mix_close_audio
    end
  end

  class Sample
    def initialize(path)
      Gosu._lazy_mixer_init
      @_chunk = SDL2_mixer.mix_load_wav(path.to_s)
      if @_chunk.null?
        raise RuntimeError,
              "Mix_LoadWAV(#{path.inspect}) failed: #{SDL2_mixer.mix_get_error}"
      end
    end

    def play(volume = 1.0, _speed = 1.0, looping = false)
      ch = SDL2_mixer.mix_play_channel(-1, @_chunk, looping ? -1 : 0)
      if ch >= 0
        SDL2_mixer.mix_volume(ch, (volume.to_f.clamp(0.0, 1.0) * 128).to_i)
        Channel.new(ch)
      else
        Channel.new(-1)
      end
    end

    def play_pan(pan = 0.0, volume = 1.0, speed = 1.0, looping = false)
      ch = play(volume, speed, looping)
      ch.pan = pan if ch && ch.respond_to?(:pan=)
      ch
    end
  end

  class Song
    @@_current = nil
    def self.current_song; @@_current; end
    def self.update; end

    def initialize(path)
      Gosu._lazy_mixer_init
      @_music = SDL2_mixer.mix_load_mus(path.to_s)
      if @_music.null?
        raise RuntimeError,
              "Mix_LoadMUS(#{path.inspect}) failed: #{SDL2_mixer.mix_get_error}"
      end
      @_volume = 1.0
    end

    def play(looping = false)
      SDL2_mixer.mix_volume_music((@_volume.clamp(0.0, 1.0) * 128).to_i)
      SDL2_mixer.mix_play_music(@_music, looping ? -1 : 1)
      @@_current = self
    end

    def pause
      SDL2_mixer.mix_pause_music
    end

    def paused?
      SDL2_mixer.mix_paused_music != 0
    end

    def stop
      SDL2_mixer.mix_halt_music
      @@_current = nil if @@_current.equal?(self)
    end

    def playing?
      @@_current.equal?(self) && SDL2_mixer.mix_playing_music != 0
    end

    def volume; @_volume; end
    def volume=(v)
      @_volume = v.to_f
      SDL2_mixer.mix_volume_music((@_volume.clamp(0.0, 1.0) * 128).to_i) if playing?
    end
  end

  class Channel
    attr_reader :_channel_id

    def initialize(channel_id = -1)
      @_channel_id = channel_id
      @_pan = 0.0
      @_volume = 1.0
    end

    def current_channel; self; end

    def playing?
      @_channel_id >= 0 && SDL2_mixer.mix_playing(@_channel_id) != 0
    end

    def paused?
      @_channel_id >= 0 && SDL2_mixer.mix_paused(@_channel_id) != 0
    end

    def pause
      SDL2_mixer.mix_pause(@_channel_id) if @_channel_id >= 0
    end

    def resume
      SDL2_mixer.mix_resume(@_channel_id) if @_channel_id >= 0
    end

    def stop
      SDL2_mixer.mix_halt_channel(@_channel_id) if @_channel_id >= 0
    end

    def volume; @_volume; end
    def volume=(v)
      @_volume = v.to_f
      if @_channel_id >= 0
        SDL2_mixer.mix_volume(@_channel_id, (@_volume.clamp(0.0, 1.0) * 128).to_i)
      end
    end

    def pan; @_pan; end
    def pan=(p)
      @_pan = p.to_f.clamp(-1.0, 1.0)
      if @_channel_id >= 0
        # SDL_mixer pan: 0..255 left, 0..255 right.
        if @_pan <= 0
          left = 255
          right = (255 * (1.0 + @_pan)).to_i
        else
          left = (255 * (1.0 - @_pan)).to_i
          right = 255
        end
        SDL2_mixer.mix_set_panning(@_channel_id, left, right)
      end
    end
  end

  class TextInput
    attr_accessor :text, :caret_pos, :selection_start, :filter

    def initialize
      @text = ""
      @caret_pos = 0
      @selection_start = 0
      @filter = nil
    end

    def delete_forward; end
    def delete_backward; end
    def insert_text(_); end
  end

  class GLTexInfo
    attr_accessor :tex_name, :left, :right, :top, :bottom

    def initialize
      @tex_name = 0
      @left = 0.0
      @right = 1.0
      @top = 0.0
      @bottom = 1.0
    end
  end

  # ---------------------------------------------------------------------
  # Module-level helpers.
  # ---------------------------------------------------------------------
  # The window that is currently running `#show`. `draw_rect` etc. reach
  # through it to the SDL renderer.
  @@_current_window = nil
  def self._current_window; @@_current_window; end
  def self._current_window=(w); @@_current_window = w; end

  # ---------------------------------------------------------------------
  # Transform stack
  # ---------------------------------------------------------------------
  # Each entry is a 6-element Array [a, b, c, d, tx, ty] representing
  # the affine matrix
  #     | a c tx |
  #     | b d ty |
  #     | 0 0  1 |
  # so a world point (x, y) maps to (a*x + c*y + tx, b*x + d*y + ty).
  IDENTITY_MATRIX = [1.0, 0.0, 0.0, 1.0, 0.0, 0.0].freeze
  @@_mat_stack = [IDENTITY_MATRIX]

  def self._mat; @@_mat_stack.last; end
  def self._mat_mul(m1, m2)
    a1, b1, c1, d1, tx1, ty1 = m1
    a2, b2, c2, d2, tx2, ty2 = m2
    [a1 * a2 + c1 * b2,
     b1 * a2 + d1 * b2,
     a1 * c2 + c1 * d2,
     b1 * c2 + d1 * d2,
     a1 * tx2 + c1 * ty2 + tx1,
     b1 * tx2 + d1 * ty2 + ty1]
  end

  def self._apply_point(x, y)
    m = _mat
    # Fast path for the (very common) identity case — the JIT's float
    # register allocator thrashes hot inlining `a*x + c*y + tx` etc.
    return [x.to_f, y.to_f] if m.equal?(IDENTITY_MATRIX)
    a, b, c, d, tx, ty = m
    [a * x + c * y + tx, b * x + d * y + ty]
  end

  # Decompose the current affine into (tx, ty, scale_x, scale_y,
  # angle_in_degrees) suitable for SDL_RenderCopyEx. Assumes no shear.
  def self._decompose
    m = _mat
    return [0.0, 0.0, 1.0, 1.0, 0.0] if m.equal?(IDENTITY_MATRIX)
    a, b, c, d, tx, ty = m
    sx = Math.sqrt(a * a + b * b)
    sy = Math.sqrt(c * c + d * d)
    # Fix sign of sy via the determinant, so a pure Y-flip stays a flip.
    sy = -sy if (a * d - b * c) < 0
    angle = Math.atan2(b, a) * 180.0 / Math::PI
    [tx, ty, sx, sy, angle]
  end

  def self._push_matrix(m)
    @@_mat_stack.push(_mat_mul(_mat, m))
    begin
      yield if block_given?
    ensure
      @@_mat_stack.pop
    end
  end

  class << self
    # Split an ARGB / Color into 4 bytes for SDL colour calls.
    def _split_color(color)
      if color.is_a?(Color)
        [color.red, color.green, color.blue, color.alpha]
      else
        argb = color.to_i
        [(argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff, (argb >> 24) & 0xff]
      end
    end
    private :_split_color

    # -------- Transform block helpers (Gosu.translate / rotate / scale /
    # transform). Each yields under the resulting matrix then pops on
    # exit. --------
    def translate(dx, dy, &block)
      _push_matrix([1.0, 0.0, 0.0, 1.0, dx.to_f, dy.to_f], &block)
    end

    def scale(sx, sy = nil, around_x = 0.0, around_y = 0.0, &block)
      sy ||= sx
      m = [sx.to_f, 0.0, 0.0, sy.to_f, 0.0, 0.0]
      if around_x != 0 || around_y != 0
        m = _mat_mul([1.0, 0.0, 0.0, 1.0, around_x.to_f, around_y.to_f], m)
        m = _mat_mul(m, [1.0, 0.0, 0.0, 1.0, -around_x.to_f, -around_y.to_f])
      end
      _push_matrix(m, &block)
    end

    def rotate(degrees, around_x = 0.0, around_y = 0.0, &block)
      rad = degrees.to_f * Math::PI / 180.0
      cos_r = Math.cos(rad)
      sin_r = Math.sin(rad)
      m = [cos_r, sin_r, -sin_r, cos_r, 0.0, 0.0]
      if around_x != 0 || around_y != 0
        m = _mat_mul([1.0, 0.0, 0.0, 1.0, around_x.to_f, around_y.to_f], m)
        m = _mat_mul(m, [1.0, 0.0, 0.0, 1.0, -around_x.to_f, -around_y.to_f])
      end
      _push_matrix(m, &block)
    end

    def transform(a, b, c, d, tx, ty, &block)
      _push_matrix([a.to_f, b.to_f, c.to_f, d.to_f, tx.to_f, ty.to_f], &block)
    end

    # Build a 2-triangle (4-vertex) SDL_RenderGeometry draw from four
    # transformed corner points of colour `rgba`.
    def _render_quad(ren, corners, rgba)
      r, g, b, a = rgba
      verts = FFI::MemoryPointer.new(:uint8, 20 * 4)
      corners.each_with_index do |(px, py), i|
        off = i * 20
        verts.put_float32(off, px)
        verts.put_float32(off + 4, py)
        verts.put_uint8(off + 8,  r)
        verts.put_uint8(off + 9,  g)
        verts.put_uint8(off + 10, b)
        verts.put_uint8(off + 11, a)
        verts.put_float32(off + 12, 0.0)
        verts.put_float32(off + 16, 0.0)
      end
      idx = FFI::MemoryPointer.new(:int32, 6)
      [0, 1, 2, 0, 2, 3].each_with_index { |v, i| idx.put_int32(i * 4, v) }
      SDL2.render_geometry(ren, nil, verts, 4, idx, 6)
    end
    private :_render_quad

    def _identity_transform?
      _mat == [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]
    end
    private :_identity_transform?

    def draw_rect(x, y, w, h, color, _z = 0, _mode = :default)
      ren = (_current_window && _current_window._sdl_renderer)
      return unless ren
      rgba = _split_color(color)
      SDL2.set_draw_blend_mode(ren, 1) # SDL_BLENDMODE_BLEND
      SDL2.set_draw_color(ren, *rgba)

      if _identity_transform?
        frect = SDL2::FRect.new
        frect[:x] = x.to_f
        frect[:y] = y.to_f
        frect[:w] = w.to_f
        frect[:h] = h.to_f
        SDL2.render_fill_rect_f(ren, frect.pointer)
      else
        corners = [
          _apply_point(x,     y    ),
          _apply_point(x + w, y    ),
          _apply_point(x + w, y + h),
          _apply_point(x,     y + h),
        ]
        _render_quad(ren, corners, rgba)
      end
    end

    def draw_line(x1, y1, c1, x2, y2, _c2 = nil, _z = 0, _mode = :default)
      ren = (_current_window && _current_window._sdl_renderer)
      return unless ren
      rgba = _split_color(c1)
      SDL2.set_draw_blend_mode(ren, 1)
      SDL2.set_draw_color(ren, *rgba)
      px1, py1 = _apply_point(x1, y1)
      px2, py2 = _apply_point(x2, y2)
      SDL2.render_draw_line_f(ren, px1, py1, px2, py2)
    end

    # draw_triangle(x1, y1, c1, x2, y2, c2, x3, y3, c3, z=0, mode=:default)
    def draw_triangle(x1, y1, c1, x2, y2, c2, x3, y3, c3, _z = 0, _mode = :default)
      ren = (_current_window && _current_window._sdl_renderer)
      return unless ren
      verts = FFI::MemoryPointer.new(:uint8, 20 * 3)
      [[x1, y1, c1], [x2, y2, c2], [x3, y3, c3]].each_with_index do |(px, py, col), i|
        tx, ty = _apply_point(px, py)
        r, g, b, a = _split_color(col)
        off = i * 20
        verts.put_float32(off,       tx)
        verts.put_float32(off + 4,   ty)
        verts.put_uint8(off + 8,   r)
        verts.put_uint8(off + 9,   g)
        verts.put_uint8(off + 10,  b)
        verts.put_uint8(off + 11,  a)
        verts.put_float32(off + 12,  0.0)
        verts.put_float32(off + 16,  0.0)
      end
      SDL2.render_geometry(ren, nil, verts, 3, nil, 0)
    end

    # draw_quad(x1, y1, c1, x2, y2, c2, x3, y3, c3, x4, y4, c4,
    #           z=0, mode=:default)  -- Gosu's vertex order is
    # top-left, top-right, bottom-left, bottom-right. Render as two
    # triangles (v0,v1,v2) and (v2,v1,v3).
    def draw_quad(x1, y1, c1, x2, y2, c2, x3, y3, c3, x4, y4, c4,
                  _z = 0, _mode = :default)
      ren = (_current_window && _current_window._sdl_renderer)
      return unless ren
      verts = FFI::MemoryPointer.new(:uint8, 20 * 4)
      [[x1, y1, c1], [x2, y2, c2], [x3, y3, c3], [x4, y4, c4]].each_with_index do |(px, py, col), i|
        tx, ty = _apply_point(px, py)
        r, g, b, a = _split_color(col)
        off = i * 20
        verts.put_float32(off,       tx)
        verts.put_float32(off + 4,   ty)
        verts.put_uint8(off + 8,   r)
        verts.put_uint8(off + 9,   g)
        verts.put_uint8(off + 10,  b)
        verts.put_uint8(off + 11,  a)
        verts.put_float32(off + 12,  0.0)
        verts.put_float32(off + 16,  0.0)
      end
      idx = FFI::MemoryPointer.new(:int32, 6)
      [0, 1, 2, 2, 1, 3].each_with_index { |v, i| idx.put_int32(i * 4, v) }
      SDL2.render_geometry(ren, nil, verts, 4, idx, 6)
    end

    def flush; end
    def record(_w, _h); yield if block_given?; nil; end

    def clip_to(x, y, w, h)
      ren = (_current_window && _current_window._sdl_renderer)
      if ren
        tx, ty = _apply_point(x, y)
        _, _, sx, sy, _ = _decompose
        rect = SDL2::Rect.new
        rect[:x] = tx.to_i
        rect[:y] = ty.to_i
        rect[:w] = (w * sx).to_i
        rect[:h] = (h * sy).to_i
        SDL2.render_set_clip_rect(ren, rect.pointer)
        begin
          yield if block_given?
        ensure
          SDL2.render_set_clip_rect(ren, nil)
        end
      else
        yield if block_given?
      end
    end

    def fps; 60; end
    def milliseconds
      SDL2.get_ticks
    rescue
      (Time.now.to_f * 1000).to_i
    end
    def random(a, b); a + rand * (b - a); end

    def available_width(_window = nil); 1920; end
    def available_height(_window = nil); 1080; end
    def screen_width(_window = nil); 1920; end
    def screen_height(_window = nil); 1080; end

    def button_down?(_id); false; end
    def button_name(_id); nil; end
    def button_id_to_char(_id); nil; end
    def char_to_button_id(_c); nil; end
    def axis(_id); 0.0; end

    def default_font_name; "DejaVu Sans"; end
    def user_languages; []; end

    def degrees_to_radians(d); d * Math::PI / 180.0; end
    def radians_to_degrees(r); r * 180.0 / Math::PI; end
    def offset_x(angle, distance)
      Math.sin(degrees_to_radians(angle)) * distance
    end
    def offset_y(angle, distance)
      -Math.cos(degrees_to_radians(angle)) * distance
    end
    def distance(x1, y1, x2, y2)
      Math.sqrt((x1 - x2)**2 + (y1 - y2)**2)
    end
    def angle(x1, y1, x2, y2)
      radians_to_degrees(Math.atan2(x2 - x1, y1 - y2)) % 360
    end
    def angle_diff(a, b)
      ((b - a + 180) % 360) - 180
    end

    def clipboard; ""; end
    def clipboard=(_); end
    def enable_undocumented_retrofication; end

    def disown_Window(_window); end
    def disown_TextInput(_input); end
  end
end

# Pure-Ruby helpers shipped with the gem (copied under gem/gosu/).
# They add deprecation helpers, Color constants, CamelCase aliases for
# the KB_*/MS_*/GP_* button constants, and Numeric angle-conversion
# helpers.
require "gosu/swig_patches"
require "gosu/patches"
require "gosu/compat"
