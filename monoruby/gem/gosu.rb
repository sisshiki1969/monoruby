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
  # Window -- stubbed game window. `show` is a no-op so programs that
  # just construct the window and call `show` exit immediately instead
  # of hanging.
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
    end

    def show; self; end
    def close; end
    def close!; end
    def tick; false; end

    def fullscreen?; @fullscreen; end
    def fullscreen=(v); @fullscreen = !!v; end
    def resizable?; @resizable; end
    def resizable=(v); @resizable = !!v; end
    def borderless?; @borderless; end
    def borderless=(v); @borderless = !!v; end

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
      if source.respond_to?(:columns) && source.respond_to?(:rows)
        @width  = @columns = source.columns.to_i
        @height = @rows    = source.rows.to_i
      else
        @width  = @columns = 0
        @height = @rows    = 0
      end
    end

    def self.from_text(_text, _height, *_opts); new; end
    def self.from_text_without_window(_text, _height, *_opts); new; end
    def self.from_markup(_text, _height, *_opts); new; end
    def self.load_tiles(_source, _tile_w, _tile_h, *_opts); []; end

    def draw(*); end
    def draw_rot(*); end
    def draw_as_quad(*); end
    def draw_mod(*); end
    def to_blob; "\0\0\0\0" * (width * height); end
    def save(_path); end
    def insert(_img, _x, _y); self; end
    def subimage(_x, _y, _w, _h); Image.new; end
    def gl_tex_info; nil; end
  end

  class Font
    attr_accessor :name, :height

    def initialize(height, *opts)
      @height = height.to_i
      @name = opts.first.is_a?(Hash) ? (opts.first[:name] || "") : ""
    end

    def draw_text(*); end
    def draw_text_rel(*); end
    def draw_markup(*); end
    def draw_markup_rel(*); end
    def text_width(text, *_opts); text.to_s.length * (@height / 2); end
    def markup_width(text, *_opts); text_width(text); end
    def set_image(_codepoint, _image); end
    def []=(*); end
  end

  class Sample
    def initialize(_path); end
    def play(_vol = 1.0, _speed = 1.0, _looping = false); Channel.new; end
    def play_pan(_pan = 0.0, _vol = 1.0, _speed = 1.0, _looping = false)
      Channel.new
    end
  end

  class Song
    def initialize(_path); end
    def self.current_song; nil; end
    def self.update; end
    def play(_looping = false); end
    def pause; end
    def paused?; false; end
    def stop; end
    def playing?; false; end
    def volume; 1.0; end
    def volume=(_); end
  end

  class Channel
    def current_channel; self; end
    def playing?; false; end
    def paused?; false; end
    def pause; end
    def resume; end
    def stop; end
    def volume; 1.0; end
    def volume=(_); end
    def pan; 0.0; end
    def pan=(_); end
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
  class << self
    def draw_line(*); end
    def draw_triangle(*); end
    def draw_quad(*); end
    def draw_rect(*); end
    def flush; end
    def record(_w, _h); yield if block_given?; nil; end
    def clip_to(*); yield if block_given?; end
    def translate(*); yield if block_given?; end
    def rotate(*); yield if block_given?; end
    def scale(*); yield if block_given?; end
    def transform(*); yield if block_given?; end

    def fps; 60; end
    def milliseconds; (Time.now.to_f * 1000).to_i; end
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
