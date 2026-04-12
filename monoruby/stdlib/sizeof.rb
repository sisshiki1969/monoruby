begin
  _result = `ruby -e 'require "rbconfig/sizeof"; p [RbConfig::SIZEOF, RbConfig::LIMITS]'`
  _sizeof, _limits = eval(_result)
  raise "failed" unless _sizeof
  RbConfig::SIZEOF = _sizeof
  RbConfig::LIMITS = _limits
rescue
  # Fallback for x86-64 Linux when ruby is not available in PATH
  RbConfig::SIZEOF = {
    "int" => 4, "short" => 2, "long" => 8, "long long" => 8,
    "__int128" => 16, "off_t" => 8, "void*" => 8, "float" => 4,
    "double" => 8, "time_t" => 8, "clock_t" => 8, "size_t" => 8,
    "ptrdiff_t" => 8, "dev_t" => 8,
    "int8_t" => 1, "uint8_t" => 1, "int16_t" => 2, "uint16_t" => 2,
    "int32_t" => 4, "uint32_t" => 4, "int64_t" => 8, "uint64_t" => 8,
    "int128_t" => 16, "uint128_t" => 16,
    "intptr_t" => 8, "uintptr_t" => 8, "ssize_t" => 8,
    "int_least8_t" => 1, "int_least16_t" => 2,
    "int_least32_t" => 4, "int_least64_t" => 8,
    "int_fast8_t" => 1, "int_fast16_t" => 8,
    "int_fast32_t" => 8, "int_fast64_t" => 8,
    "intmax_t" => 8, "sig_atomic_t" => 4,
    "wchar_t" => 4, "wint_t" => 4,
    "wctrans_t" => 8, "wctype_t" => 8,
    "_Bool" => 1, "long double" => 16,
    "float _Complex" => 8, "double _Complex" => 16,
    "long double _Complex" => 32,
    "__float128" => 16,
    "_Decimal32" => 4, "_Decimal64" => 8, "_Decimal128" => 16,
    "__float80" => 16,
  }
  RbConfig::LIMITS = {
    "CHAR_MIN" => -128, "CHAR_MAX" => 127,
    "UCHAR_MAX" => 255,
    "SHRT_MIN" => -32768, "SHRT_MAX" => 32767,
    "USHRT_MAX" => 65535,
    "INT_MIN" => -2147483648, "INT_MAX" => 2147483647,
    "UINT_MAX" => 4294967295,
    "LONG_MIN" => -9223372036854775808, "LONG_MAX" => 9223372036854775807,
    "ULONG_MAX" => 18446744073709551615,
    "LLONG_MIN" => -9223372036854775808, "LLONG_MAX" => 9223372036854775807,
    "ULLONG_MAX" => 18446744073709551615,
    "FIXNUM_MIN" => -4611686018427387904, "FIXNUM_MAX" => 4611686018427387903,
    "SIZE_MAX" => 18446744073709551615,
    "SSIZE_MAX" => 9223372036854775807,
    "INTPTR_MAX" => 9223372036854775807,
    "UINTPTR_MAX" => 18446744073709551615,
  }
end
