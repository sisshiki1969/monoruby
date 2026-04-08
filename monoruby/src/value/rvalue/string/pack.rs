use super::*;

#[derive(Debug)]
struct TemplateNode {
    template: Template,
    endian: Endianness,
    /// repeat times. None for repeat until end.
    repeat: Option<usize>,
    /// whether a count was explicitly given (vs. default of 1)
    explicit_count: bool,
}

#[derive(Debug, Clone, Copy)]
enum Template {
    Ascii,
    AsciiTrim,
    CString,
    BitString,
    Hex,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Utf8,
    Null,
    Back,
    AtPos,
    Base64,
    QuotedPrintable,
    UuEncoded,
    BerCompressedInt,
    /// 'p' — pointer to null-terminated string
    Pointer,
    /// 'P' — pointer to fixed-length string
    PointerFixed,
}

#[derive(Debug, Clone, Copy)]
enum Endianness {
    Big,
    Little,
}

struct ByteIter<'a> {
    slice: &'a [u8],
    i: usize,
}

impl<'a> ByteIter<'a> {
    fn new(slice: &'a [u8]) -> Self {
        ByteIter { slice, i: 0 }
    }

    fn next(&mut self) -> Option<u8> {
        if self.i < self.slice.len() {
            let b = self.slice[self.i];
            self.i += 1;
            Some(b)
        } else {
            None
        }
    }

    fn is_empty(&self) -> bool {
        self.i >= self.slice.len()
    }

    fn remaining(&self) -> &'a [u8] {
        &self.slice[self.i..]
    }

    fn advance(&mut self, n: usize) {
        self.i = (self.i + n).min(self.slice.len());
    }

    fn take(&mut self, n: usize) -> &'a [u8] {
        let end = (self.i + n).min(self.slice.len());
        let slice = &self.slice[self.i..end];
        self.i = end;
        slice
    }

    fn back(&mut self) -> Option<()> {
        if self.i > 0 {
            self.i -= 1;
            Some(())
        } else {
            None
        }
    }

    fn next_chunk<const N: usize>(&mut self) -> Option<&[u8; N]> {
        if self.i + N <= self.slice.len() {
            let chunk: &[u8; N] =
                unsafe { &*(&self.slice[self.i..self.i + N] as *const _ as *const [u8; N]) };
            self.i += N;
            Some(chunk)
        } else {
            None
        }
    }
}

pub(crate) fn unpack(packed: &[u8], template: &str, once: bool) -> Result<Value> {
    let mut template = parse_template(template)?;
    if once {
        template.truncate(1);
    }

    let mut b = ByteIter::new(packed);
    let mut ary = Vec::new();

    macro_rules! unpack {
        ($size: expr, $type: ident, $repeat: expr, $big_endian: expr) => {
            if let Some(repeat) = $repeat {
                for _ in 0..repeat {
                    if let Some(chunk) = b.next_chunk::<$size>() {
                        let i = if $big_endian {
                            $type::from_be_bytes(chunk.clone())
                        } else {
                            $type::from_ne_bytes(chunk.clone())
                        };
                        ary.push(Value::integer(i as i64));
                    } else {
                        break;
                    }
                    if b.is_empty() {
                        break;
                    }
                }
            } else {
                while let Some(chunk) = b.next_chunk::<$size>() {
                    let i = if $big_endian {
                        $type::from_be_bytes(chunk.clone())
                    } else {
                        $type::from_ne_bytes(chunk.clone())
                    };
                    ary.push(Value::integer(i as i64));
                }
            }
        };
    }

    for template in template {
        let repeat = template.repeat;
        let endian = matches!(template.endian, Endianness::Big);
        match template.template {
            Template::I64 => unpack!(8, i64, repeat, endian),
            Template::U64 => unpack!(8, u64, repeat, endian),
            Template::I32 => unpack!(4, i32, repeat, endian),
            Template::U32 => unpack!(4, u32, repeat, endian),
            Template::I16 => unpack!(2, i16, repeat, endian),
            Template::U16 => unpack!(2, u16, repeat, endian),
            Template::I8 => unpack!(1, i8, repeat, endian),
            Template::U8 => unpack!(1, u8, repeat, endian),
            Template::F64 => {
                if let Some(repeat) = repeat {
                    for _ in 0..repeat {
                        if let Some(chunk) = b.next_chunk::<8>() {
                            let f = if endian {
                                f64::from_be_bytes(chunk.clone())
                            } else {
                                f64::from_le_bytes(chunk.clone())
                            };
                            ary.push(Value::float(f));
                        } else {
                            break;
                        }
                        if b.is_empty() {
                            break;
                        }
                    }
                } else {
                    while let Some(chunk) = b.next_chunk::<8>() {
                        let f = if endian {
                            f64::from_be_bytes(chunk.clone())
                        } else {
                            f64::from_le_bytes(chunk.clone())
                        };
                        ary.push(Value::float(f));
                    }
                }
            }
            Template::F32 => {
                if let Some(repeat) = repeat {
                    for _ in 0..repeat {
                        if let Some(chunk) = b.next_chunk::<4>() {
                            let f = if endian {
                                f32::from_be_bytes(chunk.clone()) as f64
                            } else {
                                f32::from_le_bytes(chunk.clone()) as f64
                            };
                            ary.push(Value::float(f));
                        } else {
                            break;
                        }
                        if b.is_empty() {
                            break;
                        }
                    }
                } else {
                    while let Some(chunk) = b.next_chunk::<4>() {
                        let f = if endian {
                            f32::from_be_bytes(chunk.clone()) as f64
                        } else {
                            f32::from_le_bytes(chunk.clone()) as f64
                        };
                        ary.push(Value::float(f));
                    }
                }
            }
            Template::Hex => {
                let mut s = String::new();
                if let Some(mut repeat) = repeat {
                    while repeat > 0 {
                        if let Some(chunk) = b.next() {
                            let u = if endian {
                                chunk
                            } else {
                                chunk << 4 | chunk >> 4
                            };
                            if repeat >= 2 {
                                s.push_str(&format!("{:02x}", u));
                                repeat -= 2;
                            } else if repeat == 1 {
                                s.push_str(&format!("{:x}", u >> 4));
                                repeat -= 1;
                            }
                        } else {
                            break;
                        }
                    }
                } else {
                    while let Some(chunk) = b.next() {
                        let u = if endian {
                            chunk
                        } else {
                            chunk << 4 | chunk >> 4
                        };
                        s.push_str(&format!("{:02x}", u));
                    }
                }
                ary.push(Value::string(s));
            }
            Template::Null => {
                if let Some(repeat) = repeat {
                    for _ in 0..repeat {
                        if b.next().is_none() {
                            return Err(MonorubyErr::argumenterr("x outside of string"));
                        }
                    }
                } else {
                    while b.next().is_some() {}
                }
            }
            Template::Back => {
                if let Some(repeat) = repeat {
                    for _ in 0..repeat {
                        if b.back().is_none() {
                            return Err(MonorubyErr::argumenterr("X outside of string"));
                        }
                    }
                } else {
                    while b.back().is_some() {}
                }
            }
            Template::AtPos => {
                // '@' — move to absolute position
                if let Some(pos) = repeat {
                    if pos > b.slice.len() {
                        return Err(MonorubyErr::argumenterr("@ outside of string"));
                    }
                    b.i = pos;
                } else {
                    // @* — move to end
                    b.i = b.slice.len();
                }
            }
            Template::Ascii => {
                // 'a' — arbitrary binary string (null padded on pack, raw bytes on unpack)
                if let Some(count) = repeat {
                    let bytes = b.take(count);
                    ary.push(Value::bytes(bytes.to_vec()));
                } else {
                    let bytes = b.remaining().to_vec();
                    b.advance(bytes.len());
                    ary.push(Value::bytes(bytes));
                }
            }
            Template::AsciiTrim => {
                // 'A' — arbitrary binary string (space padded on pack, trailing spaces/nulls stripped on unpack)
                if let Some(count) = repeat {
                    let bytes = b.take(count);
                    let mut end = bytes.len();
                    while end > 0 && (bytes[end - 1] == b' ' || bytes[end - 1] == 0) {
                        end -= 1;
                    }
                    ary.push(Value::bytes(bytes[..end].to_vec()));
                } else {
                    let bytes = b.remaining();
                    let mut end = bytes.len();
                    while end > 0 && (bytes[end - 1] == b' ' || bytes[end - 1] == 0) {
                        end -= 1;
                    }
                    let result = bytes[..end].to_vec();
                    b.advance(bytes.len());
                    ary.push(Value::bytes(result));
                }
            }
            Template::CString => {
                // 'Z' — null-terminated string (on unpack, strips after first null)
                if let Some(count) = repeat {
                    let bytes = b.take(count);
                    let end = bytes.iter().position(|&x| x == 0).unwrap_or(bytes.len());
                    ary.push(Value::bytes(bytes[..end].to_vec()));
                } else {
                    // Z* — read to end, strip after first null
                    let bytes = b.remaining();
                    let end = bytes.iter().position(|&x| x == 0).unwrap_or(bytes.len());
                    let result = bytes[..end].to_vec();
                    b.advance(bytes.len());
                    ary.push(Value::bytes(result));
                }
            }
            Template::Base64 => {
                // 'm' — Base64 decode
                let data = b.remaining().to_vec();
                b.advance(data.len());
                let decoded = base64_decode(&data);
                ary.push(Value::bytes(decoded));
            }
            Template::QuotedPrintable => {
                // 'M' — MIME quoted-printable decode
                let data = b.remaining().to_vec();
                b.advance(data.len());
                let decoded = qp_decode(&data);
                ary.push(Value::bytes(decoded));
            }
            Template::UuEncoded => {
                // 'u' — UU decode
                let data = b.remaining().to_vec();
                b.advance(data.len());
                let decoded = uu_decode(&data);
                ary.push(Value::bytes(decoded));
            }
            Template::BitString => {
                // 'b' (little-endian) / 'B' (big-endian) — bit string
                let big = endian;
                let mut s = String::new();
                if let Some(count) = repeat {
                    let mut bits_left = count;
                    while bits_left > 0 {
                        if let Some(byte) = b.next() {
                            let take = bits_left.min(8);
                            for bit_i in 0..take {
                                let bit = if big {
                                    // MSB first
                                    (byte >> (7 - bit_i)) & 1
                                } else {
                                    // LSB first
                                    (byte >> bit_i) & 1
                                };
                                s.push(if bit == 1 { '1' } else { '0' });
                            }
                            bits_left -= take;
                        } else {
                            break;
                        }
                    }
                } else {
                    while let Some(byte) = b.next() {
                        for bit_i in 0..8 {
                            let bit = if big {
                                (byte >> (7 - bit_i)) & 1
                            } else {
                                (byte >> bit_i) & 1
                            };
                            s.push(if bit == 1 { '1' } else { '0' });
                        }
                    }
                }
                ary.push(Value::string(s));
            }
            Template::Utf8 => {
                // 'U' — UTF-8 characters to codepoints
                if let Some(count) = repeat {
                    for _ in 0..count {
                        if b.is_empty() {
                            break;
                        }
                        let cp = utf8_decode_one(&mut b)?;
                        ary.push(Value::integer(cp as i64));
                    }
                } else {
                    while !b.is_empty() {
                        let cp = utf8_decode_one(&mut b)?;
                        ary.push(Value::integer(cp as i64));
                    }
                }
            }
            Template::BerCompressedInt => {
                // 'w' — BER compressed integer
                if let Some(count) = repeat {
                    for _ in 0..count {
                        if b.is_empty() {
                            break;
                        }
                        let val = ber_decode(&mut b)?;
                        ary.push(val);
                    }
                } else {
                    while !b.is_empty() {
                        let val = ber_decode(&mut b)?;
                        ary.push(val);
                    }
                }
            }
            Template::Pointer => {
                // 'p' — read a pointer (8 bytes on x86-64), dereference as null-terminated string
                let count = repeat.unwrap_or(1);
                for _ in 0..count {
                    if let Some(chunk) = b.next_chunk::<8>() {
                        let ptr = usize::from_ne_bytes(chunk.clone());
                        if ptr == 0 {
                            ary.push(Value::nil());
                        } else {
                            // SAFETY: We trust that the pointer was produced by a prior pack("p")
                            // call in this process, pointing to a valid null-terminated string.
                            let cstr = unsafe { std::ffi::CStr::from_ptr(ptr as *const i8) };
                            ary.push(Value::bytes(cstr.to_bytes().to_vec()));
                        }
                    } else {
                        break;
                    }
                }
            }
            Template::PointerFixed => {
                // 'P' — read a pointer (8 bytes on x86-64), return string of specified length
                let length = repeat.unwrap_or(1);
                if let Some(chunk) = b.next_chunk::<8>() {
                    let ptr = usize::from_ne_bytes(chunk.clone());
                    if ptr == 0 {
                        ary.push(Value::nil());
                    } else {
                        // SAFETY: We trust that the pointer was produced by a prior pack("P")
                        // call in this process, pointing to a valid buffer of at least `length` bytes.
                        let slice = unsafe { std::slice::from_raw_parts(ptr as *const u8, length) };
                        ary.push(Value::bytes(slice.to_vec()));
                    }
                }
            }
        };
    }

    Ok(if once {
        if ary.is_empty() { Value::nil() } else { ary[0] }
    } else {
        Value::array_from_vec(ary)
    })
}

pub(crate) fn pack(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: &[Value],
    template: &str,
    buffer: Option<Value>,
) -> Result<Value> {
    let template = parse_template(template)?;
    // Validate buffer: keyword if provided.
    if let Some(buf_val) = buffer {
        buf_val.ensure_not_frozen(&globals.store)?;
        if buf_val.is_rstring_inner().is_none() {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                buf_val,
                STRING_CLASS,
            ));
        }
    }
    let mut packed = Vec::new();
    let template_is_empty = template.is_empty();
    let mut iter = ary.iter();

    macro_rules! pack {
        ($size: expr, $type: ident, $repeat: expr, $big_endian: expr) => {
            if let Some(repeat) = $repeat {
                for _ in 0..repeat {
                    if let Some(value) = iter.next() {
                        let i = value.coerce_to_pack_u64(vm, globals)?;
                        let bytes = if $big_endian {
                            $type::to_be_bytes(i as $type)
                        } else {
                            $type::to_le_bytes(i as $type)
                        };
                        packed.extend_from_slice(&bytes);
                    } else {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                }
            } else {
                while let Some(value) = iter.next() {
                    let i = value.coerce_to_pack_u64(vm, globals)?;
                    let bytes = if $big_endian {
                        $type::to_be_bytes(i as $type)
                    } else {
                        $type::to_le_bytes(i as $type)
                    };
                    packed.extend_from_slice(&bytes);
                }
            }
        };
    }

    for template in template {
        let endianness = matches!(template.endian, Endianness::Big);
        let repeat = template.repeat;
        match template.template {
            Template::I64 => pack!(8, i64, repeat, endianness),
            Template::U64 => pack!(8, u64, repeat, endianness),
            Template::I32 => pack!(4, i32, repeat, endianness),
            Template::U32 => pack!(4, u32, repeat, endianness),
            Template::I16 => pack!(2, i16, repeat, endianness),
            Template::U16 => pack!(2, u16, repeat, endianness),
            Template::I8 => pack!(1, i8, repeat, endianness),
            Template::U8 => pack!(1, u8, repeat, endianness),
            Template::F64 => {
                let mut process = |repeat: Option<usize>| -> Result<()> {
                    if let Some(repeat) = repeat {
                        for _ in 0..repeat {
                            if let Some(value) = iter.next() {
                                let f = value.coerce_to_f64_no_convert(&globals.store)?;
                                let bytes = if endianness {
                                    f64::to_be_bytes(f)
                                } else {
                                    f64::to_le_bytes(f)
                                };
                                packed.extend_from_slice(&bytes);
                            } else {
                                return Err(MonorubyErr::argumenterr("too few arguments"));
                            }
                        }
                    } else {
                        while let Some(value) = iter.next() {
                            let f = value.coerce_to_f64_no_convert(&globals.store)?;
                            let bytes = if endianness {
                                f64::to_be_bytes(f)
                            } else {
                                f64::to_le_bytes(f)
                            };
                            packed.extend_from_slice(&bytes);
                        }
                    }
                    Ok(())
                };
                process(repeat)?;
            }
            Template::F32 => {
                let mut process = |repeat: Option<usize>| -> Result<()> {
                    if let Some(repeat) = repeat {
                        for _ in 0..repeat {
                            if let Some(value) = iter.next() {
                                let f = value.coerce_to_f64_no_convert(&globals.store)? as f32;
                                let bytes = if endianness {
                                    f32::to_be_bytes(f)
                                } else {
                                    f32::to_le_bytes(f)
                                };
                                packed.extend_from_slice(&bytes);
                            } else {
                                return Err(MonorubyErr::argumenterr("too few arguments"));
                            }
                        }
                    } else {
                        while let Some(value) = iter.next() {
                            let f = value.coerce_to_f64_no_convert(&globals.store)? as f32;
                            let bytes = if endianness {
                                f32::to_be_bytes(f)
                            } else {
                                f32::to_le_bytes(f)
                            };
                            packed.extend_from_slice(&bytes);
                        }
                    }
                    Ok(())
                };
                process(repeat)?;
            }
            Template::Null => {
                if let Some(repeat) = repeat {
                    for _ in 0..repeat {
                        packed.push(0);
                    }
                } else {
                    // do nothing
                }
            }
            Template::Back => {
                let count = repeat.unwrap_or(1);
                for _ in 0..count {
                    if packed.pop().is_none() {
                        return Err(MonorubyErr::argumenterr("X outside of string"));
                    }
                }
            }
            Template::AtPos => {
                // '@' — move to absolute position, padding with nulls if needed
                if let Some(pos) = repeat {
                    if pos > packed.len() {
                        packed.resize(pos, 0);
                    } else {
                        packed.truncate(pos);
                    }
                }
                // @* does nothing meaningful for pack
            }
            Template::Hex => {
                // 'h' (low nibble first) / 'H' (high nibble first) — hex string
                let high = endianness; // Big = H (high nibble first)
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    let count = if let Some(count) = repeat {
                        count
                    } else {
                        s.len()
                    };
                    let mut byte: u8 = 0;
                    let mut nibble_i = 0;
                    for i in 0..count {
                        let nibble = if i < s.len() {
                            match s[i] {
                                b'0'..=b'9' => s[i] - b'0',
                                b'a'..=b'f' => s[i] - b'a' + 10,
                                b'A'..=b'F' => s[i] - b'A' + 10,
                                _ => 0,
                            }
                        } else {
                            0
                        };
                        if high {
                            if nibble_i == 0 {
                                byte = nibble << 4;
                            } else {
                                byte |= nibble;
                            }
                        } else {
                            if nibble_i == 0 {
                                byte = nibble;
                            } else {
                                byte |= nibble << 4;
                            }
                        }
                        nibble_i += 1;
                        if nibble_i == 2 {
                            packed.push(byte);
                            byte = 0;
                            nibble_i = 0;
                        }
                    }
                    if nibble_i > 0 {
                        packed.push(byte);
                    }
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
            Template::Ascii => {
                // 'a' — arbitrary binary string (null padded)
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    if let Some(count) = repeat {
                        if s.len() >= count {
                            packed.extend_from_slice(&s[..count]);
                        } else {
                            packed.extend_from_slice(&s);
                            packed.resize(packed.len() + count - s.len(), 0);
                        }
                    } else {
                        // a* — pack entire string
                        packed.extend_from_slice(&s);
                    }
                } else {
                    if let Some(count) = repeat {
                        packed.resize(packed.len() + count, 0);
                    }
                }
            }
            Template::AsciiTrim => {
                // 'A' — arbitrary binary string (space padded)
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    if let Some(count) = repeat {
                        if s.len() >= count {
                            packed.extend_from_slice(&s[..count]);
                        } else {
                            packed.extend_from_slice(&s);
                            packed.resize(packed.len() + count - s.len(), b' ');
                        }
                    } else {
                        // A* — pack entire string
                        packed.extend_from_slice(&s);
                    }
                } else {
                    if let Some(count) = repeat {
                        packed.resize(packed.len() + count, b' ');
                    }
                }
            }
            Template::CString => {
                // 'Z' — null-terminated string
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    if let Some(count) = repeat {
                        // Zn — like 'a' but null-padded (same as 'a')
                        if s.len() >= count {
                            packed.extend_from_slice(&s[..count]);
                        } else {
                            packed.extend_from_slice(&s);
                            packed.resize(packed.len() + count - s.len(), 0);
                        }
                    } else {
                        // Z* — string + null terminator
                        packed.extend_from_slice(&s);
                        packed.push(0);
                    }
                } else {
                    if let Some(count) = repeat {
                        packed.resize(packed.len() + count, 0);
                    } else {
                        packed.push(0);
                    }
                }
            }
            Template::Base64 => {
                // 'm' — Base64 encode
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    let line_len = if template.explicit_count {
                        repeat.unwrap_or(45)
                    } else {
                        45
                    };
                    let encoded = base64_encode(&s, line_len);
                    packed.extend_from_slice(encoded.as_bytes());
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
            Template::QuotedPrintable => {
                // 'M' — MIME quoted-printable encode
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    let line_len = if template.explicit_count {
                        repeat.unwrap_or(72)
                    } else {
                        72
                    };
                    let encoded = qp_encode(&s, line_len);
                    packed.extend_from_slice(encoded.as_bytes());
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
            Template::UuEncoded => {
                // 'u' — UU encode
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    let line_len = if template.explicit_count {
                        repeat.unwrap_or(45)
                    } else {
                        45
                    };
                    let encoded = uu_encode(&s, line_len);
                    packed.extend_from_slice(encoded.as_bytes());
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
            Template::BitString => {
                // 'b' (little-endian) / 'B' (big-endian) — bit string
                let big = endianness;
                if let Some(value) = iter.next() {
                    let s = get_pack_string(vm, globals, *value)?;
                    let count = if let Some(count) = repeat {
                        count
                    } else {
                        s.len()
                    };
                    let mut byte: u8 = 0;
                    let mut bit_i = 0;
                    for i in 0..count {
                        let bit = if i < s.len() && s[i] == b'1' {
                            1u8
                        } else {
                            0u8
                        };
                        if big {
                            byte |= bit << (7 - bit_i);
                        } else {
                            byte |= bit << bit_i;
                        }
                        bit_i += 1;
                        if bit_i == 8 {
                            packed.push(byte);
                            byte = 0;
                            bit_i = 0;
                        }
                    }
                    if bit_i > 0 {
                        packed.push(byte);
                    }
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
            Template::Utf8 => {
                // 'U' — UTF-8 characters from codepoints
                if let Some(count) = repeat {
                    for _ in 0..count {
                        if let Some(value) = iter.next() {
                            let i = value.coerce_to_int_i64(vm, globals)?;
                            if i < 0 {
                                return Err(MonorubyErr::argumenterr(
                                    "pack(U): value out of range",
                                ));
                            }
                            utf8_encode_one(&mut packed, i as u32);
                        } else {
                            return Err(MonorubyErr::argumenterr("too few arguments"));
                        }
                    }
                } else {
                    while let Some(value) = iter.next() {
                        let i = value.coerce_to_int_i64(vm, globals)?;
                        if i < 0 {
                            return Err(MonorubyErr::argumenterr("pack(U): value out of range"));
                        }
                        utf8_encode_one(&mut packed, i as u32);
                    }
                }
            }
            Template::BerCompressedInt => {
                // 'w' — BER compressed integer
                if let Some(count) = repeat {
                    for _ in 0..count {
                        if let Some(value) = iter.next() {
                            let i = value.coerce_to_int_i64(vm, globals)?;
                            if i < 0 {
                                return Err(MonorubyErr::argumenterr(
                                    "can't compress negative numbers",
                                ));
                            }
                            ber_encode(&mut packed, i as u64);
                        } else {
                            return Err(MonorubyErr::argumenterr("too few arguments"));
                        }
                    }
                } else {
                    while let Some(value) = iter.next() {
                        let i = value.coerce_to_int_i64(vm, globals)?;
                        if i < 0 {
                            return Err(MonorubyErr::argumenterr(
                                "can't compress negative numbers",
                            ));
                        }
                        ber_encode(&mut packed, i as u64);
                    }
                }
            }
            Template::Pointer => {
                // 'p' — store a pointer to a null-terminated copy of the string
                let count = repeat.unwrap_or(1);
                for _ in 0..count {
                    if let Some(value) = iter.next() {
                        if value.is_nil() {
                            packed.extend_from_slice(&0u64.to_ne_bytes());
                        } else {
                            let s = get_pack_string(vm, globals, *value)?;
                            // Allocate a null-terminated copy using CString.
                            // We leak the memory so the pointer remains valid.
                            let cstring = std::ffi::CString::new(s).map_err(|_| {
                                MonorubyErr::argumenterr("string contains null byte for pack('p')")
                            })?;
                            let ptr = cstring.into_raw() as u64;
                            packed.extend_from_slice(&ptr.to_ne_bytes());
                        }
                    } else {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                }
            }
            Template::PointerFixed => {
                // 'P' — store a pointer to the string data
                // The count specifies the length of data pointed to, not the repeat count.
                if let Some(value) = iter.next() {
                    if value.is_nil() {
                        packed.extend_from_slice(&0u64.to_ne_bytes());
                    } else {
                        let s = get_pack_string(vm, globals, *value)?;
                        // Leak a copy of the bytes so the pointer stays valid.
                        let boxed: Box<[u8]> = s.into_boxed_slice();
                        let ptr = Box::into_raw(boxed) as *const u8 as u64;
                        packed.extend_from_slice(&ptr.to_ne_bytes());
                    }
                } else {
                    return Err(MonorubyErr::argumenterr("too few arguments"));
                }
            }
        };
    }
    if let Some(mut buf_val) = buffer {
        // Write the packed data into the provided buffer string.
        let rstr = buf_val.as_rstring_inner_mut();
        *rstr = RStringInner::bytes_from_vec(packed);
        Ok(buf_val)
    } else if template_is_empty && packed.is_empty() {
        // Empty format string produces US-ASCII encoded empty string.
        Ok(Value::string_from_inner(RStringInner::from_encoding(
            b"",
            Encoding::UsAscii,
        )))
    } else {
        Ok(Value::bytes(packed))
    }
}

fn parse_template(template: &str) -> Result<Vec<TemplateNode>> {
    let mut temp = vec![];
    let mut iter = template.chars().peekable();
    while let Some(ch) = iter.next() {
        // Skip whitespace between directives (space, tab, newline, vertical tab, form feed, carriage return)
        if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\x0B' || ch == '\x0C' || ch == '\r' {
            continue;
        }
        // '#' starts a comment until end of line
        if ch == '#' {
            while let Some(&c) = iter.peek() {
                iter.next();
                if c == '\n' {
                    break;
                }
            }
            continue;
        }
        let (template, mut endian) = match ch {
            'a' => (Template::Ascii, Endianness::Little),
            'A' => (Template::AsciiTrim, Endianness::Little),
            'Z' => (Template::CString, Endianness::Little),
            'b' => (Template::BitString, Endianness::Little),
            'B' => (Template::BitString, Endianness::Big),
            'h' => (Template::Hex, Endianness::Little),
            'H' => (Template::Hex, Endianness::Big),
            'c' => (Template::I8, Endianness::Little),
            'C' => (Template::U8, Endianness::Little),
            's' => (Template::I16, Endianness::Little),
            'S' => (Template::U16, Endianness::Little),
            'i' => (Template::I32, Endianness::Little),
            'I' => (Template::U32, Endianness::Little),
            'l' => (Template::I32, Endianness::Little),
            'L' => (Template::U32, Endianness::Little),
            'q' => (Template::I64, Endianness::Little),
            'Q' => (Template::U64, Endianness::Little),
            'j' => (Template::I64, Endianness::Little), // intptr_t = i64 on x86-64
            'J' => (Template::U64, Endianness::Little), // uintptr_t = u64 on x86-64
            'v' => (Template::U16, Endianness::Little),
            'V' => (Template::U32, Endianness::Little),
            'n' => (Template::U16, Endianness::Big),
            'N' => (Template::U32, Endianness::Big),
            'd' | 'D' => (Template::F64, Endianness::Little),
            'f' | 'F' => (Template::F32, Endianness::Little),
            'e' => (Template::F32, Endianness::Little),
            'E' => (Template::F64, Endianness::Little),
            'g' => (Template::F32, Endianness::Big),
            'G' => (Template::F64, Endianness::Big),
            'U' => (Template::Utf8, Endianness::Little),
            'x' => (Template::Null, Endianness::Little),
            'X' => (Template::Back, Endianness::Little),
            '@' => (Template::AtPos, Endianness::Little),
            'm' => (Template::Base64, Endianness::Little),
            'M' => (Template::QuotedPrintable, Endianness::Little),
            'u' => (Template::UuEncoded, Endianness::Little),
            'w' => (Template::BerCompressedInt, Endianness::Little),
            'p' => (Template::Pointer, Endianness::Little),
            'P' => (Template::PointerFixed, Endianness::Little),
            _ => {
                return Err(MonorubyErr::argumenterr(format!(
                    "unknown pack directive '{ch}' in '{template}'",
                )));
            }
        };
        // Parse modifiers: '!' or '_' (native size), '>' (big-endian), '<' (little-endian).
        // These can appear in any order before the count.
        let mut native_modifier = None;
        let mut endian_modifier = None;
        loop {
            if let Some(&c) = iter.peek() {
                if c == '!' || c == '_' {
                    if native_modifier.is_none() {
                        native_modifier = Some(c);
                    }
                    iter.next();
                } else if c == '>' {
                    endian = Endianness::Big;
                    endian_modifier = Some(c);
                    iter.next();
                } else if c == '<' {
                    endian = Endianness::Little;
                    endian_modifier = Some(c);
                    iter.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        let has_native = native_modifier.is_some();
        // '!' and '_' modifiers are only valid for s/S/i/I/l/L/q/Q/j/J
        if let Some(modifier) = native_modifier {
            match ch {
                's' | 'S' | 'i' | 'I' | 'l' | 'L' | 'q' | 'Q' | 'j' | 'J' => {}
                _ => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "'{modifier}' allowed only after types sSiIlLqQjJ"
                    )));
                }
            }
        }
        // '<' and '>' endian modifiers are only valid for integer/float formats.
        if let Some(modifier) = endian_modifier {
            match ch {
                's' | 'S' | 'i' | 'I' | 'l' | 'L' | 'q' | 'Q' | 'j' | 'J' | 'n' | 'N'
                | 'v' | 'V' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' => {}
                _ => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "'{modifier}' allowed only after types sSiIlLqQjJ"
                    )));
                }
            }
        }
        // Apply native size modifier on x86-64 Linux:
        //   s!/S! = short = 2 bytes (same as default)
        //   i!/I! = int = 4 bytes (same as default)
        //   l!/L! = long = 8 bytes (upgrade from 4 to 8)
        //   q!/Q!/j!/J! = already 8 bytes
        let template = if has_native && ch == 'l' {
            Template::I64
        } else if has_native && ch == 'L' {
            Template::U64
        } else {
            template
        };
        if iter.next_if(|c| c == &'*').is_some() {
            temp.push(TemplateNode {
                template,
                endian,
                repeat: None,
                explicit_count: true,
            });
        } else if let Some(d1) = iter.next_if(|c| c.is_ascii_digit()) {
            let mut count = (d1 as u32 - '0' as u32) as usize;
            while let Some(d) = iter.next_if(|c| c.is_ascii_digit()) {
                count = count * 10 + (d as u32 - '0' as u32) as usize;
            }
            temp.push(TemplateNode {
                template,
                endian,
                repeat: Some(count),
                explicit_count: true,
            });
        } else {
            temp.push(TemplateNode {
                template,
                endian,
                repeat: Some(1),
                explicit_count: false,
            });
        }
    }
    Ok(temp)
}

/// Get bytes from a Value for pack string templates (a/A/Z/m/M/u).
/// Calls `#to_str` if the value is not already a String.
fn get_pack_string(
    vm: &mut Executor,
    globals: &mut Globals,
    value: Value,
) -> Result<Vec<u8>> {
    if let Some(s) = value.is_rstring_inner() {
        Ok(s.as_bytes().to_vec())
    } else {
        let rstr = value.coerce_to_rstring(vm, globals)?;
        Ok(rstr.as_bytes().to_vec())
    }
}

// --- Base64 encoding/decoding ---

const BASE64_CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn base64_encode(data: &[u8], line_len: usize) -> String {
    let mut result = String::new();
    let mut col = 0;
    // line_len is the number of input bytes per line (default 45 -> 60 output chars)
    let chars_per_line = if line_len == 0 {
        0
    } else {
        (line_len / 3) * 4 + if line_len % 3 != 0 { 4 } else { 0 }
    };

    for chunk in data.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = if chunk.len() > 1 { chunk[1] as u32 } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] as u32 } else { 0 };
        let triple = (b0 << 16) | (b1 << 8) | b2;

        result.push(BASE64_CHARS[((triple >> 18) & 0x3F) as usize] as char);
        result.push(BASE64_CHARS[((triple >> 12) & 0x3F) as usize] as char);
        if chunk.len() > 1 {
            result.push(BASE64_CHARS[((triple >> 6) & 0x3F) as usize] as char);
        } else {
            result.push('=');
        }
        if chunk.len() > 2 {
            result.push(BASE64_CHARS[(triple & 0x3F) as usize] as char);
        } else {
            result.push('=');
        }
        col += 4;

        if chars_per_line > 0 && col >= chars_per_line {
            result.push('\n');
            col = 0;
        }
    }

    // If line_len > 0 and we haven't ended with a newline, add one
    if chars_per_line > 0 && col > 0 {
        result.push('\n');
    }
    // If data is empty, no output

    result
}

fn base64_decode_char(c: u8) -> Option<u8> {
    match c {
        b'A'..=b'Z' => Some(c - b'A'),
        b'a'..=b'z' => Some(c - b'a' + 26),
        b'0'..=b'9' => Some(c - b'0' + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        _ => None,
    }
}

fn base64_decode(data: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut buf = [0u8; 4];
    let mut buf_len = 0;
    let mut pad_count = 0;

    for &byte in data {
        if byte == b'=' {
            buf[buf_len] = 0;
            buf_len += 1;
            pad_count += 1;
        } else if let Some(val) = base64_decode_char(byte) {
            buf[buf_len] = val;
            buf_len += 1;
            pad_count = 0;
        } else {
            // skip whitespace and other chars
            continue;
        }

        if buf_len == 4 {
            let triple = (buf[0] as u32) << 18
                | (buf[1] as u32) << 12
                | (buf[2] as u32) << 6
                | buf[3] as u32;
            result.push((triple >> 16) as u8);
            if pad_count < 2 {
                result.push((triple >> 8) as u8);
            }
            if pad_count < 1 {
                result.push(triple as u8);
            }
            buf_len = 0;
            pad_count = 0;
        }
    }

    result
}

// --- MIME Quoted-Printable encoding/decoding ---

fn qp_encode(data: &[u8], line_len: usize) -> String {
    let mut result = String::new();
    let mut col = 0;
    let max_col = if line_len > 0 {
        line_len - 1
    } else {
        usize::MAX
    };

    for &byte in data {
        let encoded =
            if byte == b'\t' || byte == b' ' || (byte >= 33 && byte <= 126 && byte != b'=') {
                format!("{}", byte as char)
            } else if byte == b'\n' {
                "\n".to_string()
            } else {
                format!("={:02X}", byte)
            };

        if byte != b'\n' && col + encoded.len() > max_col {
            result.push('=');
            result.push('\n');
            col = 0;
        }

        result.push_str(&encoded);
        if byte == b'\n' {
            col = 0;
        } else {
            col += encoded.len();
        }
    }

    // Always end with soft line break if not already ending with newline
    if !result.ends_with('\n') {
        result.push('=');
        result.push('\n');
    }

    result
}

fn qp_decode(data: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < data.len() {
        if data[i] == b'=' {
            if i + 2 < data.len() {
                if data[i + 1] == b'\n'
                    || (data[i + 1] == b'\r' && i + 2 < data.len() && data[i + 2] == b'\n')
                {
                    // soft line break
                    if data[i + 1] == b'\r' {
                        i += 3;
                    } else {
                        i += 2;
                    }
                    continue;
                }
                let hi = hex_val(data[i + 1]);
                let lo = hex_val(data[i + 2]);
                if let (Some(h), Some(l)) = (hi, lo) {
                    result.push((h << 4) | l);
                    i += 3;
                    continue;
                }
            } else if i + 1 < data.len() && data[i + 1] == b'\n' {
                i += 2;
                continue;
            }
            result.push(data[i]);
            i += 1;
        } else {
            result.push(data[i]);
            i += 1;
        }
    }
    result
}

fn hex_val(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'A'..=b'F' => Some(c - b'A' + 10),
        b'a'..=b'f' => Some(c - b'a' + 10),
        _ => None,
    }
}

// --- UU encoding/decoding ---

fn uu_encode(data: &[u8], line_len: usize) -> String {
    let mut result = String::new();
    let bytes_per_line = if line_len == 0 { 45 } else { line_len.min(45) };

    for chunk in data.chunks(bytes_per_line) {
        // length byte
        result.push((chunk.len() as u8 + 32) as char);

        for triple in chunk.chunks(3) {
            let b0 = triple[0] as u32;
            let b1 = if triple.len() > 1 {
                triple[1] as u32
            } else {
                0
            };
            let b2 = if triple.len() > 2 {
                triple[2] as u32
            } else {
                0
            };
            let val = (b0 << 16) | (b1 << 8) | b2;

            result.push(uu_char((val >> 18) & 0x3F));
            result.push(uu_char((val >> 12) & 0x3F));
            result.push(uu_char((val >> 6) & 0x3F));
            result.push(uu_char(val & 0x3F));
        }

        result.push('\n');
    }

    result
}

fn uu_char(val: u32) -> char {
    if val == 0 {
        '`'
    } else {
        (val as u8 + 32) as char
    }
}

fn uu_decode(data: &[u8]) -> Vec<u8> {
    let mut result = Vec::new();

    for line in data.split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }
        let len_byte = line[0];
        let expected_len = if len_byte >= 32 {
            (len_byte - 32) as usize
        } else {
            continue;
        };
        if expected_len == 0 {
            continue;
        }

        let encoded = &line[1..];
        let mut decoded_line = Vec::new();
        let mut i = 0;

        while i + 3 < encoded.len() {
            let c0 = uu_decode_char(encoded[i]);
            let c1 = uu_decode_char(encoded[i + 1]);
            let c2 = uu_decode_char(encoded[i + 2]);
            let c3 = uu_decode_char(encoded[i + 3]);
            let val = (c0 << 18) | (c1 << 12) | (c2 << 6) | c3;
            decoded_line.push((val >> 16) as u8);
            decoded_line.push((val >> 8) as u8);
            decoded_line.push(val as u8);
            i += 4;
        }

        decoded_line.truncate(expected_len);
        result.extend_from_slice(&decoded_line);
    }

    result
}

fn uu_decode_char(c: u8) -> u32 {
    if c == b'`' {
        0
    } else if c >= 32 {
        (c - 32) as u32 & 0x3F
    } else {
        0
    }
}

// --- BER compressed integer encoding/decoding ---

fn ber_encode(buf: &mut Vec<u8>, mut val: u64) {
    if val == 0 {
        buf.push(0);
        return;
    }
    // Encode in reverse, then push
    let mut tmp = Vec::new();
    tmp.push((val & 0x7F) as u8);
    val >>= 7;
    while val > 0 {
        tmp.push((val & 0x7F) as u8 | 0x80);
        val >>= 7;
    }
    tmp.reverse();
    buf.extend_from_slice(&tmp);
}

// --- UTF-8 encoding/decoding ---

fn utf8_encode_one(buf: &mut Vec<u8>, cp: u32) {
    if cp <= 0x7F {
        buf.push(cp as u8);
    } else if cp <= 0x7FF {
        buf.push((0xC0 | (cp >> 6)) as u8);
        buf.push((0x80 | (cp & 0x3F)) as u8);
    } else if cp <= 0xFFFF {
        buf.push((0xE0 | (cp >> 12)) as u8);
        buf.push((0x80 | ((cp >> 6) & 0x3F)) as u8);
        buf.push((0x80 | (cp & 0x3F)) as u8);
    } else if cp <= 0x10FFFF {
        buf.push((0xF0 | (cp >> 18)) as u8);
        buf.push((0x80 | ((cp >> 12) & 0x3F)) as u8);
        buf.push((0x80 | ((cp >> 6) & 0x3F)) as u8);
        buf.push((0x80 | (cp & 0x3F)) as u8);
    }
}

fn utf8_decode_one(b: &mut ByteIter) -> Result<u32> {
    let first = b
        .next()
        .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
    if first & 0x80 == 0 {
        Ok(first as u32)
    } else if first & 0xE0 == 0xC0 {
        let b1 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        Ok(((first as u32 & 0x1F) << 6) | (b1 as u32 & 0x3F))
    } else if first & 0xF0 == 0xE0 {
        let b1 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        let b2 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        Ok(((first as u32 & 0x0F) << 12) | ((b1 as u32 & 0x3F) << 6) | (b2 as u32 & 0x3F))
    } else if first & 0xF8 == 0xF0 {
        let b1 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        let b2 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        let b3 = b
            .next()
            .ok_or_else(|| MonorubyErr::argumenterr("malformed UTF-8 character"))?;
        Ok(((first as u32 & 0x07) << 18)
            | ((b1 as u32 & 0x3F) << 12)
            | ((b2 as u32 & 0x3F) << 6)
            | (b3 as u32 & 0x3F))
    } else {
        Err(MonorubyErr::argumenterr("malformed UTF-8 character"))
    }
}

fn ber_decode(b: &mut ByteIter) -> Result<Value> {
    let mut val: u64 = 0;
    loop {
        if let Some(byte) = b.next() {
            val = (val << 7) | (byte & 0x7F) as u64;
            if byte & 0x80 == 0 {
                return Ok(Value::integer(val as i64));
            }
        } else {
            return Err(MonorubyErr::argumenterr("malformed BER compressed integer"));
        }
    }
}
