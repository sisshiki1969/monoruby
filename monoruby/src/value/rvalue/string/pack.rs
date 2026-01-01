use super::*;

#[derive(Debug)]
struct TemplateNode {
    template: Template,
    endian: Endianness,
    /// repeat times. None for repeat until end.
    repeat: Option<usize>,
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
    Utf8,
    Null,
    Back,
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
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "Currently, the template character is not supported.",
                ));
            }
        };
    }

    Ok(if once {
        if ary.is_empty() { Value::nil() } else { ary[0] }
    } else {
        Value::array_from_vec(ary)
    })
}

pub(crate) fn pack(store: &Store, ary: &[Value], template: &str) -> Result<Value> {
    let mut packed = Vec::new();
    let template = parse_template(template)?;
    let mut iter = ary.iter();

    macro_rules! pack {
        ($store:expr, $size: expr, $type: ident, $repeat: expr, $big_endian: expr) => {
            if let Some(repeat) = $repeat {
                for _ in 0..repeat {
                    if let Some(value) = iter.next() {
                        let i = value.coerce_to_i64($store)?;
                        let bytes = if $big_endian {
                            $type::to_be_bytes(i as $type)
                        } else {
                            $type::to_ne_bytes(i as $type)
                        };
                        packed.extend_from_slice(&bytes);
                    } else {
                        return Err(MonorubyErr::argumenterr("too few arguments"));
                    }
                }
            } else {
                while let Some(value) = iter.next() {
                    let i = value.coerce_to_i64($store)?;
                    let bytes = if $big_endian {
                        $type::to_be_bytes(i as $type)
                    } else {
                        $type::to_ne_bytes(i as $type)
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
            Template::I64 => pack!(store, 8, i64, repeat, endianness),
            Template::U64 => pack!(store, 8, u64, repeat, endianness),
            Template::I32 => pack!(store, 4, i32, repeat, endianness),
            Template::U32 => pack!(store, 4, u32, repeat, endianness),
            Template::I16 => pack!(store, 2, i16, repeat, endianness),
            Template::U16 => pack!(store, 2, u16, repeat, endianness),
            Template::I8 => pack!(store, 1, i8, repeat, endianness),
            Template::U8 => pack!(store, 1, u8, repeat, endianness),
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
                if packed.pop().is_none() {
                    return Err(MonorubyErr::argumenterr("X outside of string"));
                }
            }
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "Currently, the template character is not supported.",
                ));
            }
        };
    }
    Ok(Value::bytes(packed))
}

fn parse_template(template: &str) -> Result<Vec<TemplateNode>> {
    let mut temp = vec![];
    let mut iter = template.chars().peekable();
    while let Some(ch) = iter.next() {
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
            'l' | 'i' => (Template::I32, Endianness::Little),
            'L' | 'I' => (Template::U32, Endianness::Little),
            'q' => (Template::I64, Endianness::Little),
            'Q' => (Template::U64, Endianness::Little),
            'n' => (Template::U16, Endianness::Big),
            'N' => (Template::U32, Endianness::Big),
            'U' => (Template::Utf8, Endianness::Little),
            'x' => (Template::Null, Endianness::Little),
            'X' => (Template::Back, Endianness::Little),
            _ => {
                return Err(MonorubyErr::argumenterr(format!(
                    "String#pack/unpack Unknown template: {template}",
                )));
            }
        };
        if iter.next_if(|c| c == &'>').is_some() {
            endian = Endianness::Big;
        } else if iter.next_if(|c| c == &'<').is_some() {
            endian = Endianness::Little;
        }
        if iter.next_if(|c| c == &'*').is_some() {
            temp.push(TemplateNode {
                template,
                endian,
                repeat: None,
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
            });
        } else {
            temp.push(TemplateNode {
                template,
                endian,
                repeat: Some(1),
            });
        }
    }
    Ok(temp)
}
