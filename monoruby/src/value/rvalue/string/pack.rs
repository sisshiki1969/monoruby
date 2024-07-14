use super::*;

#[derive(Debug)]
struct TemplateNode {
    template: Template,
    endless: bool,
}

#[derive(Debug, Clone, Copy)]
enum Template {
    Ascii,
    AsciiTrim,
    CString,
    BitString,
    BitStringBigEndian,
    Hex,
    HexBigEndian,
    I8,
    U8,
    I16,
    U16,
    U16BigEndian,
    I32,
    U32,
    U32BigEndian,
    I64,
    U64,
    Utf8,
    Null,
    Back,
}

pub(crate) fn unpack1(packed: &[u8], template: &str) -> Result<Value> {
    let b = packed.iter();

    macro_rules! unpack {
        ($size: expr, $type: ident, $big_endian: expr) => {
            for chunk in b.array_chunks::<$size>() {
                let bytes = chunk.map(|e| *e);
                let i = if $big_endian {
                    $type::from_be_bytes(bytes)
                } else {
                    $type::from_ne_bytes(bytes)
                };
                return Ok(Value::integer(i as i64));
            }
        };
    }

    match parse_template(template)[0].template {
        Template::I64 => unpack!(8, i64, false),
        Template::U64 => unpack!(8, u64, false),
        Template::I32 => unpack!(4, i32, false),
        Template::U32 => unpack!(4, u32, false),
        Template::U32BigEndian => unpack!(4, u32, true),
        Template::I16 => unpack!(2, i16, false),
        Template::U16 => unpack!(2, u16, false),
        Template::U16BigEndian => unpack!(2, u16, true),
        Template::I8 => unpack!(1, i8, false),
        Template::U8 => unpack!(1, u8, false),
        Template::Null => {}
        _ => {
            return Err(MonorubyErr::argumenterr(
                "Currently, the template character is not supported.",
            ))
        }
    };
    Ok(Value::nil())
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
        if dbg!(self.i) < self.slice.len() {
            let b = self.slice[self.i];
            self.i += 1;
            Some(b)
        } else {
            None
        }
    }

    fn back(&mut self) -> Option<()> {
        if dbg!(self.i) > 0 {
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

pub(crate) fn unpack(packed: &[u8], template: &str) -> Result<Value> {
    let mut b = ByteIter::new(packed);
    let mut ary = Vec::new();
    let template = parse_template(template);

    macro_rules! unpack {
        ($size: expr, $type: ident, $endless: expr, $big_endian: expr) => {
            if $endless {
                while let Some(chunk) = b.next_chunk::<$size>() {
                    let i = if $big_endian {
                        $type::from_be_bytes(chunk.clone())
                    } else {
                        $type::from_ne_bytes(chunk.clone())
                    };
                    ary.push(Value::integer(i as i64));
                }
            } else {
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
            }
        };
    }

    for template in template {
        let endless = template.endless;
        match template.template {
            Template::I64 => unpack!(8, i64, endless, false),
            Template::U64 => unpack!(8, u64, endless, false),
            Template::I32 => unpack!(4, i32, endless, false),
            Template::U32 => unpack!(4, u32, endless, false),
            Template::U32BigEndian => unpack!(4, u32, endless, true),
            Template::I16 => unpack!(2, i16, endless, false),
            Template::U16 => unpack!(2, u16, endless, false),
            Template::U16BigEndian => unpack!(2, u16, endless, true),
            Template::I8 => unpack!(1, i8, endless, false),
            Template::U8 => unpack!(1, u8, endless, false),
            Template::Null => {
                if endless {
                    while b.next().is_some() {}
                } else if b.next().is_none() {
                    return Err(MonorubyErr::argumenterr("x outside of string"));
                }
            }
            Template::Back => {
                if endless {
                    while b.back().is_some() {}
                } else {
                    b.back();
                }
            }
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "Currently, the template character is not supported.",
                ))
            }
        };
    }
    Ok(Value::array_from_vec(ary))
}

pub(crate) fn pack(ary: &[Value], template: &str) -> Result<Value> {
    let mut packed = Vec::new();
    let template = parse_template(template);
    let mut iter = ary.iter();

    macro_rules! pack {
        ($size: expr, $type: ident, $endless: expr, $big_endian: expr) => {
            if $endless {
                while let Some(value) = iter.next() {
                    let i = value.coerce_to_i64()?;
                    let bytes = if $big_endian {
                        $type::to_be_bytes(i as $type)
                    } else {
                        $type::to_ne_bytes(i as $type)
                    };
                    packed.extend_from_slice(&bytes);
                }
            } else {
                if let Some(value) = iter.next() {
                    let i = value.coerce_to_i64()?;
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
        };
    }

    for template in template {
        match template.template {
            Template::I64 => pack!(8, i64, template.endless, false),
            Template::U64 => pack!(8, u64, template.endless, false),
            Template::I32 => pack!(4, i32, template.endless, false),
            Template::U32 => pack!(4, u32, template.endless, false),
            Template::U32BigEndian => pack!(4, u32, template.endless, true),
            Template::I16 => pack!(2, i16, template.endless, false),
            Template::U16 => pack!(2, u16, template.endless, false),
            Template::U16BigEndian => pack!(2, u16, template.endless, true),
            Template::I8 => pack!(1, i8, template.endless, false),
            Template::U8 => pack!(1, u8, template.endless, false),
            Template::Null => {
                packed.push(0);
            }
            Template::Back => {
                packed.pop();
            }
            _ => {
                return Err(MonorubyErr::argumenterr(
                    "Currently, the template character is not supported.",
                ))
            }
        };
    }
    Ok(Value::bytes(packed))
}

fn parse_template(template: &str) -> Vec<TemplateNode> {
    let mut temp = vec![];
    let mut iter = template.chars().peekable();
    while let Some(ch) = iter.next() {
        let template = match ch {
            'a' => Template::Ascii,
            'A' => Template::AsciiTrim,
            'Z' => Template::CString,
            'b' => Template::BitString,
            'B' => Template::BitStringBigEndian,
            'h' => Template::Hex,
            'H' => Template::HexBigEndian,
            'c' => Template::I8,
            'C' => Template::U8,
            's' => Template::I16,
            'S' => Template::U16,
            'l' | 'i' => Template::I32,
            'L' | 'I' => Template::U32,
            'q' => Template::I64,
            'Q' => Template::U64,
            'n' => Template::U16BigEndian,
            'N' => Template::U32BigEndian,
            'U' => Template::Utf8,
            'x' => Template::Null,
            'X' => Template::Back,
            _ => panic!("Unknown template: {}", ch),
        };
        if iter.next_if(|c| c == &'*').is_some() {
            temp.push(TemplateNode {
                template,
                endless: true,
            });
        } else if let Some(d1) = iter.next_if(|c| c.is_ascii_digit()) {
            let mut count = (d1 as u32 - '0' as u32) as usize;
            while let Some(d) = iter.next_if(|c| c.is_ascii_digit()) {
                count = count * 10 + (d as u32 - '0' as u32) as usize;
            }
            for _ in 0..count {
                temp.push(TemplateNode {
                    template,
                    endless: false,
                });
            }
        } else {
            temp.push(TemplateNode {
                template,
                endless: false,
            });
        }
    }
    temp
}
