//! Whether a shared library exports a symbol, read off the file.
//!
//! `is_capi_extension` asks this of every same-named `.so` on `$LOAD_PATH`
//! whenever a stand-in matched, and the answer must not come from loading
//! the library (see there). The symbols a dynamic linker resolves are in
//! an ELF file's `.dynsym` section and in a Mach-O file's `LC_SYMTAB`
//! table; both survive stripping, since the dynamic linker needs them.

use std::path::Path;

/// `Some(true)` when the library at `path` defines and exports `symbol`,
/// `Some(false)` when it does not, `None` when the file is not a shared
/// library this reader understands: another format or byte order, a
/// universal (fat) Mach-O binary, a file without section headers, a
/// truncated one.
pub(crate) fn exports(path: &Path, symbol: &str) -> Option<bool> {
    let bytes = std::fs::read(path).ok()?;
    match bytes.get(0..4)? {
        b"\x7fELF" => elf_exports(&bytes, symbol),
        // MH_MAGIC_64, little-endian.
        [0xcf, 0xfa, 0xed, 0xfe] => macho_exports(&bytes, symbol),
        _ => None,
    }
}

/// A 64-bit little-endian ELF file: the `.dynsym` section's defined
/// global or weak symbols, named through the section its `sh_link` points
/// at.
fn elf_exports(b: &[u8], symbol: &str) -> Option<bool> {
    const ELFCLASS64: u8 = 2;
    const ELFDATA2LSB: u8 = 1;
    const SHT_DYNSYM: u32 = 11;
    const SHN_UNDEF: u16 = 0;
    const STB_LOCAL: u8 = 0;
    if *b.get(4)? != ELFCLASS64 || *b.get(5)? != ELFDATA2LSB {
        return None;
    }
    let shoff = u64_at(b, 0x28)? as usize;
    let shentsize = u16_at(b, 0x3a)? as usize;
    let shnum = u16_at(b, 0x3c)? as usize;
    if shoff == 0 || shentsize < 64 {
        return None;
    }
    for i in 0..shnum {
        let sh = shoff.checked_add(i.checked_mul(shentsize)?)?;
        if u32_at(b, sh + 4)? != SHT_DYNSYM {
            continue;
        }
        let sym_off = u64_at(b, sh + 24)? as usize;
        let sym_size = u64_at(b, sh + 32)? as usize;
        let link = u32_at(b, sh + 40)? as usize;
        let entsize = u64_at(b, sh + 56)? as usize;
        if entsize < 24 {
            return None;
        }
        let strh = shoff.checked_add(link.checked_mul(shentsize)?)?;
        let str_off = u64_at(b, strh + 24)? as usize;
        let str_size = u64_at(b, strh + 32)? as usize;
        let strtab = b.get(str_off..str_off.checked_add(str_size)?)?;
        let syms = b.get(sym_off..sym_off.checked_add(sym_size)?)?;
        for sym in syms.chunks_exact(entsize) {
            let name = u32_at(sym, 0)? as usize;
            let binding = *sym.get(4)? >> 4;
            let shndx = u16_at(sym, 6)?;
            // A reference to another library, or a symbol it keeps to
            // itself.
            if shndx == SHN_UNDEF || binding == STB_LOCAL {
                continue;
            }
            if cstr_eq(strtab, name, symbol) {
                return Some(true);
            }
        }
        return Some(false);
    }
    None
}

/// A 64-bit little-endian Mach-O file: the `LC_SYMTAB` command's
/// external symbols defined in a section. C symbols carry a leading
/// underscore there.
fn macho_exports(b: &[u8], symbol: &str) -> Option<bool> {
    const HEADER_SIZE: usize = 32;
    const LC_SYMTAB: u32 = 0x2;
    const NLIST_SIZE: usize = 16;
    const N_STAB: u8 = 0xe0;
    const N_TYPE: u8 = 0x0e;
    const N_SECT: u8 = 0x0e;
    const N_EXT: u8 = 0x01;
    let ncmds = u32_at(b, 16)? as usize;
    let want = format!("_{symbol}");
    let mut off = HEADER_SIZE;
    for _ in 0..ncmds {
        let cmd = u32_at(b, off)?;
        let cmdsize = u32_at(b, off + 4)? as usize;
        if cmdsize < 8 {
            return None;
        }
        if cmd == LC_SYMTAB {
            let symoff = u32_at(b, off + 8)? as usize;
            let nsyms = u32_at(b, off + 12)? as usize;
            let stroff = u32_at(b, off + 16)? as usize;
            let strsize = u32_at(b, off + 20)? as usize;
            let strtab = b.get(stroff..stroff.checked_add(strsize)?)?;
            let syms = b.get(symoff..symoff.checked_add(nsyms.checked_mul(NLIST_SIZE)?)?)?;
            for sym in syms.chunks_exact(NLIST_SIZE) {
                let strx = u32_at(sym, 0)? as usize;
                let n_type = *sym.get(4)?;
                let defined_external =
                    n_type & N_STAB == 0 && n_type & N_EXT != 0 && n_type & N_TYPE == N_SECT;
                if defined_external && cstr_eq(strtab, strx, &want) {
                    return Some(true);
                }
            }
            return Some(false);
        }
        off = off.checked_add(cmdsize)?;
    }
    None
}

/// Whether the NUL-terminated string at `off` in `strtab` is `s`.
fn cstr_eq(strtab: &[u8], off: usize, s: &str) -> bool {
    let Some(rest) = strtab.get(off..) else {
        return false;
    };
    let end = rest.iter().position(|&c| c == 0).unwrap_or(rest.len());
    &rest[..end] == s.as_bytes()
}

fn u16_at(b: &[u8], off: usize) -> Option<u16> {
    Some(u16::from_le_bytes(b.get(off..off + 2)?.try_into().ok()?))
}

fn u32_at(b: &[u8], off: usize) -> Option<u32> {
    Some(u32::from_le_bytes(b.get(off..off + 4)?.try_into().ok()?))
}

fn u64_at(b: &[u8], off: usize) -> Option<u64> {
    Some(u64::from_le_bytes(b.get(off..off + 8)?.try_into().ok()?))
}
