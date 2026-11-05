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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// `exports` of `symbol` in a file holding `image`.
    fn probe(image: &[u8], symbol: &str) -> Option<bool> {
        let mut f = tempfile::NamedTempFile::new().unwrap();
        f.write_all(image).unwrap();
        exports(f.path(), symbol)
    }

    fn put_u16(b: &mut [u8], off: usize, v: u16) {
        b[off..off + 2].copy_from_slice(&v.to_le_bytes());
    }

    fn put_u32(b: &mut [u8], off: usize, v: u32) {
        b[off..off + 4].copy_from_slice(&v.to_le_bytes());
    }

    fn put_u64(b: &mut [u8], off: usize, v: u64) {
        b[off..off + 8].copy_from_slice(&v.to_le_bytes());
    }

    /// The names of `symbols` as a string table (a leading NUL, each
    /// NUL-terminated), with each name's offset.
    fn strtab(symbols: &[(&str, u8, u16)]) -> (Vec<u8>, Vec<u32>) {
        let mut tab = vec![0u8];
        let mut offs = vec![];
        for (name, _, _) in symbols {
            offs.push(tab.len() as u32);
            tab.extend_from_slice(name.as_bytes());
            tab.push(0);
        }
        (tab, offs)
    }

    /// A 64-bit little-endian ELF shared object whose `.dynsym` holds
    /// `symbols` as (name, `st_info`, `st_shndx`); without `dynsym`, the
    /// section table has only the null section and the string table.
    fn elf(symbols: &[(&str, u8, u16)], dynsym: bool) -> Vec<u8> {
        const ELF_HEADER: usize = 64;
        const SH_SIZE: usize = 64;
        const SYM_SIZE: usize = 24;
        let (names, name_offs) = strtab(symbols);
        let mut b = vec![0u8; ELF_HEADER];
        b[0..4].copy_from_slice(b"\x7fELF");
        b[4] = 2; // ELFCLASS64
        b[5] = 1; // ELFDATA2LSB
        b[6] = 1;
        put_u16(&mut b, 16, 3); // ET_DYN
        put_u16(&mut b, 18, 62); // EM_X86_64
        put_u32(&mut b, 20, 1);
        put_u16(&mut b, 52, ELF_HEADER as u16);
        put_u16(&mut b, 58, SH_SIZE as u16);
        let str_off = b.len();
        b.extend_from_slice(&names);
        let sym_off = b.len();
        for (i, (_, info, shndx)) in symbols.iter().enumerate() {
            let mut sym = [0u8; SYM_SIZE];
            put_u32(&mut sym, 0, name_offs[i]);
            sym[4] = *info;
            put_u16(&mut sym, 6, *shndx);
            b.extend_from_slice(&sym);
        }
        let sym_size = b.len() - sym_off;
        let sh_off = b.len();
        let mut sections: Vec<[u8; SH_SIZE]> = vec![[0u8; SH_SIZE]];
        if dynsym {
            let mut sh = [0u8; SH_SIZE];
            put_u32(&mut sh, 4, 11); // SHT_DYNSYM
            put_u64(&mut sh, 24, sym_off as u64);
            put_u64(&mut sh, 32, sym_size as u64);
            put_u32(&mut sh, 40, 2); // sh_link: the string table below
            put_u64(&mut sh, 56, SYM_SIZE as u64);
            sections.push(sh);
        }
        let mut sh = [0u8; SH_SIZE];
        put_u32(&mut sh, 4, 3); // SHT_STRTAB
        put_u64(&mut sh, 24, str_off as u64);
        put_u64(&mut sh, 32, names.len() as u64);
        sections.push(sh);
        for sh in &sections {
            b.extend_from_slice(sh);
        }
        put_u64(&mut b, 0x28, sh_off as u64);
        put_u16(&mut b, 0x3c, sections.len() as u16);
        b
    }

    /// A 64-bit little-endian Mach-O dynamic library whose `LC_SYMTAB`
    /// holds `symbols` as (name, `n_type`); without `symtab`, no load
    /// command at all.
    fn macho(symbols: &[(&str, u8, u16)], symtab: bool) -> Vec<u8> {
        const HEADER: usize = 32;
        const LC_SYMTAB_SIZE: usize = 24;
        const NLIST_SIZE: usize = 16;
        let (names, name_offs) = strtab(symbols);
        let mut b = vec![0u8; HEADER];
        put_u32(&mut b, 0, 0xfeedfacf); // MH_MAGIC_64
        put_u32(&mut b, 4, 0x0100000c); // CPU_TYPE_ARM64
        put_u32(&mut b, 12, 6); // MH_DYLIB
        if symtab {
            put_u32(&mut b, 16, 1);
            put_u32(&mut b, 20, LC_SYMTAB_SIZE as u32);
            let symoff = HEADER + LC_SYMTAB_SIZE;
            let stroff = symoff + symbols.len() * NLIST_SIZE;
            let mut lc = [0u8; LC_SYMTAB_SIZE];
            put_u32(&mut lc, 0, 0x2); // LC_SYMTAB
            put_u32(&mut lc, 4, LC_SYMTAB_SIZE as u32);
            put_u32(&mut lc, 8, symoff as u32);
            put_u32(&mut lc, 12, symbols.len() as u32);
            put_u32(&mut lc, 16, stroff as u32);
            put_u32(&mut lc, 20, names.len() as u32);
            b.extend_from_slice(&lc);
            for (i, (_, n_type, _)) in symbols.iter().enumerate() {
                let mut sym = [0u8; NLIST_SIZE];
                put_u32(&mut sym, 0, name_offs[i]);
                sym[4] = *n_type;
                sym[5] = 1;
                b.extend_from_slice(&sym);
            }
            b.extend_from_slice(&names);
        }
        b
    }

    const GLOBAL_FUNC: u8 = 0x12; // STB_GLOBAL, STT_FUNC
    const WEAK_FUNC: u8 = 0x22; // STB_WEAK, STT_FUNC
    const LOCAL_FUNC: u8 = 0x02; // STB_LOCAL, STT_FUNC

    #[test]
    fn elf_defined_global_or_weak_symbol_is_exported() {
        let image = elf(&[("other", GLOBAL_FUNC, 7), ("monoruby_capi_init", WEAK_FUNC, 9)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(true));
        assert_eq!(probe(&image, "other"), Some(true));
        assert_eq!(probe(&image, "monoruby_capi"), Some(false));
        assert_eq!(probe(&image, "absent"), Some(false));
    }

    #[test]
    fn elf_undefined_or_local_symbol_is_not_exported() {
        // A reference to another library's definition.
        let image = elf(&[("monoruby_capi_init", GLOBAL_FUNC, 0)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(false));
        let image = elf(&[("monoruby_capi_init", LOCAL_FUNC, 9)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(false));
    }

    #[test]
    fn elf_without_a_dynamic_symbol_table_or_section_headers_is_unknown() {
        assert_eq!(probe(&elf(&[], false), "monoruby_capi_init"), None);
        let mut image = elf(&[("monoruby_capi_init", GLOBAL_FUNC, 9)], true);
        put_u64(&mut image, 0x28, 0);
        assert_eq!(probe(&image, "monoruby_capi_init"), None);
        // Section headers past the end of the file.
        let mut image = elf(&[("monoruby_capi_init", GLOBAL_FUNC, 9)], true);
        put_u64(&mut image, 0x28, 1 << 40);
        assert_eq!(probe(&image, "monoruby_capi_init"), None);
    }

    #[test]
    fn elf_of_another_class_or_byte_order_is_unknown() {
        let mut image = elf(&[("monoruby_capi_init", GLOBAL_FUNC, 9)], true);
        image[4] = 1; // ELFCLASS32
        assert_eq!(probe(&image, "monoruby_capi_init"), None);
        let mut image = elf(&[("monoruby_capi_init", GLOBAL_FUNC, 9)], true);
        image[5] = 2; // ELFDATA2MSB
        assert_eq!(probe(&image, "monoruby_capi_init"), None);
        assert_eq!(probe(&image[..20], "monoruby_capi_init"), None);
    }

    const N_SECT_EXT: u8 = 0x0f; // N_SECT | N_EXT
    const N_SECT_PRIVATE: u8 = 0x0e; // N_SECT without N_EXT
    const N_UNDF_EXT: u8 = 0x01; // N_UNDF | N_EXT
    const N_STAB_FUN: u8 = 0x24; // a debugging entry

    #[test]
    fn macho_defined_external_symbol_is_exported() {
        let image = macho(&[("_other", N_SECT_EXT, 0), ("_monoruby_capi_init", N_SECT_EXT, 0)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(true));
        assert_eq!(probe(&image, "other"), Some(true));
        assert_eq!(probe(&image, "absent"), Some(false));
    }

    #[test]
    fn macho_private_undefined_or_debugging_entries_are_not_exported() {
        let image = macho(&[("_monoruby_capi_init", N_SECT_PRIVATE, 0)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(false));
        let image = macho(&[("_monoruby_capi_init", N_UNDF_EXT, 0)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(false));
        let image = macho(&[("_monoruby_capi_init", N_STAB_FUN, 0)], true);
        assert_eq!(probe(&image, "monoruby_capi_init"), Some(false));
    }

    #[test]
    fn macho_without_a_symbol_table_and_other_formats_are_unknown() {
        assert_eq!(probe(&macho(&[], false), "monoruby_capi_init"), None);
        // A truncated load command.
        let mut image = macho(&[("_monoruby_capi_init", N_SECT_EXT, 0)], true);
        image.truncate(40);
        assert_eq!(probe(&image, "monoruby_capi_init"), None);
        // A universal binary, a 32-bit Mach-O file, and no object file at all.
        assert_eq!(probe(&[0xca, 0xfe, 0xba, 0xbe, 0, 0, 0, 2], "x"), None);
        assert_eq!(probe(&[0xce, 0xfa, 0xed, 0xfe, 0, 0, 0, 0], "x"), None);
        assert_eq!(probe(b"#!/usr/bin/env ruby\n", "x"), None);
        assert_eq!(probe(b"", "x"), None);
    }
}
