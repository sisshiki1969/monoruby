use super::*;

/// IO::Buffer flag bits (CRuby-compatible values).
pub const BUF_EXTERNAL: u32 = 1;
pub const BUF_INTERNAL: u32 = 2;
pub const BUF_MAPPED: u32 = 4;
pub const BUF_SHARED: u32 = 8;
pub const BUF_LOCKED: u32 = 32;
pub const BUF_PRIVATE: u32 = 64;
pub const BUF_READONLY: u32 = 128;

/// Where an `IO::Buffer`'s bytes live.
#[derive(Debug)]
pub enum BufStorage {
    /// Heap allocation owned by this buffer (INTERNAL, and anonymous
    /// MAPPED allocations, which monoruby backs with the same heap
    /// memory — the MAPPED flag is presentation only until file mapping
    /// arrives).
    Owned(Vec<u8>),
    /// A view into the byte storage of a Ruby String (`IO::Buffer.for`,
    /// and slices of string-backed buffers — they reference the String
    /// directly, so freeing the source buffer does not invalidate them).
    /// Reads and writes go directly through the String's bytes, so
    /// mutations are visible to (block form) — and from — the original.
    Str { s: RString, offset: usize },
    /// A file-backed mmap region (`.map`). Unmapped on drop/free.
    FileMap { ptr: *mut u8, len: usize },
    /// A view into a span of another IO::Buffer (`#slice`). Access is
    /// re-resolved through the parent on every operation, so a parent
    /// resize cannot leave a dangling pointer (out-of-range access
    /// simply fails the bounds check).
    Slice { parent: IoBuffer, offset: usize },
}

impl Clone for BufStorage {
    fn clone(&self) -> Self {
        match self {
            Self::Owned(v) => Self::Owned(v.clone()),
            // Cloning a mapping (object #dup) materializes a private copy —
            // sharing the region would double-munmap on drop.
            Self::FileMap { ptr, len } => {
                // SAFETY: ptr/len describe a live mapping owned by self.
                Self::Owned(unsafe { std::slice::from_raw_parts(*ptr, *len) }.to_vec())
            }
            Self::Str { s, offset } => Self::Str {
                s: *s,
                offset: *offset,
            },
            Self::Slice { parent, offset } => Self::Slice {
                parent: *parent,
                offset: *offset,
            },
        }
    }
}

impl Drop for BufStorage {
    fn drop(&mut self) {
        if let Self::FileMap { ptr, len } = self {
            // SAFETY: ptr/len came from a successful mmap owned solely by
            // this storage (Clone never duplicates the mapping).
            unsafe { libc::munmap(*ptr as *mut libc::c_void, *len) };
        }
    }
}

#[monoruby_object]
pub struct IoBuffer(Value);

///
/// The native payload of an `IO::Buffer` (`ObjTy::IO_BUFFER`).
///
#[derive(Debug, Clone)]
pub struct IoBufferInner {
    pub storage: BufStorage,
    pub size: usize,
    pub flags: u32,
    pub locked: bool,
}

impl IoBufferInner {
    pub fn null() -> Self {
        Self {
            storage: BufStorage::Owned(Vec::new()),
            size: 0,
            flags: 0,
            locked: false,
        }
    }

    pub fn owned(bytes: Vec<u8>, flags: u32) -> Self {
        let size = bytes.len();
        Self {
            storage: BufStorage::Owned(bytes),
            size,
            flags,
            locked: false,
        }
    }

    pub fn string_backed(s: RString, size: usize, flags: u32) -> Self {
        Self::string_backed_at(s, 0, size, flags)
    }

    pub fn string_backed_at(s: RString, offset: usize, size: usize, flags: u32) -> Self {
        Self {
            storage: BufStorage::Str { s, offset },
            size,
            flags,
            locked: false,
        }
    }

    pub fn file_map(ptr: *mut u8, len: usize, flags: u32) -> Self {
        Self {
            storage: BufStorage::FileMap { ptr, len },
            size: len,
            flags,
            locked: false,
        }
    }

    pub fn slice_of(parent: IoBuffer, offset: usize, size: usize, flags: u32) -> Self {
        Self {
            storage: BufStorage::Slice { parent, offset },
            size,
            flags,
            locked: false,
        }
    }

    pub fn is_null(&self) -> bool {
        match &self.storage {
            BufStorage::Owned(v) => v.is_empty(),
            _ => false,
        }
    }

    pub(crate) fn mark(&self, alloc: &mut crate::alloc::Allocator<RValue>) {
        match &self.storage {
            BufStorage::Str { s, .. } => s.mark(alloc),
            BufStorage::Slice { parent, .. } => parent.mark(alloc),
            _ => {}
        }
    }

    /// Copy out the buffer's live byte span. Fails with the CRuby
    /// bounds message when a slice no longer fits its parent (e.g. the
    /// parent was resized smaller).
    pub fn read_bytes(&self) -> Result<&[u8]> {
        match &self.storage {
            BufStorage::Owned(v) => Ok(v),
            BufStorage::FileMap { ptr, len } => {
                let n = self.size.min(*len);
                // SAFETY: ptr/len describe this buffer's live mapping.
                Ok(unsafe { std::slice::from_raw_parts(*ptr, n) })
            }
            BufStorage::Str { s, offset } => {
                let bytes = s.as_bytes();
                if *offset + self.size > bytes.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                Ok(&bytes[*offset..*offset + self.size])
            }
            BufStorage::Slice { parent, offset } => {
                let parent_bytes = parent.read_bytes()?;
                if *offset + self.size > parent_bytes.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                Ok(&parent_bytes[*offset..*offset + self.size])
            }
        }
    }

    /// Identity of the allocation this buffer's bytes live in, for
    /// `#copy`'s alias check: two buffers can only overlap when they
    /// resolve to the same root (the same owned Vec, mapping, or backing
    /// String — slices resolve through their parent). A CoW'd String
    /// shares its sibling's byte array, so views of either compare equal
    /// until the write un-shares them, which errs on the safe side.
    pub fn storage_root(&self) -> usize {
        match &self.storage {
            BufStorage::Owned(v) => v.as_ptr() as usize,
            BufStorage::FileMap { ptr, .. } => *ptr as usize,
            BufStorage::Str { s, .. } => s.as_bytes().as_ptr() as usize,
            BufStorage::Slice { parent, .. } => parent.storage_root(),
        }
    }

    /// Write `data` into the buffer at `offset` (already bounds-checked
    /// by the caller against `self.size`). String-backed buffers mutate
    /// the String's bytes in place, so the change is visible through the
    /// original String; slices write through to their parent.
    pub fn write_at(&mut self, offset: usize, data: &[u8]) -> Result<()> {
        match &mut self.storage {
            BufStorage::Owned(v) => {
                v[offset..offset + data.len()].copy_from_slice(data);
                Ok(())
            }
            BufStorage::FileMap { ptr, len } => {
                if offset + data.len() > *len {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                // SAFETY: bounds checked against the mapping length above.
                unsafe {
                    std::ptr::copy_nonoverlapping(data.as_ptr(), ptr.add(offset), data.len())
                };
                Ok(())
            }
            BufStorage::Str { s, offset: base } => {
                let base = *base;
                if base + offset + data.len() > s.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                for (i, b) in data.iter().enumerate() {
                    s.set_byte(base + offset + i, *b);
                }
                Ok(())
            }
            BufStorage::Slice {
                parent,
                offset: base,
            } => parent.write_at(*base + offset, data),
        }
    }
}
