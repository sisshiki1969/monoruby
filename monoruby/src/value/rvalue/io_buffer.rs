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
#[derive(Debug, Clone)]
pub enum BufStorage {
    /// Zero-sized or freed buffer.
    Null,
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
    Str { s: Value, offset: usize },
    /// A view into a span of another IO::Buffer (`#slice`). Access is
    /// re-resolved through the parent on every operation, so a parent
    /// resize cannot leave a dangling pointer (out-of-range access
    /// simply fails the bounds check).
    Slice { parent: Value, offset: usize },
}

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
            storage: BufStorage::Null,
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

    pub fn string_backed(s: Value, size: usize, flags: u32) -> Self {
        Self::string_backed_at(s, 0, size, flags)
    }

    pub fn string_backed_at(s: Value, offset: usize, size: usize, flags: u32) -> Self {
        Self {
            storage: BufStorage::Str { s, offset },
            size,
            flags,
            locked: false,
        }
    }

    pub fn slice_of(parent: Value, offset: usize, size: usize, flags: u32) -> Self {
        Self {
            storage: BufStorage::Slice { parent, offset },
            size,
            flags,
            locked: false,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self.storage, BufStorage::Null)
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
    pub fn read_bytes(&self) -> Result<Vec<u8>> {
        match &self.storage {
            BufStorage::Null => Ok(Vec::new()),
            BufStorage::Owned(v) => Ok(v.clone()),
            BufStorage::Str { s, offset } => {
                let bytes = s.as_rstring_inner().as_bytes();
                if *offset + self.size > bytes.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                Ok(bytes[*offset..*offset + self.size].to_vec())
            }
            BufStorage::Slice { parent, offset } => {
                let parent_bytes = parent.as_io_buffer_inner().read_bytes()?;
                if *offset + self.size > parent_bytes.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                Ok(parent_bytes[*offset..*offset + self.size].to_vec())
            }
        }
    }

    /// Write `data` into the buffer at `offset` (already bounds-checked
    /// by the caller against `self.size`). String-backed buffers mutate
    /// the String's bytes in place, so the change is visible through the
    /// original String; slices write through to their parent.
    pub fn write_at(&mut self, offset: usize, data: &[u8]) -> Result<()> {
        match &mut self.storage {
            BufStorage::Null => Ok(()),
            BufStorage::Owned(v) => {
                v[offset..offset + data.len()].copy_from_slice(data);
                Ok(())
            }
            BufStorage::Str { s, offset: base } => {
                let base = *base;
                let mut sv = *s;
                let inner = sv.as_rstring_inner_mut();
                if base + offset + data.len() > inner.len() {
                    return Err(MonorubyErr::argumenterr(
                        "Specified offset+length is bigger than the buffer size!",
                    ));
                }
                for (i, b) in data.iter().enumerate() {
                    inner.set_byte(base + offset + i, *b);
                }
                Ok(())
            }
            BufStorage::Slice {
                parent,
                offset: base,
            } => {
                let base = *base;
                let mut pv = *parent;
                pv.as_io_buffer_inner_mut().write_at(base + offset, data)
            }
        }
    }
}
