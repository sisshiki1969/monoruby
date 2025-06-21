pub(crate) use self::inner::{do_alloc, Global};
// Basic non-nightly case.
// This uses `allocator-api2` enabled by default.
// If any crate enables "nightly" in `allocator-api2`,
// this will be equivalent to the nightly case,
// since `allocator_api2::alloc::Allocator` would be re-export of
// `core::alloc::Allocator`.
mod inner {
    use crate::alloc::alloc::Layout;
    pub use allocator_api2::alloc::{Allocator, Global};
    use core::ptr::NonNull;

    #[allow(clippy::map_err_ignore)]
    pub(crate) fn do_alloc(layout: Layout) -> Result<NonNull<u8>, ()> {
        match Global.allocate(layout) {
            Ok(ptr) => Ok(ptr.cast()),
            Err(_) => Err(()),
        }
    }
}
