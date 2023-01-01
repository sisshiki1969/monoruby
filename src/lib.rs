#![feature(box_patterns)]
#![feature(int_roundings)]
#![feature(const_option)]
#![feature(once_cell)]
#![allow(clippy::too_many_arguments)]
mod alloc;
mod executor;
mod id_table;
#[cfg(test)]
mod tests;
mod value;

use alloc::*;
pub use executor::*;
use fxhash::FxHashMap as HashMap;
use id_table::*;
use monoasm::CodePtr;
use value::*;

pub const STRING_INLINE_CAP: usize = 39;
