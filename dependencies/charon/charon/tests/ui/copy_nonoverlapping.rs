//@ charon-args=--extract-opaque-bodies
//@ charon-args=--opaque core::ptr::copy_nonoverlapping::precondition_check
//@ charon-args=--opaque core::alloc::layout::_::new
//@ charon-args=--opaque core::alloc::layout::_::from_size_align_unchecked
//@ charon-args=--opaque core::alloc::layout::Layout

use std::mem;
use std::ptr;

fn write<T>(x: &mut T, y: &T) {
    unsafe {
        ptr::copy_nonoverlapping(y, x, 1);
    }
}
