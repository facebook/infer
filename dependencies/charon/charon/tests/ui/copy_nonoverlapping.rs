//@ charon-args=--extract-opaque-bodies
//@ charon-args=--opaque core::ptr::copy_nonoverlapping::precondition_check

use std::mem;
use std::ptr;

fn write<T>(x: &mut T, y: &T) {
    unsafe {
        ptr::copy_nonoverlapping(y, x, 1);
    }
}
