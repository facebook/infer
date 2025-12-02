//! This module uses external types and functions

/// This function uses an external function
pub fn swap<'a, T>(x: &'a mut T, y: &'a mut T) {
    std::mem::swap(x, y)
}

/// This function uses external types and functions
pub fn test_new_non_zero_u32(x: u32) -> std::num::NonZeroU32 {
    std::num::NonZeroU32::new(x).unwrap()
}

use std::vec::Vec;

#[allow(clippy::vec_init_then_push)]
pub fn test_vec_push() {
    let mut v: Vec<u32> = Vec::new();
    v.push(0);
}

use std::cell::Cell;

pub fn use_get(rc: &Cell<u32>) -> u32 {
    rc.get()
}

pub fn incr(rc: &mut Cell<u32>) {
    *rc.get_mut() += 1;
}
