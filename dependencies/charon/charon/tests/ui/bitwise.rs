//! Exercise the bitwise operations
pub fn shift_u32(a: u32) -> u32 {
    let i: usize = 16;
    let mut t = a >> i;
    t <<= i;
    t
}

pub fn shift_i32(a: i32) -> i32 {
    let i: isize = 16;
    let mut t = a >> i;
    t <<= i;
    t
}

pub fn xor_u32(a: u32, b: u32) -> u32 {
    a ^ b
}

pub fn or_u32(a: u32, b: u32) -> u32 {
    a | b
}

pub fn and_u32(a: u32, b: u32) -> u32 {
    a & b
}
