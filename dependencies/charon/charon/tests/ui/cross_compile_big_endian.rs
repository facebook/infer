//@ rustc-args=--target armebv7r-none-eabi
// To run, first install target armebv7r-none-eabi with `rustup target add armebv7r-none-eabi`
#![no_std]

const S: [u8; 16] = 0x12345678901234567890123456789012u128.to_ne_bytes();

#[repr(i64)]
enum HasBEDiscr {
    First = 24,
    Second = 32,
}

fn main() {
    let x: usize = 52;
    let y: usize = usize::MAX;
    let z = y - x;
    let a: u128 = 102u128 << 64;
    let b = HasBEDiscr::Second;
}
