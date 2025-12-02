//@ charon-args=--mir elaborated
//@ charon-args=--include core::num::_::_

#![feature(core_intrinsics)]
fn main() {
    let _: u8 = unsafe { std::intrinsics::wrapping_add(255, 1) };
    let _: u8 = unsafe { std::intrinsics::wrapping_sub(0, 1) };
    let _: u8 = unsafe { std::intrinsics::wrapping_mul(255, 2) };

    255u8.wrapping_add(1);
    0u8.wrapping_sub(1);
    255u8.wrapping_mul(2);
}
