//@ charon-args=--mir elaborated
#![feature(core_intrinsics)]
fn main() {
    let _: u8 = unsafe { std::intrinsics::unchecked_add(255, 1) };
    let _: u8 = unsafe { std::intrinsics::unchecked_sub(0, 1) };
    let _: u8 = unsafe { std::intrinsics::unchecked_mul(255, 2) };
    let _: u8 = unsafe { std::intrinsics::unchecked_shl(255, 9) };
    let _: u8 = unsafe { std::intrinsics::unchecked_shr(255, 9) };
}
