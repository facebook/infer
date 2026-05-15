//@ charon-args=--monomorphize
#![feature(core_intrinsics)]
#![allow(internal_features)]
enum E1 {
    A,
    B,
}

enum E2 {
    A,
    B,
}

fn main() {
    // We detect these in a pass; this test makes sure we don't mess up the generics in `--mono`
    // mode.
    let _ = core::intrinsics::discriminant_value(&E1::A);
    let _ = core::intrinsics::discriminant_value(&E2::A);
}
