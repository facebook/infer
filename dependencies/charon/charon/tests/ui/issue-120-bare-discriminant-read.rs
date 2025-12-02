//@ charon-args=--extract-opaque-bodies
//@ charon-args=--mir_optimized
//@ rustc-args=-C opt-level=3
#![feature(core_intrinsics)]
#![allow(internal_features)]

fn discriminant_value<T>(opt: &Option<T>) -> isize {
    core::intrinsics::discriminant_value(opt)
}

fn is_some<T>(opt: Option<T>) -> bool {
    discriminant_value(&opt) != 0
}

// This doesn't optimize to a bare discriminant read :(
fn my_is_some<T>(opt: Option<T>) -> isize {
    match opt {
        Some(_) => 1,
        None => 0,
    }
}
