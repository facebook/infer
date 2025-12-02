//@ rustc-args=--target i686-unknown-linux-gnu
// To run, first install target i686-unknown-linux-gnu with `rustup target add i686-unknown-linux-gnu`

use std::ptr::NonNull;

enum HasPointerNiche {
    First,
    Second(NonNull<usize>),
}

fn main() {
    let mut x: usize = 52;
    let y: usize = usize::MAX;
    let z = y - x;

    let a = unsafe { HasPointerNiche::Second(NonNull::new_unchecked(&raw mut x)) };
}
