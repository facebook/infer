//@ charon-args=--include=alloc::vec::*
fn vec() {
    let _: Vec<u32> = Vec::with_capacity(42);
}
