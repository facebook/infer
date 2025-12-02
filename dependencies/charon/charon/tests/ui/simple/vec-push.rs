//@ charon-args=--include=alloc::vec::*
fn vec(x: &mut Vec<u32>) {
    x.push(42)
}
