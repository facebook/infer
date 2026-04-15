//@ charon-args=--mir optimized
fn main() {
    let x: *const u8 = &0;
    let _ = x as usize;
}
