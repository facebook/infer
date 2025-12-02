//@ charon-args=--mir_optimized
fn six() -> u32 {
    let x = &(0 + 1);
    let y = &(2 + 3);
    x + y
}
