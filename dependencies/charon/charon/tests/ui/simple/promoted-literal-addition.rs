//@ charon-args=--mir optimized
fn two() -> &'static u32 {
    &(1 + 1)
}
