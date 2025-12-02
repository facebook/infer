//@ charon-args=--mir_optimized
#[allow(arithmetic_overflow)]
fn overflow() -> &'static u32 {
    &(u32::MAX + 1)
}
