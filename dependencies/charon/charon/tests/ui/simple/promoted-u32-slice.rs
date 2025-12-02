//@ charon-args=--mir_optimized
pub fn foo() -> &'static [u32] {
    &[0, 1, 2, 3]
}
