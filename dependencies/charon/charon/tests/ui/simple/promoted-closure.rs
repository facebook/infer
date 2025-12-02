//@ charon-args=--mir_optimized
pub fn foo() -> &'static impl Fn(u32) -> u32 {
    &|x: u32| x
}
