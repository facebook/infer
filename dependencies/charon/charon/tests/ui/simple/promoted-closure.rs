//@ charon-args=--mir optimized
pub fn foo() -> &'static impl Fn(u32) -> u32 {
    &|x: u32| x
}
