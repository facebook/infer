#![allow(invalid_reference_casting)]
fn f(a: &i32) {
    let p = a as *const i32 as *mut i32;
    unsafe { *p = 1; }
}
fn main() {
    let x = 0;
    f(&x);
}
