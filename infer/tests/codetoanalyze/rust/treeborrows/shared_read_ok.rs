fn f(a: &i32) -> i32 {
    let p = a as *const i32;
    unsafe { *p }
}
fn main() {
    let x = 0;
    let _ = f(&x);
}
