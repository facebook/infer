struct S {
    a: i32,
    b: i32,
}
fn main() {
    let mut s = S { a: 0, b: 0 };
    let raw = &mut s.a as *mut i32;
    let r1 = unsafe { &mut *raw };
    let r2 = unsafe { &mut *raw };
    *r2 = 1;
    *r1 = 2;
}
