struct P {
    a: i32,
    b: i32,
}
fn main() {
    let mut s = P { a: 0, b: 0 };
    let r = &mut s as *mut P;
    unsafe {
        let fa = &mut (*r).a;
        let whole = &mut *r;
        (*whole).a = 7;
        *fa = 8;
    }
}
