fn main() {
    let mut x = 0;
    let raw = &mut x as *mut i32;
    let r1 = unsafe { &mut *raw };
    *r1 = 1;
    let r2 = unsafe { &mut *raw };
    *r2 = 2;
}
