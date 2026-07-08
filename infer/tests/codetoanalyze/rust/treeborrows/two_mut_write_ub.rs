fn main() {
    let mut x = 0;
    let raw = &mut x as *mut i32;
    let r1 = unsafe { &mut *raw };
    let r2 = unsafe { &mut *raw };
    *r2 = 1;
    *r1 = 2;
}
