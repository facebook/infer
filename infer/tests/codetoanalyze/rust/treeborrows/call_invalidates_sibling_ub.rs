fn write(a: &mut i32) {
    *a = 1;
}

fn main() {
    let mut v = 0;
    let p = &mut v as *mut i32;
    let q = unsafe { &mut *p };
    let r = unsafe { &mut *p };
    write(q);
    *r = 5;
}
