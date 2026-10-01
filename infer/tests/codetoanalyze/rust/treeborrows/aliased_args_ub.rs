fn use_both(a: &mut i32, b: &mut i32) {
    *a = 1;
    let _ = *b;
}

fn main() {
    let mut x = 0;
    let p: *mut i32 = &mut x;
    unsafe {
        use_both(&mut *p, &mut *p);
    }
}
