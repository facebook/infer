fn reborrow(x: &mut i32) -> &mut i32 {
    &mut *x
}

fn main() {
    let mut v = 0;
    let p = &mut v;
    let q = reborrow(p);
    *q = 1;
}
