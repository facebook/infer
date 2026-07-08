struct S {
    a: i32,
    b: i32,
}
fn main() {
    let mut s = S { a: 0, b: 0 };
    let ra = &mut s.a;
    let rb = &mut s.b;
    *ra = 1;
    *rb = 2;
}
