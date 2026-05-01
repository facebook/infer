struct A {
    x: B,
}

struct B {
    y: u32,
}

fn main() {
    let _ = std::mem::offset_of!(A, x.y);
}
