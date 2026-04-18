//@ charon-args=--precise-drops
struct A {
    x: Box<u32>,
    b: B,
}

struct B {
    x: Box<u32>,
    y: Box<u32>,
}

fn takes_a(_: A) {}

impl Drop for B {
    fn drop(&mut self) {}
}
