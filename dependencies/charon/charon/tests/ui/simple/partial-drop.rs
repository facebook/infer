//@ charon-args=--precise-drops
//@ charon-args=--hide-marker-traits
//@ charon-args=--hide-allocator
struct Foo {
    x: Box<u32>,
    y: Box<u32>,
    z: Box<u32>,
}

fn foo(f: Foo) {
    drop(f.x); // Move out of `x`
    // Only `y` and `z` should be dropped here
}
