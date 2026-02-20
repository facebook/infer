fn main() {
    let x = Foo { array: [1, 2], tuple: (3, 4) };
}

struct Foo {
    array: [i32; 2],
    tuple: (i32, i32),
}