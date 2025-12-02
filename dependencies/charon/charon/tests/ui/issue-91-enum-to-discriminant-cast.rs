#[derive(Copy, Clone)]
enum Foo {
    A,
    B,
}

enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

fn main() {
    let x = Foo::A;
    let _ = x as isize;
    let _ = x as u8;

    let x = Ordering::Greater;
    let _ = x as isize;
}
