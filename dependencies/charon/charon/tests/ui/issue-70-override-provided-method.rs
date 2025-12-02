fn main() {
    let _ = Some(1) == Some(1);
}

#[derive(PartialEq, PartialOrd)]
struct Foo(u32);
