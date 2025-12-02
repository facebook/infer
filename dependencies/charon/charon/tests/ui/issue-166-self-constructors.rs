enum Foo {
    A,
    B(usize),
}

impl Foo {
    pub fn b() -> Self {
        Self::B(0)
    }
}

struct Bar<'a> {
    r: &'a i32,
}

impl<'a> Bar<'a> {
    fn new(r: &'a i32) -> Bar {
        Self { r }
    }
}
