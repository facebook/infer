trait Trait {
    fn required(&self);
    fn provided1(&self) {
        self.required();
        self.provided2();
    }
    fn provided2(&self) {
        self.required();
        self.provided1();
    }
}

struct Foo;
impl Trait for Foo {
    fn required(&self) {
        self.provided1();
    }
}

struct Bar;
impl Trait for Bar {
    fn required(&self) {
        self.provided2();
    }
    fn provided1(&self) {
        self.provided2();
    }
}
