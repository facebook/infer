//@ charon-args=--mir optimized
pub trait MyTrait: 'static + Send {
    fn foo(&self);
}

pub struct MyStruct;

impl MyTrait for MyStruct {
    fn foo(&self) {}
}

fn main() {
    let x: &dyn MyTrait = &MyStruct;
    x.foo();
}
