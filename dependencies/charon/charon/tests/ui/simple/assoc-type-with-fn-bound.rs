//@ charon-args=--remove-associated-types=*
#![feature(unboxed_closures)]

pub trait Trait {
    type Foo: Fn();
    fn call(&self) -> <Self::Foo as FnOnce<()>>::Output;
}

impl<F: Fn()> Trait for F {
    type Foo = F;
    fn call(&self) -> <Self::Foo as FnOnce<()>>::Output {
        self()
    }
}

pub fn use_foo() -> <<fn() as Trait>::Foo as FnOnce<()>>::Output {}
