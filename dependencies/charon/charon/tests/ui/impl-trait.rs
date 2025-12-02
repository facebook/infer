pub trait Foo {
    type Type: Clone;

    fn get_ty(&self) -> &Self::Type;
}

impl Foo for () {
    type Type = u32;

    fn get_ty(&self) -> &u32 {
        &42
    }
}

fn mk_foo() -> impl Foo {
    ()
}

fn use_foo() {
    let foo = mk_foo();
    let _ = foo.get_ty().clone();
}

// "Return Position Impl Trait In Trait". This generates a virtual associated type.
pub trait RPITIT {
    fn make_foo() -> impl Foo {}
}

impl RPITIT for () {
    fn make_foo() -> impl Foo {
        ()
    }
}

fn use_tait<T: RPITIT>() {
    let foo = T::make_foo();
    let _ = foo.get_ty().clone();
}

pub struct WrapClone<T: Clone>(T);
pub fn wrap<U>() -> impl for<'a> FnOnce(&'a U) -> WrapClone<&'a U> {
    |x| WrapClone(x)
}

pub fn use_wrap() {
    let f = wrap::<u32>();
    let _: WrapClone<&u32> = f(&42);
}
