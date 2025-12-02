//! Implemented methods are allowed to have more general types than the declared method. This
//! causes subtleties in translation.
trait Trait: Sized {
    fn method1(self, other: &'static u32) -> bool;
    fn method2<T: Copy>(self, other: T);
}

impl Trait for () {
    // This implementation is more general because it works for non-static refs.
    fn method1(self, _other: &u32) -> bool {
        true
    }
    // This implementation is more general because it works for non-`Copy` `T`s. We can't call
    // it with its more general type however.
    fn method2<T>(self, _other: T) {}
}

trait MyCompare<Other>: Sized {
    fn compare(self, other: Other) -> bool;
}

impl<'a> MyCompare<&'a ()> for &'a () {
    // This implementation is more general because it works for non-`'a` refs. Note that only
    // late-bound vars may differ in this way.
    // If we used this signature when translating, we'd need unification to map from the expected
    // trait method generics to the actual impl method generics.
    fn compare<'b>(self, _other: &'b ()) -> bool {
        true
    }
}

fn main() {
    let _ = ().method1(&1u32);
    // TODO: this gives incorrect predicates
    // let _ = ().method2(false);
    // Not allowed to use the more precise signature.
    // let _ = ().method2(String::new());
    let _ = ().compare(&());
}

trait Foo {
    fn foo<'a, 'b>(x: &'a (), y: &'b ()) -> &'a ();
}
impl Foo for () {
    // Lifetimes declared in opposite order despite having the same names.
    // TODO: make sure we substitute the right lifetimes.
    fn foo<'a, 'b>(x: &'b (), y: &'a ()) -> &'b () {
        x
    }
}
fn call_foo<'e>(x: &'e ()) -> &'e () {
    // Calls have erased lifetimes so we can't notice the discrepancy if there is one.
    <() as Foo>::foo(x, &())
}
