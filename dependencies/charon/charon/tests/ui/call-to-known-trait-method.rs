//@ charon-args=--remove-associated-types=*
//! Test that we pass generics correctly in the `skip_trait_refs_when_known` pass.
#[derive(Default)]
struct Struct<A>(A);

trait Trait<B> {
    type Item;
    fn method<C>();
}

impl<A: Clone, B: PartialEq<bool>> Trait<B> for Struct<A> {
    type Item = (A, B);
    fn method<C>() {}
}

fn main() {
    let _x: <Struct<u8> as Trait<bool>>::Item = (0u8, false);
    let _y: Struct<bool> = Default::default();
    <Struct<u8> as Trait<bool>>::method::<String>();
}
