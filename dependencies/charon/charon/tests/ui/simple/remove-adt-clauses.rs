//@ charon-args=--remove-adt-clauses
//@ charon-args=--remove-associated-types=*
trait Trait {
    type Type;
}

impl Trait for u32 {
    type Type = bool;
}

struct Foo<T: Trait>(T::Type);

fn foo(_x: Foo<u32>) {}
