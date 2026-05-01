//@ known-failure
//@ charon-args=--lift-associated-types=*
pub trait HasAssoc {
    type Assoc;
}

pub type Alias<B> = <B as HasAssoc>::Assoc;
