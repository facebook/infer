//@ charon-args=--remove-associated-types=*
#![feature(trait_alias)]
trait Trait<T> {
    type Item;
}

#[derive(Clone)]
struct Struct;

impl<T> Trait<T> for Struct {
    type Item = u32;
}

trait Alias<U> = Trait<Option<U>, Item = u32> + Clone;

fn takes_alias<T: Alias<()>>(x: T) {
    let _ = x.clone();
}

fn use_alias() {
    takes_alias(Struct);
}
