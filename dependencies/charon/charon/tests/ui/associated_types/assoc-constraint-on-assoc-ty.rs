//@ charon-args=--remove-associated-types=*
trait Trait {
    type Assoc;
}
fn takes_trait<I: Trait>(it: I) {}

trait IntoIterator {
    type IntoIter: Trait<Assoc = ()>;
}

fn collect<I: IntoIterator>(it: I::IntoIter) {
    takes_trait(it)
}
