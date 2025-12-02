trait HasAssoc {
    type Assoc;
}

trait SuperTrait {}
trait Trait: SuperTrait {}

impl<T> SuperTrait for T where T: HasAssoc<Assoc = ()> {}
impl<T> Trait for T where T: HasAssoc<Assoc = ()> {}

fn main() {}
