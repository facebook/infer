//@ charon-args=--remove-associated-types=*
trait Trait {
    fn method<T: Copy>();
}

impl Trait for () {
    fn method<T: Clone>() {}
}

fn main() {
    <() as Trait>::method::<()>()
}
