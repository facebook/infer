//@ charon-args=--add-drop-bounds
trait Trait<T> {
    type Type;
    fn foo<U>(x: U) {}
}

impl Trait<u32> for () {
    type Type = String;
}

fn use_trait<T, X: Trait<T>>(_x: T, _y: X) {}

fn main() {
    use_trait::<u32, ()>(42, ());
}
