//@ charon-args=--remove-associated-types=*
trait HasType {
    type Type;
}

trait HasMethod {
    fn method()
    where
        Self: HasType,
    {
    }
}

impl<T> HasMethod for Option<T> {}
