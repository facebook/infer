//! "RPITIT" stands for "Return Position Impl Trait In Trait".
trait Trait {}

trait Foo<T> {
    fn into_iter<U>(self) -> impl Trait;
}
