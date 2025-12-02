//@ charon-args=--remove-associated-types=*
trait Iterator {
    type Item;
}
trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
}
impl<I: Iterator> IntoIterator for I {
    type Item = I::Item;
    type IntoIter = I;
}

// See https://github.com/lcnr/random-rust-snippets/issues/2
fn callee<T>(t: <T as Iterator>::Item) -> <T as IntoIterator>::Item
where
    T: Iterator,
{
    t
}

/// For this to typecheck, we would need to use the blanket impl over the local clause, but rustc
/// does the opposite.
fn caller<T>(t: <T as Iterator>::Item) -> <T as IntoIterator>::Item
where
    T: Iterator + IntoIterator,
{
    callee::<T>(t)
}

trait X {
    type Assoc;
    fn method(&self) -> Self::Assoc;
}
trait A: X {}
trait B: X {}

fn a<T: A>(x: T) -> T::Assoc {
    x.method()
}
fn b<T: B>(x: T) -> T::Assoc {
    x.method()
}

/// This will pick one of the two parent clauses to resolve `T::Assoc` within this body, which is
/// guaranteed to make one of the two calls fail to typecheck.
fn x<T: A + B + Copy>(x: T) -> (T::Assoc, T::Assoc) {
    (a(x), b(x))
}
