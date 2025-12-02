//@ known-failure
//! Regression test minimized from the rustc test suite. This used to cause a panic.
trait HasGAT {
    type GAT<T>;
    type Friend: HasAssoc;
}

trait HasAssoc {
    type Assoc;
}

fn floatify<C>() -> <C::Friend as HasAssoc>::Assoc
where
    C: HasGAT,
{
    todo!()
}
