//@ charon-args=--precise-drops
//! Test that we can generate drop glue for a polymorphic type that uses associated types (as that
//! used to cause ICEs in the compiler).
enum Cow<'a, B>
where
    B: 'a + ToOwned + ?Sized,
{
    Borrowed(&'a B),
    Owned(<B as ToOwned>::Owned),
}

fn drop_cow<'a, B>(_: Cow<'a, B>)
where
    B: 'a + ToOwned + ?Sized,
{
}
