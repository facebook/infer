//@ charon-args=--remove-associated-types=*
trait Trait<'a> {
    type Type;
}

// This is incorrectly translated (https://github.com/AeneasVerif/charon/issues/534).
fn foo<T>()
where
    T: for<'a> Trait<'a, Type = &'a ()>,
{
}
