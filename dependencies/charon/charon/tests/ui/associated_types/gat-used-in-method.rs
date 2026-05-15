//@ charon-args=--remove-associated-types=_
trait Foo {
    type T<A>;
    fn f<A>(x: Self::T<A>);
}
