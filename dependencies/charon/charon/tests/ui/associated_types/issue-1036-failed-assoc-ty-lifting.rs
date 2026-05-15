//@ charon-args=--remove-associated-types=_
trait T1 {
    type A;
}
trait T2: T1 {}

impl<I: T1> T2 for I {}
