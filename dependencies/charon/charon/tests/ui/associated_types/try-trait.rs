//@ charon-args=--remove-associated-types=*
//@ charon-args=--hide-marker-traits
//! The `Try` trait provides a fun example of complex associated types.
pub trait Residual<O> {
    type TryType: Try<Output = O, Residual = Self>;
}

pub trait Try: FromResidual {
    type Output;
    type Residual;
}

pub trait FromResidual<R = <Self as Try>::Residual> {}
