pub trait Try: FromResidual<()> {
    type Residual;
}

pub trait FromResidual<R> {}

impl<T> Try for Option<T> {
    type Residual = ();
}

impl<T> FromResidual<<Self as Try>::Residual> for Option<T> {}
