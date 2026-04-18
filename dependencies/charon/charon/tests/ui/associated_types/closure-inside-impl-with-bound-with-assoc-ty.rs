//@ charon-args=--remove-associated-types=*
//! Regression test for issue https://github.com/AeneasVerif/charon/issues/627
pub trait PrimeField {
    type Repr;
}

pub struct SqrtTables<F>(F);

impl<F> SqrtTables<F> {
    pub fn sqrt_common()
    where
        F: PrimeField,
    {
        let _closure = |_x: ()| ();
    }
}
