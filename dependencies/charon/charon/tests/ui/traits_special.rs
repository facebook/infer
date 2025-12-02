// The example below (in particular the impl block) is peculiar with regards
// to the treatment of regions. In particular, when we translate the implementation
// of `from` to LLBC, we get something which looks like this:
// ```
//
// fn crate::{bool}::from<@R0, @R1>(@1: &@R1 (bool)) ->
//   core::result::Result<bool, crate::{bool}<@R0>::Error> {
//   //                                       ^^^
//   //                                       HERE
//   ... // omitted
// }
// ```
pub trait From<T> {
    type Error;
    fn from(v: T) -> Result<Self, Self::Error>
    where
        Self: std::marker::Sized;
}

impl From<&bool> for bool {
    type Error = ();

    fn from(v: &bool) -> Result<Self, Self::Error> {
        Ok(*v)
    }
}
