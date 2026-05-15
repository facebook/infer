use std::borrow::Cow;

pub type Usize = usize;
pub type Generic<'a, T> = &'a T;
pub type Generic2<'a, T: Clone> = Cow<'a, [T]>;
pub type GenericWithoutBound<'a, T> = Cow<'a, [T]>;
