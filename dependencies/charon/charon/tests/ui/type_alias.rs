use std::borrow::Cow;
// type Foo = usize;
// type Generic<'a, T> = &'a T;
// type Generic2<'a, T: Clone> = Cow<'a, [T]>;
// type GenericWithoutBound<'a, T> = Cow<'a, [T]>;
struct Generic2<'a, T: Clone>(Cow<'a, [T]>);
