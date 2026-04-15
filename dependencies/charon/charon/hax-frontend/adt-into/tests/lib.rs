/// For the example, let's assume we are working with `Literal`, an
/// ADT that represents literal values. Suppose strings are
/// represented via an identifier stored in a state `State`.
pub mod source {
    use std::collections::HashMap;
    #[derive(Clone, Debug)]
    pub struct State(pub HashMap<StringId, String>);

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub struct StringId(u32);

    #[derive(Clone, Debug)]
    pub enum Literal {
        Integer(u32),
        String(StringId),
    }
}

/// Here, we mirror the same data type `Literal`, but with a small
/// difference: there is no `StringId` any longer: we define a `impl`
/// of `SInto` specifically for `StringId`, that ships with a stateful
/// lookup. Magically, everytime a mirrored datatype annotated with
/// `AdtInto` will have a field or a variant of type String while the
/// original type was `StringId`, the lookup will be done
/// automatically.
mod mirrored {
    use super::{sinto::*, source};
    use hax_adt_into::*;

    #[derive(AdtInto)]
    #[args(<>, from: source::Literal, state: source::State as s)]
    pub enum Literal {
        Integer(u32),
        String(String),
    }

    impl SInto<source::State, String> for source::StringId {
        fn sinto(&self, s: &source::State) -> String {
            s.0.get(self).unwrap().clone()
        }
    }
}

/// Definition of the `sinto` trait used by the `AdtInto` macro
pub mod sinto {
    pub trait SInto<S, To> {
        fn sinto(&self, s: &S) -> To;
    }

    /// Default implementation for type implementing Copy
    impl<S, T: Copy> SInto<S, T> for T {
        fn sinto(&self, _s: &S) -> T {
            *self
        }
    }
}
