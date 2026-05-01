pub mod generator;
pub mod index_map;
pub mod index_vec;

pub use generator::Generator;
pub use index_map::IndexMap;
pub use index_vec::{Idx, IndexVec};

/// Generate an `Index` index type. We use it because we need manipulate a lot of different indices
/// (for various kinds of declarations, variables, blocks, etc.).
/// For sanity, we prevent any confusion between the different kinds of indices by using different
/// types. The following macro allows us to easily derive those types.
///
/// The `name` parameter should contain the name of the module to declare. The `pretty_name`
/// parameter is used to implement `Id::to_pretty_string`; if not provided, it defaults to `name`.
#[macro_export]
macro_rules! generate_index_type {
    ($name:ident) => {
        $crate::generate_index_type!($name, stringify!($name));
    };
    ($name:ident, $pretty_name:expr) => {
        index_vec::define_index_type! {
            #[derive(Default, derive_generic_visitor::Drive, derive_generic_visitor::DriveMut)]
            #[drive(skip)]
            pub struct $name = usize;
            // Must fit in an u32 for serialization.
            MAX_INDEX = std::u32::MAX as usize;
        }

        impl $name {
            pub const ZERO: Self = Self { _raw: 0 };
            pub fn is_zero(&self) -> bool {
                self.index() == 0
            }
            pub fn to_pretty_string(self) -> String {
                format!("@{}{}", $pretty_name, self)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
            ) -> std::result::Result<(), std::fmt::Error> {
                f.write_str(self.index().to_string().as_str())
            }
        }

        impl<State: ?Sized> serde_state::SerializeState<State> for $name {
            fn serialize_state<S: serde::ser::Serializer>(
                &self,
                _state: &State,
                serializer: S,
            ) -> Result<S::Ok, S::Error> {
                use serde::Serialize;
                self.index().serialize(serializer)
            }
        }
        impl<'de, State: ?Sized> serde_state::DeserializeState<'de, State> for $name {
            fn deserialize_state<D: serde::de::Deserializer<'de>>(
                _state: &State,
                deserializer: D,
            ) -> Result<Self, D::Error> {
                use serde::Deserialize;
                usize::deserialize(deserializer).map(Self::from_usize)
            }
        }
    };
}
