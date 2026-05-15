use derive_generic_visitor::{Drive, DriveMut, Visit, VisitMut};
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::ops::{ControlFlow, Deref};
use std::sync::Arc;

use crate::common::hash_by_addr::HashByAddr;
use crate::common::type_map::Mappable;

/// Hash-consed data structure: a reference-counted wrapper that guarantees that two equal
/// value will be stored at the same address. This makes it possible to use the pointer address
/// as a hash value.
// Warning: a `derive` should not introduce a way to create a new `HashConsed` value without
// going through the interning table.
#[derive(PartialEq, Eq, Hash)]
pub struct HashConsed<T>(HashByAddr<Arc<T>>);

impl<T> Clone for HashConsed<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> HashConsed<T> {
    pub fn inner(&self) -> &T {
        self.0.0.as_ref()
    }
}

pub trait HashConsable = Hash + PartialEq + Eq + Clone + Mappable;

/// Unique id identifying a hashconsed value amongst those with the same type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HashConsId(usize);

// Private module that contains the static we'll use as interning map. A value of type
// `HashCons` MUST NOT be created in any other way than this table, else hashing and euqality
// on it will be broken. Note that this likely means that if a crate uses charon both as a
// direct dependency and as a dylib, then the static will be duplicated, causing hashing and
// equality on `HashCons` to be broken.
mod intern_table {
    use indexmap::IndexSet as SeqHashSet;
    use std::sync::{Arc, LazyLock, RwLock};

    use super::{HashConsId, HashConsable, HashConsed};
    use crate::common::hash_by_addr::HashByAddr;
    use crate::common::type_map::{Mappable, Mapper, TypeMap};

    // This is a static mutable `SeqHashSet<Arc<T>>` that records for each `T` value a unique
    // `Arc<T>` that contains the same value. Values inside the set are hashed/compared
    // as is normal for `T`.
    // Once we've gotten an `Arc` out of the set however, we're sure that "T-equality"
    // implies address-equality, hence the `HashByAddr` wrapper preserves correct equality
    // and hashing behavior.
    struct InternMapper;
    impl Mapper for InternMapper {
        type Value<T: Mappable> = SeqHashSet<Arc<T>>;
    }
    static INTERNED: LazyLock<RwLock<TypeMap<InternMapper>>> = LazyLock::new(|| Default::default());

    pub fn intern<T: HashConsable>(inner: T) -> HashConsed<T> {
        // Fast read-only check.
        #[expect(irrefutable_let_patterns)] // https://github.com/rust-lang/rust/issues/139369
        let arc = if let read_guard = INTERNED.read().unwrap()
            && let Some(set) = read_guard.get::<T>()
            && let Some(arc) = set.get(&inner)
        {
            arc.clone()
        } else {
            // Concurrent access is possible right here, so we have to check everything again.
            let mut write_guard = INTERNED.write().unwrap();
            let set: &mut SeqHashSet<Arc<T>> = write_guard.or_default::<T>();
            if let Some(arc) = set.get(&inner) {
                arc.clone()
            } else {
                let arc: Arc<T> = Arc::new(inner);
                set.insert(arc.clone());
                arc
            }
        };
        HashConsed(HashByAddr(arc))
    }

    /// Identify this value uniquely amongst values of its type. The id depends on insertion
    /// order into the interning table which makes them in principle deterministic.
    pub fn id<T: HashConsable>(x: &HashConsed<T>) -> HashConsId {
        // `HashConsed` can only be constructed via `intern`, so we know this value exists in
        // the table.
        HashConsId(
            (*INTERNED.read().unwrap())
                .get::<T>()
                .unwrap()
                .get_index_of(&x.0.0)
                .unwrap(),
        )
    }
}

impl<T> HashConsed<T>
where
    T: HashConsable,
{
    /// Deduplicate the values by hashing them. This deduplication is crucial for the hashing
    /// function to be correct. This is the only function allowed to create `Self` values.
    pub fn new(inner: T) -> Self {
        intern_table::intern(inner)
    }

    /// Clones if needed to get mutable access to the inner value.
    pub fn with_inner_mut<R>(&mut self, f: impl FnOnce(&mut T) -> R) -> R {
        // The value is behind a shared `Arc`, we clone it in order to mutate it.
        let mut value = self.inner().clone();
        let ret = f(&mut value);
        // Re-intern the new value.
        *self = Self::new(value);
        ret
    }

    pub fn id(&self) -> HashConsId {
        intern_table::id(self)
    }
}

impl<T> Deref for HashConsed<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for HashConsed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Hide the `HashByAddr` wrapper.
        f.debug_tuple("HashConsed").field(self.inner()).finish()
    }
}

impl<'s, T, V: Visit<'s, T>> Drive<'s, V> for HashConsed<T> {
    fn drive_inner(&'s self, v: &mut V) -> ControlFlow<V::Break> {
        v.visit(self.inner())
    }
}
/// Note: this explores the inner value mutably by cloning and re-hashing afterwards.
impl<'s, T, V> DriveMut<'s, V> for HashConsed<T>
where
    T: HashConsable,
    V: for<'a> VisitMut<'a, T>,
{
    fn drive_inner_mut(&'s mut self, v: &mut V) -> ControlFlow<V::Break> {
        self.with_inner_mut(|inner| v.visit(inner))
    }
}

/// `HashCons` supports serializing each value to a unique id in order to serialize
/// highly-shared values without explosion.
///
/// Note that the deduplication scheme is highly order-dependent: we serialize the real value
/// the first time it comes up, and use ids only subsequent times. This relies on the fact that
/// `derive(Serialize, Deserialize)` traverse the value in the same order.
pub use serialize::{HashConsDedupSerializer, HashConsSerializerState};
mod serialize {
    use indexmap::IndexMap as SeqHashMap;
    use serde::{Deserialize, Serialize};
    use serde_state::{DeserializeState, SerializeState};
    use std::any::type_name;
    use std::cell::RefCell;
    use std::collections::HashSet;

    use super::{HashConsId, HashConsable, HashConsed};
    use crate::common::type_map::{Mappable, Mapper, TypeMap};

    pub trait HashConsSerializerState: Sized {
        /// Record that this type is being serialized. Return `None` if we're not deduplicating
        /// values, otherwise return whether this item was newly recorded.
        fn record_serialized<T: Mappable>(&self, id: HashConsId) -> Option<bool>;
        /// Record that we deserialized this type.
        fn record_deserialized<T: Mappable>(&self, id: HashConsId, value: HashConsed<T>);
        /// Find the previously-deserialized type with that id.
        fn get_deserialized_val<T: Mappable>(&self, id: HashConsId) -> Option<HashConsed<T>>;
    }

    impl HashConsSerializerState for () {
        fn record_serialized<T: Mappable>(&self, _id: HashConsId) -> Option<bool> {
            None
        }
        fn record_deserialized<T: Mappable>(&self, _id: HashConsId, _value: HashConsed<T>) {}
        fn get_deserialized_val<T: Mappable>(&self, _id: HashConsId) -> Option<HashConsed<T>> {
            None
        }
    }

    struct SerializeTableMapper;
    impl Mapper for SerializeTableMapper {
        type Value<T: Mappable> = HashSet<HashConsId>;
    }
    struct DeserializeTableMapper;
    impl Mapper for DeserializeTableMapper {
        type Value<T: Mappable> = SeqHashMap<HashConsId, HashConsed<T>>;
    }
    #[derive(Default)]
    pub struct HashConsDedupSerializer {
        // Table used for serialization.
        ser: RefCell<TypeMap<SerializeTableMapper>>,
        // Table used for deserialization.
        de: RefCell<TypeMap<DeserializeTableMapper>>,
    }
    impl HashConsSerializerState for HashConsDedupSerializer {
        fn record_serialized<T: Mappable>(&self, id: HashConsId) -> Option<bool> {
            Some(self.ser.borrow_mut().or_default::<T>().insert(id))
        }
        fn record_deserialized<T: Mappable>(&self, id: HashConsId, val: HashConsed<T>) {
            self.de.borrow_mut().or_default::<T>().insert(id, val);
        }
        fn get_deserialized_val<T: Mappable>(&self, id: HashConsId) -> Option<HashConsed<T>> {
            self.de
                .borrow()
                .get::<T>()
                .and_then(|map| map.get(&id))
                .cloned()
        }
    }

    /// A dummy enum used when serializing/deserializing a `HashConsed<T>`.
    #[derive(Serialize, Deserialize, SerializeState, DeserializeState)]
    #[serde_state(state_implements = HashConsSerializerState)]
    enum SerRepr<T> {
        /// A value represented normally, accompanied by its id. This is emitted the first time
        /// we serialize a given value: subsequent times will use `SerRepr::Deduplicate`
        /// instead.
        HashConsedValue(#[serde_state(stateless)] HashConsId, T),
        /// A value represented by its id. The actual value must have been emitted as a
        /// `SerRepr::Value` with that same id earlier.
        #[serde_state(stateless)]
        Deduplicated(HashConsId),
        /// A plain value without an id.
        Untagged(T),
    }

    impl<T> Serialize for HashConsed<T>
    where
        T: Serialize + HashConsable,
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            SerRepr::Untagged(self.inner()).serialize(serializer)
        }
    }
    /// Options for the state are `()` to serialize values normally and `HashConsDedupSerializer`
    /// to deduplicate identical values in the serialized output.
    impl<T, State> SerializeState<State> for HashConsed<T>
    where
        T: SerializeState<State> + HashConsable,
        State: HashConsSerializerState,
    {
        fn serialize_state<S>(&self, state: &State, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let hash_cons_id = self.id();
            let repr = match state.record_serialized::<T>(hash_cons_id) {
                Some(true) => SerRepr::HashConsedValue(hash_cons_id, self.inner()),
                Some(false) => SerRepr::Deduplicated(hash_cons_id),
                None => SerRepr::Untagged(self.inner()),
            };
            repr.serialize_state(state, serializer)
        }
    }

    impl<'de, T> Deserialize<'de> for HashConsed<T>
    where
        T: Deserialize<'de> + HashConsable,
    {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            use serde::de::Error;
            let repr: SerRepr<T> = SerRepr::deserialize(deserializer)?;
            match repr {
                SerRepr::HashConsedValue { .. } | SerRepr::Deduplicated { .. } => {
                    let msg = format!(
                        "trying to deserialize a deduplicated value using serde's `{ty}::deserialize` method. \
                        This won't work, use serde_state's \
                        `{ty}::deserialize_state(&HashConsDedupSerializer::default(), _)` instead",
                        ty = type_name::<T>(),
                    );
                    Err(D::Error::custom(msg))
                }
                SerRepr::Untagged(val) => Ok(HashConsed::new(val)),
            }
        }
    }
    impl<'de, T, State> DeserializeState<'de, State> for HashConsed<T>
    where
        T: DeserializeState<'de, State> + HashConsable,
        State: HashConsSerializerState,
    {
        fn deserialize_state<D>(state: &State, deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            use serde::de::Error;
            let repr: SerRepr<T> = SerRepr::deserialize_state(state, deserializer)?;
            Ok(match repr {
                SerRepr::HashConsedValue(hash_cons_id, value) => {
                    let val = HashConsed::new(value);
                    state.record_deserialized(hash_cons_id, val.clone());
                    val
                }
                SerRepr::Deduplicated(hash_cons_id) => {
                    state.get_deserialized_val(hash_cons_id).ok_or_else(|| {
                        let msg = format!(
                            "can't deserialize deduplicated value of type {}; \
                            were you careful with managing the deduplication state?",
                            type_name::<T>()
                        );
                        D::Error::custom(msg)
                    })?
                }
                SerRepr::Untagged(val) => HashConsed::new(val),
            })
        }
    }
}

#[test]
fn test_hash_cons() {
    let x = HashConsed::new(42u32);
    let y = HashConsed::new(42u32);
    assert_eq!(x, y);
    // Test a serialization round-trip.
    let z = serde_json::from_value(serde_json::to_value(x.clone()).unwrap()).unwrap();
    assert_eq!(x, z);
}

#[test]
fn test_hash_cons_concurrent() {
    use itertools::Itertools;
    let handles = (0..10)
        .into_iter()
        .map(|_| std::thread::spawn(|| std::hint::black_box(HashConsed::new(42u32))))
        .collect_vec();
    let values = handles.into_iter().map(|h| h.join().unwrap()).collect_vec();
    assert!(values.iter().all_equal())
}

#[test]
fn test_hash_cons_dedup() {
    use serde_state::{DeserializeState, SerializeState};
    type Ty = HashConsed<TyKind>;
    #[derive(Debug, Clone, PartialEq, Eq, Hash, SerializeState, DeserializeState)]
    #[serde_state(state = HashConsDedupSerializer)]
    enum TyKind {
        Bool,
        Pair(Ty, Ty),
    }

    // Build a value with some redundancy.
    let bool1 = HashConsed::new(TyKind::Bool);
    let bool2 = HashConsed::new(TyKind::Bool);
    let pair = HashConsed::new(TyKind::Pair(bool1.clone(), bool2));
    let triple = HashConsed::new(TyKind::Pair(bool1, pair));

    let state = HashConsDedupSerializer::default();
    let json_val = triple
        .serialize_state(&state, serde_json::value::Serializer)
        .unwrap();
    let state = HashConsDedupSerializer::default();
    let round_tripped = Ty::deserialize_state(&state, json_val).unwrap();

    assert_eq!(triple, round_tripped);
}
