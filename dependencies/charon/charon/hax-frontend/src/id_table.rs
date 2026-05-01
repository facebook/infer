/// This module provides a notion of table, identifiers and nodes. A
/// `Node<T>` is a `Arc<T>` bundled with a unique identifier such that
/// there exists an entry in a table for that identifier.
///
/// The type `WithTable<T>` bundles a table with a value of type
/// `T`. That value of type `T` may hold an arbitrary number of
/// `Node<_>`s. In the context of a `WithTable<T>`, the type `Node<_>`
/// serializes and deserializes using a table as a state. In this
/// case, serializing a `Node<U>` produces only an identifier, without
/// any data of type `U`. Deserializing a `Node<U>` under a
/// `WithTable<T>` will recover `U` data from the table held by
/// `WithTable`.
use std::hash::Hash;

/// Unique IDs in a ID table.

#[derive(Default, Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Id {
    id: u32,
}

pub mod type_map {
    use std::{
        any::{Any, TypeId},
        collections::HashMap,
        marker::PhantomData,
    };

    pub trait Mappable = Any + Send + Sync;

    pub trait Mapper {
        type Value<T: Mappable>: Mappable;
    }

    /// A map that maps types to values in a generic manner: we store for each type `T` a value of
    /// type `M::Value<T>`.
    pub struct TypeMap<M> {
        data: HashMap<TypeId, Box<dyn Mappable>>,
        phantom: PhantomData<M>,
    }

    impl<M: Mapper> TypeMap<M> {
        pub fn get<T: Mappable>(&self) -> Option<&M::Value<T>> {
            self.data
                .get(&TypeId::of::<T>())
                // We must be careful to not accidentally cast the box itself as `dyn Any`.
                .map(|val: &Box<dyn Mappable>| &**val)
                .and_then(|val: &dyn Mappable| (val as &dyn Any).downcast_ref())
        }

        pub fn get_mut<T: Mappable>(&mut self) -> Option<&mut M::Value<T>> {
            self.data
                .get_mut(&TypeId::of::<T>())
                // We must be careful to not accidentally cast the box itself as `dyn Any`.
                .map(|val: &mut Box<dyn Mappable>| &mut **val)
                .and_then(|val: &mut dyn Mappable| (val as &mut dyn Any).downcast_mut())
        }

        pub fn insert<T: Mappable>(&mut self, val: M::Value<T>) -> Option<Box<M::Value<T>>> {
            self.data
                .insert(TypeId::of::<T>(), Box::new(val))
                .and_then(|val: Box<dyn Mappable>| (val as Box<dyn Any>).downcast().ok())
        }
    }

    impl<M> Default for TypeMap<M> {
        fn default() -> Self {
            Self {
                data: Default::default(),
                phantom: Default::default(),
            }
        }
    }
}

pub mod hash_consing {
    use super::hash_by_addr::HashByAddr;
    use super::type_map::{Mappable, Mapper, TypeMap};
    use std::collections::HashSet;
    use std::hash::Hash;
    use std::ops::Deref;
    use std::sync::{Arc, LazyLock, RwLock};

    /// Hash-consed data structure: a reference-counted wrapper that guarantees that two equal
    /// value will be stored at the same address. This makes it possible to use the pointer address
    /// as a hash value.
    #[derive(PartialEq, Eq, Hash)]
    pub struct HashConsed<T>(HashByAddr<Arc<T>>);

    impl<T> HashConsed<T> {
        pub fn inner(&self) -> &T {
            self.0.0.as_ref()
        }
    }

    impl<T> HashConsed<T>
    where
        T: Hash + PartialEq + Eq + Mappable,
    {
        pub fn new(inner: T) -> Self {
            Self::intern(inner)
        }

        /// Clones if needed to get mutable access to the inner value.
        pub fn with_inner_mut<R>(&mut self, f: impl FnOnce(&mut T) -> R) -> R
        where
            T: Clone,
        {
            // The value is behind a shared `Arc`, we clone it in order to mutate it.
            let mut value = self.inner().clone();
            let ret = f(&mut value);
            // Re-intern the new value.
            *self = Self::intern(value);
            ret
        }

        /// Deduplicate the values by hashing them. This deduplication is crucial for the hashing
        /// function to be correct. This is the only function allowed to create `Self` values.
        fn intern(inner: T) -> Self {
            struct InternMapper;
            impl Mapper for InternMapper {
                type Value<T: Mappable> = HashSet<Arc<T>>;
            }
            // This is a static mutable `HashSet<Arc<T>>` that records for each `T` value a unique
            // `Arc<T>` that contains the same value. Values inside the set are hashed/compared
            // as is normal for `T`.
            // Once we've gotten an `Arc` out of the set however, we're sure that "T-equality"
            // implies address-equality, hence the `HashByAddr` wrapper preserves correct equality
            // and hashing behavior.
            static INTERNED: LazyLock<RwLock<TypeMap<InternMapper>>> =
                LazyLock::new(|| Default::default());

            if INTERNED.read().unwrap().get::<T>().is_none() {
                INTERNED.write().unwrap().insert::<T>(HashSet::default());
            }
            let read_guard = INTERNED.read().unwrap();
            let arc = if let Some(arc) = (*read_guard).get::<T>().unwrap().get(&inner) {
                arc.clone()
            } else {
                drop(read_guard);
                let arc: Arc<T> = Arc::new(inner);
                INTERNED
                    .write()
                    .unwrap()
                    .get_mut::<T>()
                    .unwrap()
                    .insert(arc.clone());
                arc
            };
            Self(HashByAddr(arc))
        }
    }

    impl<T> Clone for HashConsed<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
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

    #[test]
    fn test_hash_cons() {
        let x = HashConsed::new(42u32);
        let y = HashConsed::new(42u32);
        assert_eq!(x, y);
        let z = serde_json::from_value(serde_json::to_value(x.clone()).unwrap()).unwrap();
        assert_eq!(x, z);
    }
}

pub mod hash_by_addr {
    use std::{
        hash::{Hash, Hasher},
        ops::Deref,
    };

    /// A wrapper around a smart pointer that hashes and compares the contents by the address of
    /// the pointee.
    #[derive(Debug, Clone)]
    pub struct HashByAddr<T>(pub T);

    impl<T: Deref> HashByAddr<T> {
        pub fn addr(&self) -> *const T::Target {
            self.0.deref()
        }
    }

    impl<T: Deref> Eq for HashByAddr<T> {}
    impl<T: Deref> PartialEq for HashByAddr<T> {
        fn eq(&self, other: &Self) -> bool {
            std::ptr::addr_eq(self.addr(), other.addr())
        }
    }

    impl<T: Deref> Hash for HashByAddr<T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.addr().hash(state);
        }
    }

    // Delegate `Ord` impls to the derefed value.
    impl<T: Deref<Target: PartialOrd>> PartialOrd for HashByAddr<T> {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            self.0.partial_cmp(&other.0)
        }
    }
    impl<T: Deref<Target: Ord>> Ord for HashByAddr<T> {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.cmp(&other.0)
        }
    }
}
