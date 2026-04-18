use std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
};

pub trait TypeMappable = Any + Send + Sync;

/// Defines a mapping from types to types.
pub trait TypeMapper {
    type Value<T: TypeMappable>: TypeMappable;
}

/// A map that maps types to values in a generic manner: we store for each type `T` a value of
/// type `M::Value<T>`.
pub struct TypeMap<M> {
    data: HashMap<TypeId, Box<dyn TypeMappable>>,
    phantom: PhantomData<M>,
}

impl<M: TypeMapper> TypeMap<M> {
    pub fn get<T: TypeMappable>(&self) -> Option<&M::Value<T>> {
        self.data
            .get(&TypeId::of::<T>())
            // We must be careful to not accidentally cast the box itself as `dyn Any`.
            .map(|val: &Box<dyn TypeMappable>| &**val)
            .and_then(|val: &dyn TypeMappable| (val as &dyn Any).downcast_ref())
    }

    pub fn get_mut<T: TypeMappable>(&mut self) -> Option<&mut M::Value<T>> {
        self.data
            .get_mut(&TypeId::of::<T>())
            // We must be careful to not accidentally cast the box itself as `dyn Any`.
            .map(|val: &mut Box<dyn TypeMappable>| &mut **val)
            .and_then(|val: &mut dyn TypeMappable| (val as &mut dyn Any).downcast_mut())
    }
    pub fn or_default<T: TypeMappable>(&mut self) -> &mut M::Value<T>
    where
        M::Value<T>: Default,
    {
        if self.get::<T>().is_none() {
            self.insert::<T>(Default::default());
        }
        self.get_mut().unwrap()
    }

    pub fn insert<T: TypeMappable>(&mut self, val: M::Value<T>) -> Option<Box<M::Value<T>>> {
        self.data
            .insert(TypeId::of::<T>(), Box::new(val))
            .and_then(|val: Box<dyn TypeMappable>| (val as Box<dyn Any>).downcast().ok())
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
