use crate::prelude::*;
use paste::paste;
use rustc_middle::ty::TyCtxt;

macro_rules! mk_aux {
    ($state:ident {$($lts:lifetime)*} $field:ident {$($field_type:tt)+} {$($gen:tt)*} {$($gen_full:tt)*} {$($params:tt)*} {$($fields:tt)*}) => {
        paste ! {
            pub trait [<Has $field:camel>]<$($lts,)*> {
                fn $field(self: &Self) -> $($field_type)+<$($lts)*>;
            }
            impl<$($lts,)*$($gen)*> [<Has $field:camel>]<$($lts,)*> for $state<$($params)*> {
                fn $field(self: &Self) -> $($field_type)+<$($lts)*> {
                    self.$field.clone()
                }
            }
        }
    };
}
macro_rules! mk {
    (struct $state:ident<$($glts:lifetime),*> {$($field:ident : {$($lts:lifetime),*} $field_type:ty),*$(,)?}) => {
        mk!(@$state {} {$($field)*} {$($field: {$($lts),*} {$field_type},)*});
    };
    (@$state:ident {$($acc:tt)*} $fields:tt {
        $field:ident : $lts:tt $field_type:tt
        $(,$($rest:tt)*)?
    }) => {mk!(@$state {
        $($acc)* $fields $field: $lts $field_type,
    } $fields {$($($rest)*)?} );};
    (@$state:ident $body:tt $fields:tt {$(,)?}) => { mk! (@@ $state $body ); };
    (@@$state:ident {$({$($fields:tt)*} $field:ident : {$($lts:lifetime)*} {$($field_type:tt)+},)*}) => {
        paste! {
            #[derive(Clone)]
            pub struct $state<$([<$field:camel>],)*>{
                $(pub $field: [<$field:camel>],)*
            }
        }
        $(
            macro_rules! __inner_helper {
                ($gen:tt {$$($full_gen:tt)*} {$$($params:tt)*} $field $$($rest:tt)*) => {
                    paste! {__inner_helper!(
                        $gen {$$($full_gen)*[<$field:camel>],}
                        {$$($params)*$($field_type)+<$($lts,)*>,} $$($rest)*
                    );}
                };
                ({$$($gen:tt)*} {$$($full_gen:tt)*} {$$($params:tt)*} $i:ident $$($rest:tt)*) => {
                    paste! {__inner_helper!(
                        {$$($gen)*[<$i:camel>],} {$$($full_gen)*[<$i:camel>],}
                        {$$($params)*[<$i:camel>],} $$($rest)*
                    );}
                };
                ($gen:tt $full_gen:tt $params:tt $$(,)?) => {
                    mk_aux!($state {$($lts)*} $field {$($field_type)+} $gen $full_gen $params {$($fields)*});
                };
            }
            __inner_helper!({} {} {} $($fields)*);
        )*
    };
}

mod types {
    use crate::prelude::*;
    use rustc_middle::ty;
    use std::{cell::RefCell, sync::Arc};

    pub struct LocalContextS {
        pub vars: HashMap<rustc_middle::thir::LocalVarId, String>,
    }

    impl Default for LocalContextS {
        fn default() -> Self {
            Self::new()
        }
    }

    impl LocalContextS {
        pub fn new() -> LocalContextS {
            LocalContextS {
                vars: HashMap::new(),
            }
        }
    }

    /// Global caches
    #[derive(Default)]
    pub struct GlobalCache<'tcx> {
        /// Per-item cache.
        pub per_item: HashMap<RDefId, ItemCache<'tcx>>,
        /// Map that recovers rustc args for a given `ItemRef`.
        pub reverse_item_refs_map: HashMap<ItemRef, ty::GenericArgsRef<'tcx>>,
        /// We create some artificial items; their def_ids are stored here. See the
        /// `synthetic_items` module.
        pub synthetic_def_ids: HashMap<SyntheticItem, RDefId>,
        pub reverse_synthetic_map: HashMap<RDefId, SyntheticItem>,
    }

    /// Per-item cache
    #[derive(Default)]
    pub struct ItemCache<'tcx> {
        /// The translated `DefId`.
        pub def_id: Option<DefId>,
        /// The translated definitions, generic in the Body kind.
        /// Each rustc `DefId` gives several hax `DefId`s: one for each promoted constant (if any),
        /// and the base one represented by `None`. Moreover we can instantiate definitions with
        /// generic arguments.
        pub full_defs:
            HashMap<(Option<PromotedId>, Option<ty::GenericArgsRef<'tcx>>), Arc<FullDef>>,
        /// Cache the `Ty` translations.
        pub tys: HashMap<ty::Ty<'tcx>, Ty>,
        /// Cache the `ItemRef` translations. This is fast because `GenericArgsRef` is interned.
        pub item_refs: HashMap<(DefId, ty::GenericArgsRef<'tcx>, bool), ItemRef>,
        /// Cache the trait resolution engine for each item.
        pub predicate_searcher: Option<crate::traits::PredicateSearcher<'tcx>>,
        /// Cache of trait refs to resolved impl expressions.
        pub impl_exprs: HashMap<ty::PolyTraitRef<'tcx>, crate::traits::ImplExpr>,
    }

    #[derive(Clone)]
    pub struct Base<'tcx> {
        pub options: Rc<crate::options::Options>,
        pub local_ctx: Rc<RefCell<LocalContextS>>,
        pub opt_def_id: Option<rustc_hir::def_id::DefId>,
        pub cache: Rc<RefCell<GlobalCache<'tcx>>>,
        pub tcx: ty::TyCtxt<'tcx>,
    }

    impl<'tcx> Base<'tcx> {
        pub fn new(tcx: rustc_middle::ty::TyCtxt<'tcx>, options: crate::options::Options) -> Self {
            Self {
                tcx,
                cache: Default::default(),
                options: Rc::new(options),
                // Always prefer `s.owner_id()` to `s.base().opt_def_id`.
                // `opt_def_id` is used in `utils` for error reporting
                opt_def_id: None,
                local_ctx: Rc::new(RefCell::new(LocalContextS::new())),
            }
        }
    }

    pub type UnitBinder<'tcx> = rustc_middle::ty::Binder<'tcx, ()>;
}

mk!(
    struct State<'tcx> {
        base: {'tcx} types::Base,
        owner: {} DefId,
        binder: {'tcx} types::UnitBinder,
    }
);

pub use self::types::*;

pub type StateWithBase<'tcx> = State<Base<'tcx>, (), ()>;
pub type StateWithOwner<'tcx> = State<Base<'tcx>, DefId, ()>;
pub type StateWithBinder<'tcx> = State<Base<'tcx>, DefId, types::UnitBinder<'tcx>>;

impl<'tcx> StateWithBase<'tcx> {
    pub fn new(tcx: rustc_middle::ty::TyCtxt<'tcx>, options: crate::options::Options) -> Self {
        Self {
            base: Base::new(tcx, options),
            owner: (),
            binder: (),
        }
    }
}

pub trait BaseState<'tcx>: HasBase<'tcx> + Clone {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.base().tcx
    }

    /// Create a state with the given owner.
    fn with_hax_owner(&self, owner: &DefId) -> StateWithOwner<'tcx> {
        let mut base = self.base();
        base.opt_def_id = owner.underlying_rust_def_id();
        State {
            owner: owner.clone(),
            base,
            binder: (),
        }
    }
    /// Create a state with the given owner.
    fn with_rustc_owner(&self, owner_id: RDefId) -> StateWithOwner<'tcx> {
        let owner = &owner_id.sinto(self);
        Self::with_hax_owner(self, owner)
    }
}
impl<'tcx, T: HasBase<'tcx> + Clone> BaseState<'tcx> for T {}

/// State of anything below an `owner`.
pub trait UnderOwnerState<'tcx>: BaseState<'tcx> + HasOwner {
    fn owner_id(&self) -> RDefId {
        self.owner().as_def_id_even_synthetic()
    }
    fn with_base(&self, base: types::Base<'tcx>) -> StateWithOwner<'tcx> {
        State {
            owner: self.owner().clone(),
            base,
            binder: (),
        }
    }
    fn with_binder(&self, binder: types::UnitBinder<'tcx>) -> StateWithBinder<'tcx> {
        State {
            base: self.base(),
            owner: self.owner().clone(),
            binder,
        }
    }
}
impl<'tcx, T: BaseState<'tcx> + HasOwner> UnderOwnerState<'tcx> for T {}

/// State of anything below a binder.
pub trait UnderBinderState<'tcx> = UnderOwnerState<'tcx> + HasBinder<'tcx>;

pub trait WithGlobalCacheExt<'tcx>: BaseState<'tcx> {
    /// Access the global cache. You must not call `sinto` within this function as this will likely
    /// result in `BorrowMut` panics.
    fn with_global_cache<T>(&self, f: impl FnOnce(&mut GlobalCache<'tcx>) -> T) -> T {
        let base = self.base();
        let mut cache = base.cache.borrow_mut();
        f(&mut *cache)
    }
    /// Access the cache for a given item. You must not call `sinto` within this function as this
    /// will likely result in `BorrowMut` panics.
    fn with_item_cache<T>(&self, def_id: RDefId, f: impl FnOnce(&mut ItemCache<'tcx>) -> T) -> T {
        self.with_global_cache(|cache| f(cache.per_item.entry(def_id).or_default()))
    }
}
impl<'tcx, S: BaseState<'tcx>> WithGlobalCacheExt<'tcx> for S {}

pub trait WithItemCacheExt<'tcx>: UnderOwnerState<'tcx> {
    /// Access the cache for the current item. You must not call `sinto` within this function as
    /// this will likely result in `BorrowMut` panics.
    fn with_cache<T>(&self, f: impl FnOnce(&mut ItemCache<'tcx>) -> T) -> T {
        self.with_item_cache(self.owner_id(), f)
    }
    fn with_predicate_searcher<T>(&self, f: impl FnOnce(&mut PredicateSearcher<'tcx>) -> T) -> T {
        self.with_cache(|cache| {
            f(cache.predicate_searcher.get_or_insert_with(|| {
                PredicateSearcher::new_for_owner(
                    self.base().tcx,
                    self.owner_id(),
                    self.base().options.bounds_options,
                )
            }))
        })
    }
}
impl<'tcx, S: UnderOwnerState<'tcx>> WithItemCacheExt<'tcx> for S {}
