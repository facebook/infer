//! The translation contexts.
use super::translate_crate::RustcItem;
pub use super::translate_crate::{TraitImplSource, TransItemSource, TransItemSourceKind};
use super::translate_generics::{BindingLevel, LifetimeMutabilityComputer};
use charon_lib::ast::*;
use charon_lib::formatter::{FmtCtx, IntoFormatter};
use charon_lib::options::TranslateOptions;
use hax::SInto;
use rustc_middle::ty::TyCtxt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;
use std::sync::Arc;
use std::{fmt, mem};

// Re-export to avoid having to fix imports.
pub(crate) use charon_lib::errors::{
    DepSource, ErrorCtx, Level, error_assert, raise_error, register_error,
};

/// Translation context used while translating the crate data into our representation.
pub struct TranslateCtx<'tcx> {
    /// The Rust compiler type context
    pub tcx: TyCtxt<'tcx>,
    /// Path to the toolchain root.
    pub sysroot: PathBuf,
    /// The Hax context
    pub hax_state: hax::StateWithBase<'tcx>,

    /// The options that control translation.
    pub options: TranslateOptions,
    /// The translated data.
    pub translated: TranslatedCrate,

    /// Record data for each method whether it is ever used (called or implemented) and the
    /// `FunDeclId`s of the implementations. We use this to lazily translate methods, so that we
    /// skip unused default methods of large traits like `Iterator`.
    ///
    /// The complete scheme works as follows: by default we enqueue no methods for translation.
    /// When we find a use of a method, we mark it "used" using `mark_method_as_used`. This
    /// enqueues all known and future impls of this method. We also mark a method as used if we
    /// find an implementation of it in a non-opaque impl, and if the method is a required method.
    pub method_status: IndexMap<TraitDeclId, HashMap<TraitItemName, MethodStatus>>,

    /// The map from rustc id to translated id.
    pub id_map: HashMap<TransItemSource, ItemId>,
    /// The reverse map of ids.
    pub reverse_id_map: HashMap<ItemId, TransItemSource>,
    /// The reverse filename map.
    pub file_to_id: HashMap<FileName, FileId>,

    /// Context for tracking and reporting errors.
    pub errors: RefCell<ErrorCtx>,
    /// The declarations we came accross and which we haven't translated yet.
    pub items_to_translate: VecDeque<TransItemSource>,
    /// The declaration we've already processed (successfully or not).
    pub processed: HashSet<TransItemSource>,
    /// Stack of the translations currently happening. Used to avoid accidental cycles.
    pub translate_stack: Vec<ItemId>,
    /// Cache the names to compute them only once each.
    pub cached_names: HashMap<RustcItem, Name>,
    /// Cache the `ItemMeta`s to compute them only once each.
    pub cached_item_metas: HashMap<TransItemSource, ItemMeta>,
    /// Compute which lifetimes are used in a `&'a mut T`. This is a global fixpoint analysis.
    pub lt_mutability_computer: LifetimeMutabilityComputer,
    /// Cache translated dyn trait preshims by generic and associated arguments.
    /// This is used to fetch the unique preshim
    /// when invoking dyn trait methods (see transform_dyn_trait_calls.rs).
    pub translated_preshims: HashSet<(TraitDeclId, Vec<Ty>)>,
}

/// Tracks whether a method is used (i.e. called or (non-opaquely) implemented).
#[derive(Debug)]
pub enum MethodStatus {
    Unused {
        /// The `FunDeclId`s of the method implementations. Because the method is unused, these
        /// items are not enqueued for translation yet. When marking the method as used we'll
        /// enqueue them.
        implementors: HashSet<FunDeclId>,
    },
    Used,
}

impl Default for MethodStatus {
    fn default() -> Self {
        Self::Unused {
            implementors: Default::default(),
        }
    }
}

/// A translation context for items.
/// Augments the [TranslateCtx] with type-level variables.
pub(crate) struct ItemTransCtx<'tcx, 'ctx> {
    /// The definition we are currently extracting.
    pub item_src: TransItemSource,
    /// The id of the definition we are currently extracting, if there is one.
    pub item_id: Option<ItemId>,
    /// The translation context containing the top-level definitions/ids.
    pub t_ctx: &'ctx mut TranslateCtx<'tcx>,
    /// The Hax context with the current `DefId`.
    pub hax_state: hax::StateWithOwner<'tcx>,
    /// Whether to consider a `ImplExprAtom::Error` as an error for us. True except inside type
    /// aliases, because rust does not enforce correct trait bounds on type aliases.
    pub error_on_impl_expr_error: bool,

    /// The stack of generic parameter binders for the current context. Each binder introduces an
    /// entry in this stack, with the entry as index `0` being the innermost binder. These
    /// parameters are referenced using [`DeBruijnVar`]; see there for details.
    pub binding_levels: BindingStack<BindingLevel>,
    /// When `Some`, translate any erased lifetime to a fresh `Region::Body` lifetime.
    pub lifetime_freshener: Option<IndexMap<RegionId, ()>>,
}

/// Translates `T` into `U` using `hax`'s `SInto` trait, catching any hax panics.
pub fn catch_sinto<S, T, U>(
    s: &S,
    err: &mut ErrorCtx,
    krate: &TranslatedCrate,
    span: Span,
    x: &T,
) -> Result<U, Error>
where
    T: Debug + SInto<S, U>,
{
    let unwind_safe_s = std::panic::AssertUnwindSafe(s);
    let unwind_safe_x = std::panic::AssertUnwindSafe(x);
    std::panic::catch_unwind(move || unwind_safe_x.sinto(*unwind_safe_s)).or_else(|_| {
        raise_error!(
            err,
            crate(krate),
            span,
            "Hax panicked when translating `{x:?}`."
        )
    })
}

impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Span an error and register the error.
    pub fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.errors
            .borrow_mut()
            .span_err(&self.translated, span, msg, level)
    }

    /// Translates `T` into `U` using `hax`'s `SInto` trait, catching any hax panics.
    pub fn catch_sinto<S, T, U>(&mut self, s: &S, span: Span, x: &T) -> Result<U, Error>
    where
        T: Debug + SInto<S, U>,
    {
        catch_sinto(s, &mut *self.errors.borrow_mut(), &self.translated, span, x)
    }

    /// Return the polymorphic definition for this item. Use with care, prefer `hax_def` whenever
    /// possible.
    ///
    /// Used for computing names, for associated items, and for various checks.
    pub fn poly_hax_def(&mut self, def_id: &hax::DefId) -> Result<Arc<hax::FullDef>, Error> {
        self.hax_def_for_item(&RustcItem::Poly(def_id.clone()))
    }

    /// Return the definition for this item. This uses the polymorphic or monomorphic definition
    /// depending on user choice.
    pub fn hax_def_for_item(&mut self, item: &RustcItem) -> Result<Arc<hax::FullDef>, Error> {
        let def_id = item.def_id();
        let span = self.def_span(def_id);
        if let RustcItem::Mono(item_ref) = item
            && item_ref.has_non_lt_param
        {
            raise_error!(self, span, "Item is not monomorphic: {item:?}")
        }
        // Hax takes care of caching the translation.
        let unwind_safe_s = std::panic::AssertUnwindSafe(&self.hax_state);
        std::panic::catch_unwind(move || match item {
            RustcItem::Poly(def_id) => def_id.full_def(*unwind_safe_s),
            RustcItem::Mono(item_ref) => item_ref.instantiated_full_def(*unwind_safe_s),
            RustcItem::MonoTrait(def_id) => def_id.full_def(*unwind_safe_s),
        })
        .or_else(|_| raise_error!(self, span, "Hax panicked when translating `{def_id:?}`."))
    }

    pub(crate) fn with_def_id<F, T>(
        &mut self,
        def_id: &hax::DefId,
        item_id: Option<ItemId>,
        f: F,
    ) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let mut errors = self.errors.borrow_mut();
        let current_def_id = mem::replace(&mut errors.def_id, item_id);
        let current_def_id_is_local = mem::replace(&mut errors.def_id_is_local, def_id.is_local());
        drop(errors); // important: release the refcell "lock"
        let ret = f(self);
        let mut errors = self.errors.borrow_mut();
        errors.def_id = current_def_id;
        errors.def_id_is_local = current_def_id_is_local;
        ret
    }
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// Create a new `ExecContext`.
    pub(crate) fn new(
        item_src: TransItemSource,
        item_id: Option<ItemId>,
        t_ctx: &'ctx mut TranslateCtx<'tcx>,
    ) -> Self {
        use hax::BaseState;
        let hax_state_with_id = t_ctx.hax_state.clone().with_hax_owner(&item_src.def_id());
        ItemTransCtx {
            item_src,
            item_id,
            t_ctx,
            hax_state: hax_state_with_id,
            error_on_impl_expr_error: true,
            binding_levels: Default::default(),
            lifetime_freshener: None,
        }
    }

    /// Whether to monomorphize items we encounter.
    pub fn monomorphize(&self) -> bool {
        matches!(
            self.item_src.item,
            RustcItem::Mono(..) | RustcItem::MonoTrait(..)
        )
    }

    pub fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.t_ctx.span_err(span, msg, level)
    }

    pub fn hax_state(&self) -> &hax::StateWithBase<'tcx> {
        &self.t_ctx.hax_state
    }

    pub fn hax_state_with_id(&self) -> &hax::StateWithOwner<'tcx> {
        &self.hax_state
    }

    pub fn catch_sinto<T, U>(&mut self, span: Span, x: &T) -> Result<U, Error>
    where
        T: Debug + SInto<hax::StateWithOwner<'tcx>, U>,
    {
        self.t_ctx.catch_sinto(&self.hax_state, span, x)
    }

    /// Return the definition for this item. This uses the polymorphic or monomorphic definition
    /// depending on user choice. For `TraitDecl` or `VTable`, we always use polymorphic definitions.
    pub fn hax_def(&mut self, item: &hax::ItemRef) -> Result<Arc<hax::FullDef>, Error> {
        let item = if self.monomorphize()
            && !matches!(
                self.item_src.kind,
                TransItemSourceKind::TraitDecl | TransItemSourceKind::VTable
            ) {
            RustcItem::Mono(item.clone())
        } else {
            RustcItem::Poly(item.def_id.clone())
        };
        self.t_ctx.hax_def_for_item(&item)
    }

    pub(crate) fn poly_hax_def(&mut self, def_id: &hax::DefId) -> Result<Arc<hax::FullDef>, Error> {
        self.t_ctx.poly_hax_def(def_id)
    }
}

impl<'tcx> Deref for ItemTransCtx<'tcx, '_> {
    type Target = TranslateCtx<'tcx>;
    fn deref(&self) -> &Self::Target {
        self.t_ctx
    }
}
impl<'tcx> DerefMut for ItemTransCtx<'tcx, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.t_ctx
    }
}

impl<'a> IntoFormatter for &'a TranslateCtx<'_> {
    type C = FmtCtx<'a>;
    fn into_fmt(self) -> Self::C {
        self.translated.into_fmt()
    }
}

impl<'a> IntoFormatter for &'a ItemTransCtx<'_, '_> {
    type C = FmtCtx<'a>;
    fn into_fmt(self) -> Self::C {
        FmtCtx {
            translated: Some(&self.t_ctx.translated),
            generics: self.binding_levels.map_ref(|bl| Cow::Borrowed(&bl.params)),
            locals: None,
            indent_level: 0,
        }
    }
}

impl<'tcx, 'ctx> fmt::Display for TranslateCtx<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.translated.fmt(f)
    }
}
