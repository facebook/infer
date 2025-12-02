//! This file governs the overall translation of items.
//!
//! Translation works as follows: we translate each `TransItemSource` of interest into an
//! appropriate item. In the process of translating an item we may find more `hax::DefId`s of
//! interest; we register those as an appropriate `TransItemSource`, which will 1/ enqueue the item
//! so that it eventually gets translated too, and 2/ return an `AnyTransId` we can use to refer to
//! it.
//!
//! We start with the DefId of the current crate (or of anything passed to `--start-from`) and
//! recursively translate everything we find.
//!
//! There's another important component at play: opacity. Each item is assigned an opacity based on
//! its name. By default, items from the local crate are transparent and items from foreign crates
//! are opaque (this can be controlled with `--include`, `--opaque` and `--exclude`). If an item is
//! opaque, its signature/"outer shell" will be translated (e.g. for functions that's the
//! signature) but not its contents.
use super::translate_ctx::*;
use charon_lib::ast::*;
use charon_lib::options::{CliOpts, TranslateOptions};
use charon_lib::transform::TransformCtx;
use hax_frontend_exporter::{self as hax, SInto};
use itertools::Itertools;
use macros::VariantIndexArity;
use rustc_middle::ty::TyCtxt;
use std::cell::RefCell;
use std::path::PathBuf;

/// The id of an untranslated item. Note that a given `DefId` may show up as multiple different
/// item sources, e.g. a constant will have both a `Global` version (for the constant itself) and a
/// `FunDecl` one (for its initializer function).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TransItemSource {
    pub item: RustcItem,
    pub kind: TransItemSourceKind,
}

/// Refers to a rustc item. Can be either the polymorphic version of the item, or a
/// monomorphization of it.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RustcItem {
    Poly(hax::DefId),
    Mono(hax::ItemRef),
}

/// The kind of a [`TransItemSource`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, VariantIndexArity)]
pub enum TransItemSourceKind {
    Global,
    TraitDecl,
    TraitImpl(TraitImplSource),
    Fun,
    Type,
    /// We don't translate these as proper items, but we translate them a bit in names.
    InherentImpl,
    /// We don't translate these as proper items, but we use them to explore the crate.
    Module,
    /// The `call_*` method of the appropriate `TraitImplSource::Closure` impl.
    ClosureMethod(ClosureKind),
    /// A cast of a state-less closure as a function pointer.
    ClosureAsFnCast,
    /// The `drop` method of a `TraitImplSource::DropGlue` trait impl.
    DropGlueMethod,
    /// The virtual table struct definition for a trait. The `DefId` is that of the trait.
    VTable,
    /// The static vtable value for a specific impl.
    VTableInstance(TraitImplSource),
    /// The initializer function of the `VTableInstance`.
    VTableInstanceInitializer(TraitImplSource),
    /// Shim function to store a method in a vtable; give a method with `self: Ptr<Self>` argument,
    /// this takes a `Ptr<dyn Trait>` and forwards to the method. The `DefId` refers to the method
    /// implementation.
    VTableMethod,
}

/// The kind of a [`TransItemSourceKind::TraitImpl`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, VariantIndexArity)]
pub enum TraitImplSource {
    /// A user-written trait impl with a `DefId`.
    Normal,
    /// The blanket impl we generate for a trait alias. The `DefId` is that of the trait alias.
    TraitAlias,
    /// An impl of the appropriate `Fn*` trait for a closure. The `DefId` is that of the closure.
    Closure(ClosureKind),
    /// A fictitious `impl Drop for T` that contains the drop glue code for the given ADT. The
    /// `DefId` is that of the ADT.
    DropGlue,
}

impl TransItemSource {
    pub fn new(item: RustcItem, kind: TransItemSourceKind) -> Self {
        if let RustcItem::Mono(item) = &item {
            if item.has_param {
                panic!("Item is not monomorphic: {item:?}")
            }
        }
        Self { item, kind }
    }

    /// Refers to the given item. Depending on `monomorphize`, this chooses between the monomorphic
    /// and polymorphic versions of the item.
    pub fn from_item(item: &hax::ItemRef, kind: TransItemSourceKind, monomorphize: bool) -> Self {
        if monomorphize {
            Self::monomorphic(item, kind)
        } else {
            Self::polymorphic(&item.def_id, kind)
        }
    }

    /// Refers to the polymorphic version of this item.
    pub fn polymorphic(def_id: &hax::DefId, kind: TransItemSourceKind) -> Self {
        Self::new(RustcItem::Poly(def_id.clone()), kind)
    }

    /// Refers to the monomorphic version of this item.
    pub fn monomorphic(item: &hax::ItemRef, kind: TransItemSourceKind) -> Self {
        Self::new(RustcItem::Mono(item.clone()), kind)
    }

    pub fn def_id(&self) -> &hax::DefId {
        self.item.def_id()
    }

    /// Keep the same def_id but change the kind.
    pub(crate) fn with_kind(&self, kind: TransItemSourceKind) -> Self {
        let mut ret = self.clone();
        ret.kind = kind;
        ret
    }

    /// For virtual items that have a parent (typically a method impl), return this parent. Does
    /// not attempt to generally compute the parent of an item. Used to compute names.
    pub(crate) fn parent(&self) -> Option<Self> {
        let parent_kind = match self.kind {
            TransItemSourceKind::ClosureMethod(kind) => {
                TransItemSourceKind::TraitImpl(TraitImplSource::Closure(kind))
            }
            TransItemSourceKind::DropGlueMethod => {
                TransItemSourceKind::TraitImpl(TraitImplSource::DropGlue)
            }
            TransItemSourceKind::VTableInstance(impl_kind)
            | TransItemSourceKind::VTableInstanceInitializer(impl_kind) => {
                TransItemSourceKind::TraitImpl(impl_kind)
            }
            _ => return None,
        };
        Some(self.with_kind(parent_kind))
    }

    /// Whether this item is the "main" item for this def_id or not (e.g. drop impl/methods are not
    /// the main item).
    pub(crate) fn is_derived_item(&self) -> bool {
        use TransItemSourceKind::*;
        match self.kind {
            Global
            | TraitDecl
            | TraitImpl(TraitImplSource::Normal)
            | InherentImpl
            | Module
            | Fun
            | Type => false,
            _ => true,
        }
    }

    /// Value with which we order values.
    fn sort_key(&self) -> impl Ord + '_ {
        let item_id = match &self.item {
            RustcItem::Poly(_) => None,
            RustcItem::Mono(item) => Some(item.id()),
        };
        (self.def_id().index, &self.kind, item_id)
    }
}

impl RustcItem {
    pub fn def_id(&self) -> &hax::DefId {
        match self {
            RustcItem::Poly(def_id) => def_id,
            RustcItem::Mono(item_ref) => &item_ref.def_id,
        }
    }
}

/// Manual impls because `DefId` is not orderable.
impl PartialOrd for TransItemSource {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for TransItemSource {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sort_key().cmp(&other.sort_key())
    }
}

impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Returns the default translation kind for the given `DefId`. Returns `None` for items that
    /// we don't translate. Errors on unexpected items.
    pub fn base_kind_for_item(&mut self, def_id: &hax::DefId) -> Option<TransItemSourceKind> {
        use hax::DefKind::*;
        Some(match &def_id.kind {
            Enum { .. } | Struct { .. } | Union { .. } | TyAlias { .. } | ForeignTy => {
                TransItemSourceKind::Type
            }
            Fn { .. } | AssocFn { .. } => TransItemSourceKind::Fun,
            Const { .. } | Static { .. } | AssocConst { .. } => TransItemSourceKind::Global,
            Trait { .. } | TraitAlias { .. } => TransItemSourceKind::TraitDecl,
            Impl { of_trait: true } => TransItemSourceKind::TraitImpl(TraitImplSource::Normal),
            Impl { of_trait: false } => TransItemSourceKind::InherentImpl,
            Mod { .. } | ForeignMod { .. } => TransItemSourceKind::Module,

            // We skip these
            ExternCrate { .. } | GlobalAsm { .. } | Macro { .. } | Use { .. } => return None,
            // We cannot encounter these since they're not top-level items.
            AnonConst { .. }
            | AssocTy { .. }
            | Closure { .. }
            | ConstParam { .. }
            | Ctor { .. }
            | Field { .. }
            | InlineConst { .. }
            | PromotedConst { .. }
            | LifetimeParam { .. }
            | OpaqueTy { .. }
            | SyntheticCoroutineBody { .. }
            | TyParam { .. }
            | Variant { .. } => {
                let span = self.def_span(def_id);
                register_error!(
                    self,
                    span,
                    "Cannot register item `{def_id:?}` with kind `{:?}`",
                    def_id.kind
                );
                return None;
            }
        })
    }

    /// Add this item to the queue of items to translate. Each translated item will then
    /// recursively register the items it refers to. We call this on the crate root and end up
    /// exploring the whole crate.
    #[tracing::instrument(skip(self))]
    pub fn enqueue_module_item(&mut self, def_id: &hax::DefId) {
        let Some(kind) = self.base_kind_for_item(def_id) else {
            return;
        };
        let item_src = if self.options.monomorphize_with_hax
            && let Ok(def) = self.poly_hax_def(def_id)
            && !def.has_any_generics()
        {
            // Monomorphize this item and the items it depends on.
            TransItemSource::monomorphic(def.this(), kind)
        } else {
            TransItemSource::polymorphic(def_id, kind)
        };
        let _: Option<AnyTransId> = self.register_and_enqueue(&None, item_src);
    }

    pub(crate) fn register_no_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        dep_src: &Option<DepSource>,
        src: &TransItemSource,
    ) -> Option<T> {
        let item_id = match self.id_map.get(src) {
            Some(tid) => *tid,
            None => {
                use TransItemSourceKind::*;
                let trans_id = match src.kind {
                    Type | VTable => AnyTransId::Type(self.translated.type_decls.reserve_slot()),
                    TraitDecl => AnyTransId::TraitDecl(self.translated.trait_decls.reserve_slot()),
                    TraitImpl(..) => {
                        AnyTransId::TraitImpl(self.translated.trait_impls.reserve_slot())
                    }
                    Global | VTableInstance(..) => {
                        AnyTransId::Global(self.translated.global_decls.reserve_slot())
                    }
                    Fun
                    | ClosureMethod(..)
                    | ClosureAsFnCast
                    | DropGlueMethod
                    | VTableInstanceInitializer(..)
                    | VTableMethod => AnyTransId::Fun(self.translated.fun_decls.reserve_slot()),
                    InherentImpl | Module => return None,
                };
                // Add the id to the queue of declarations to translate
                self.id_map.insert(src.clone(), trans_id);
                self.reverse_id_map.insert(trans_id, src.clone());
                // Store the name early so the name matcher can identify paths.
                if let Ok(name) = self.translate_name(src) {
                    self.translated.item_names.insert(trans_id, name);
                }
                trans_id
            }
        };
        self.errors
            .borrow_mut()
            .register_dep_source(dep_src, item_id, src.def_id().is_local);
        item_id.try_into().ok()
    }

    /// Register this item source and enqueue it for translation.
    pub(crate) fn register_and_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        dep_src: &Option<DepSource>,
        item_src: TransItemSource,
    ) -> Option<T> {
        let id = self.register_no_enqueue(dep_src, &item_src);
        self.items_to_translate.insert(item_src);
        id
    }

    pub(crate) fn register_target_info(&mut self) {
        let target_data = &self.tcx.data_layout;
        self.translated.target_information = krate::TargetInfo {
            target_pointer_size: target_data.pointer_size().bytes(),
            is_little_endian: matches!(target_data.endian, rustc_abi::Endian::Little),
        }
    }
}

pub struct ItemRef<Id> {
    id: Id,
    generics: BoxedArgs,
}

// Implement `ItemRef<_>` -> `FooDeclRef` conversions.
macro_rules! convert_item_ref {
    ($item_ref_ty:ident($id:ident)) => {
        impl TryFrom<ItemRef<AnyTransId>> for $item_ref_ty {
            type Error = ();
            fn try_from(item: ItemRef<AnyTransId>) -> Result<Self, ()> {
                Ok($item_ref_ty {
                    id: item.id.try_into()?,
                    generics: item.generics,
                })
            }
        }
        impl From<ItemRef<$id>> for $item_ref_ty {
            fn from(item: ItemRef<$id>) -> Self {
                $item_ref_ty {
                    id: item.id,
                    generics: item.generics,
                }
            }
        }
    };
}
convert_item_ref!(TypeDeclRef(TypeId));
convert_item_ref!(FunDeclRef(FunDeclId));
convert_item_ref!(MaybeBuiltinFunDeclRef(FunId));
convert_item_ref!(GlobalDeclRef(GlobalDeclId));
convert_item_ref!(TraitDeclRef(TraitDeclId));
convert_item_ref!(TraitImplRef(TraitImplId));
impl TryFrom<ItemRef<AnyTransId>> for FnPtr {
    type Error = ();
    fn try_from(item: ItemRef<AnyTransId>) -> Result<Self, ()> {
        let id: FunId = item.id.try_into()?;
        Ok(FnPtr {
            func: Box::new(id.into()),
            generics: item.generics,
        })
    }
}

// Id and item reference registration.
impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    pub(crate) fn make_dep_source(&self, span: Span) -> Option<DepSource> {
        Some(DepSource {
            src_id: self.item_id?,
            span: self.item_src.def_id().is_local.then_some(span),
        })
    }

    /// Register this item source and enqueue it for translation.
    pub(crate) fn register_and_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        span: Span,
        item_src: TransItemSource,
    ) -> T {
        let dep_src = self.make_dep_source(span);
        self.t_ctx.register_and_enqueue(&dep_src, item_src).unwrap()
    }

    pub(crate) fn register_no_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        span: Span,
        src: &TransItemSource,
    ) -> T {
        let dep_src = self.make_dep_source(span);
        self.t_ctx.register_no_enqueue(&dep_src, src).unwrap()
    }

    /// Register this item and maybe enqueue it for translation.
    pub(crate) fn register_item_maybe_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        span: Span,
        enqueue: bool,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> T {
        let item = if self.monomorphize() && item.has_param {
            item.erase(&self.hax_state_with_id())
        } else {
            item.clone()
        };
        let item_src = TransItemSource::from_item(&item, kind, self.monomorphize());
        if enqueue {
            self.register_and_enqueue(span, item_src)
        } else {
            self.register_no_enqueue(span, &item_src)
        }
    }

    /// Register this item and enqueue it for translation.
    pub(crate) fn register_item<T: TryFrom<AnyTransId>>(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> T {
        self.register_item_maybe_enqueue(span, true, item, kind)
    }

    /// Register this item without enqueueing it for translation.
    #[expect(dead_code)]
    pub(crate) fn register_item_no_enqueue<T: TryFrom<AnyTransId>>(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> T {
        self.register_item_maybe_enqueue(span, false, item, kind)
    }

    /// Register this item and maybe enqueue it for translation.
    pub(crate) fn translate_item_maybe_enqueue<T: TryFrom<ItemRef<AnyTransId>>>(
        &mut self,
        span: Span,
        enqueue: bool,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> Result<T, Error> {
        let id: AnyTransId = self.register_item_maybe_enqueue(span, enqueue, item, kind);
        let generics = if self.monomorphize() {
            Ok(GenericArgs::empty())
        } else {
            self.translate_generic_args(span, &item.generic_args, &item.impl_exprs)
        }?;
        let item = ItemRef {
            id,
            generics: Box::new(generics),
        };
        Ok(item.try_into().ok().unwrap())
    }

    /// Register this item and enqueue it for translation.
    pub(crate) fn translate_item<T: TryFrom<ItemRef<AnyTransId>>>(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> Result<T, Error> {
        self.translate_item_maybe_enqueue(span, true, item, kind)
    }

    /// Register this item and don't enqueue it for translation.
    pub(crate) fn translate_item_no_enqueue<T: TryFrom<ItemRef<AnyTransId>>>(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
        kind: TransItemSourceKind,
    ) -> Result<T, Error> {
        self.translate_item_maybe_enqueue(span, false, item, kind)
    }

    /// Translate a type def id
    pub(crate) fn translate_type_decl_ref(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<TypeDeclRef, Error> {
        match self.recognize_builtin_type(item)? {
            Some(id) => {
                let generics =
                    self.translate_generic_args(span, &item.generic_args, &item.impl_exprs)?;
                Ok(TypeDeclRef {
                    id: TypeId::Builtin(id),
                    generics: Box::new(generics),
                })
            }
            None => self.translate_item(span, item, TransItemSourceKind::Type),
        }
    }

    /// Translate a function def id
    pub(crate) fn translate_fun_item(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<MaybeBuiltinFunDeclRef, Error> {
        match self.recognize_builtin_fun(item)? {
            Some(id) => {
                let generics =
                    self.translate_generic_args(span, &item.generic_args, &item.impl_exprs)?;
                Ok(MaybeBuiltinFunDeclRef {
                    id: FunId::Builtin(id),
                    generics: Box::new(generics),
                })
            }
            None => self.translate_item(span, item, TransItemSourceKind::Fun),
        }
    }

    /// Auxiliary function to translate function calls and references to functions.
    /// Translate a function id applied with some substitutions.
    #[tracing::instrument(skip(self, span))]
    pub(crate) fn translate_fn_ptr(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<RegionBinder<FnPtr>, Error> {
        let fun_item = self.translate_fun_item(span, item)?;
        let late_bound = match self.hax_def(item)?.kind() {
            hax::FullDefKind::Fn { sig, .. } | hax::FullDefKind::AssocFn { sig, .. } => {
                Some(sig.as_ref().rebind(()))
            }
            _ => None,
        };
        let bound_generics =
            self.append_late_bound_to_generics(span, *fun_item.generics, late_bound)?;
        let fun_id = match &item.in_trait {
            // Direct function call
            None => FunIdOrTraitMethodRef::Fun(fun_item.id),
            // Trait method
            Some(impl_expr) => {
                let trait_ref = self.translate_trait_impl_expr(span, impl_expr)?;
                let name = self.t_ctx.translate_trait_item_name(&item.def_id)?;
                let method_decl_id = *fun_item
                    .id
                    .as_regular()
                    .expect("methods are not builtin functions");
                FunIdOrTraitMethodRef::Trait(trait_ref.move_under_binder(), name, method_decl_id)
            }
        };
        Ok(bound_generics.map(|generics| FnPtr {
            func: Box::new(fun_id),
            generics: Box::new(generics),
        }))
    }

    pub(crate) fn translate_global_decl_ref(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<GlobalDeclRef, Error> {
        self.translate_item(span, item, TransItemSourceKind::Global)
    }

    pub(crate) fn translate_trait_decl_ref(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
    ) -> Result<TraitDeclRef, Error> {
        self.translate_item(span, item, TransItemSourceKind::TraitDecl)
    }

    pub(crate) fn translate_trait_impl_ref(
        &mut self,
        span: Span,
        item: &hax::ItemRef,
        kind: TraitImplSource,
    ) -> Result<TraitImplRef, Error> {
        self.translate_item(span, item, TransItemSourceKind::TraitImpl(kind))
    }
}

#[tracing::instrument(skip(tcx))]
pub fn translate<'tcx, 'ctx>(
    options: &CliOpts,
    tcx: TyCtxt<'tcx>,
    sysroot: PathBuf,
) -> TransformCtx {
    let hax_state = hax::state::State::new(
        tcx,
        hax::options::Options {
            inline_anon_consts: true,
            resolve_drop_bounds: options.add_drop_bounds,
        },
    );

    let crate_def_id: hax::DefId = rustc_span::def_id::CRATE_DEF_ID
        .to_def_id()
        .sinto(&hax_state);
    let crate_name = crate_def_id.krate.clone();
    trace!("# Crate: {}", crate_name);

    let mut error_ctx = ErrorCtx::new(!options.abort_on_error, options.error_on_warnings);
    let translate_options = TranslateOptions::new(&mut error_ctx, options);
    let mut ctx = TranslateCtx {
        tcx,
        sysroot,
        hax_state,
        options: translate_options,
        errors: RefCell::new(error_ctx),
        translated: TranslatedCrate {
            crate_name,
            options: options.clone(),
            ..TranslatedCrate::default()
        },
        id_map: Default::default(),
        reverse_id_map: Default::default(),
        file_to_id: Default::default(),
        items_to_translate: Default::default(),
        processed: Default::default(),
        translate_stack: Default::default(),
        cached_item_metas: Default::default(),
        cached_names: Default::default(),
    };
    ctx.register_target_info();

    if options.start_from.is_empty() {
        // Recursively register all the items in the crate, starting from the crate root.
        ctx.enqueue_module_item(&crate_def_id);
    } else {
        // Start translating from the selected items.
        for path in options.start_from.iter() {
            let path = path.split("::").collect_vec();
            let resolved = super::resolve_path::def_path_def_ids(&ctx.hax_state, &path);
            match resolved {
                Ok(resolved) => {
                    for def_id in resolved {
                        let def_id: hax::DefId = def_id.sinto(&ctx.hax_state);
                        ctx.enqueue_module_item(&def_id);
                    }
                }
                Err(path) => {
                    let path = path.join("::");
                    register_error!(
                        ctx,
                        Span::dummy(),
                        "path {path} does not correspond to any item"
                    );
                }
            }
        }
    }

    trace!(
        "Queue after we explored the crate:\n{:?}",
        &ctx.items_to_translate
    );

    // Translate.
    //
    // For as long as the queue of items to translate is not empty, we pop the top item and
    // translate it. If an item refers to non-translated (potentially external) items, we add them
    // to the queue.
    //
    // Note that the order in which we translate the definitions doesn't matter:
    // we never need to lookup a translated definition, and only use the map
    // from Rust ids to translated ids.
    while let Some(item_src) = ctx.items_to_translate.pop_first() {
        trace!("About to translate item: {:?}", item_src);
        if ctx.processed.insert(item_src.clone()) {
            ctx.translate_item(&item_src);
        }
    }

    // Return the context, dropping the hax state and rustc `tcx`.
    TransformCtx {
        options: ctx.options,
        translated: ctx.translated,
        errors: ctx.errors,
    }
}
