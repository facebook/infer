use crate::prelude::*;

use itertools::Itertools;
use rustc_hir as hir;
use rustc_hir::def::DefKind as RDefKind;
use rustc_middle::mir;
use rustc_middle::ty;
use rustc_span::def_id::DefId as RDefId;
use std::sync::Arc;

/// Gathers a lot of definition information about a [`rustc_hir::def_id::DefId`].
#[derive(Clone, Debug)]
pub struct FullDef {
    /// A reference to the current item. If the item was provided with generic args, they are
    /// stored here; otherwise the args are the identity_args for this item.
    pub this: ItemRef,
    /// The span of the definition of this item (e.g. for a function this is is signature).
    pub span: Span,
    /// The span of the whole definition (including e.g. the function body).
    pub source_span: Option<Span>,
    /// The text of the whole definition.
    pub source_text: Option<String>,
    /// Attributes on this definition, if applicable.
    pub attributes: Vec<hir::Attribute>,
    /// Visibility of the definition, for definitions where this makes sense.
    pub visibility: Option<bool>,
    /// If this definition is a lang item, we store the identifier, e.g. `sized`.
    pub lang_item: Option<Symbol>,
    /// If this definition is a diagnostic item, we store the identifier, e.g. `box_new`.
    pub diagnostic_item: Option<Symbol>,
    pub kind: FullDefKind,
}

/// Construct the `FullDefKind` for this item. If `args` is `Some`, the returned `FullDef` will be
/// instantiated with the provided generics.
fn translate_full_def<'tcx, S>(
    s: &S,
    def_id: &DefId,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> FullDef
where
    S: UnderOwnerState<'tcx>,
{
    let tcx = s.base().tcx;
    let source_span;
    let lang_item;
    let diagnostic_item;
    let kind;
    match def_id.base {
        DefIdBase::Synthetic(item, rust_def_id) => {
            let adt_kind = match item {
                SyntheticItem::Array => AdtKind::Array,
                SyntheticItem::Slice => AdtKind::Slice,
                SyntheticItem::Tuple(..) => AdtKind::Tuple,
            };
            let param_env = get_param_env(s, args);
            let destruct_impl = {
                let destruct_trait = tcx.lang_items().destruct_trait().unwrap();
                let type_of_self = inst_binder(tcx, s.typing_env(), args, tcx.type_of(rust_def_id));
                virtual_impl_for(s, ty::TraitRef::new(tcx, destruct_trait, [type_of_self]))
            };
            kind = FullDefKind::Adt {
                param_env,
                adt_kind,
                variants: [].into_iter().collect(),
                flags: AdtFlags::AdtFlags {
                    todo: String::new(),
                },
                repr: ReprOptions {
                    int_specified: false,
                    typ: Ty::new(s, TyKind::Int(IntTy::Isize)),
                    align: None,
                    pack: None,
                    flags: Default::default(),
                },
                destruct_impl,
            };

            source_span = None;
            lang_item = Default::default();
            diagnostic_item = Default::default();
        }
        DefIdBase::Promoted(rust_def_id, promoted_id) => {
            let parent_def_id = rust_def_id.sinto(s);
            let parent_def = parent_def_id.full_def_maybe_instantiated(s, args);
            let parent_param_env = parent_def.param_env().unwrap();
            let param_env = ParamEnv {
                generics: TyGenerics {
                    parent: Some(parent_def_id),
                    parent_count: parent_param_env.generics.count_total_params(),
                    params: vec![],
                    has_self: false,
                    has_late_bound_regions: None,
                },
                predicates: GenericPredicates { predicates: vec![] },
                parent: Some(parent_def.this().clone()),
            };
            let body = get_promoted_mir(tcx, rust_def_id, promoted_id);
            source_span = Some(body.span);

            let ty = body.local_decls[rustc_middle::mir::Local::ZERO].ty;
            let ty = substitute(tcx, s.typing_env(), args, ty).sinto(s);
            kind = FullDefKind::Const {
                param_env,
                ty,
                kind: ConstKind::PromotedConst,
                value: None,
            };

            lang_item = Default::default();
            diagnostic_item = Default::default();
        }
        DefIdBase::Real(rust_def_id) => {
            kind = translate_full_def_kind(s, def_id, args);

            source_span = rust_def_id.as_local().map(|ldid| tcx.source_span(ldid));
            lang_item = s
                .base()
                .tcx
                .as_lang_item(rust_def_id)
                .map(|litem| litem.name())
                .sinto(s);
            diagnostic_item = tcx.get_diagnostic_name(rust_def_id).sinto(s);
        }
    }

    let attributes = def_id.attrs(tcx).to_vec();
    let visibility = def_id.visibility(tcx);
    let rust_def_id = def_id.as_def_id_even_synthetic();
    let source_text = source_span
        .filter(|source_span| source_span.ctxt().is_root())
        .and_then(|source_span| tcx.sess.source_map().span_to_snippet(source_span).ok());
    let this = if can_have_generics(tcx, rust_def_id) {
        let args_or_default =
            args.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, rust_def_id));
        ItemRef::translate_from_hax_def_id(s, def_id.clone(), args_or_default)
    } else {
        ItemRef::dummy_without_generics(s, def_id.clone())
    };
    FullDef {
        this,
        span: def_id.def_span(s),
        source_span: source_span.sinto(s),
        source_text,
        attributes,
        visibility,
        lang_item,
        diagnostic_item,
        kind,
    }
}

impl DefId {
    /// Get the span of the definition of this item. This is the span used in diagnostics when
    /// referring to the item.
    pub fn def_span<'tcx>(&self, s: &impl BaseState<'tcx>) -> Span {
        use DefKind::*;
        let tcx = s.base().tcx;
        if let Some(def_id) = self.underlying_rust_def_id() {
            if let ForeignMod = &self.kind {
                // These kind causes `def_span` to panic.
                rustc_span::DUMMY_SP
            } else if let Some(ldid) = def_id.as_local()
                && let hir_id = tcx.local_def_id_to_hir_id(ldid)
                && matches!(tcx.hir_node(hir_id), rustc_hir::Node::Synthetic)
            {
                // Synthetic items (those we create ourselves) make `def_span` panic.
                rustc_span::DUMMY_SP
            } else {
                tcx.def_span(def_id)
            }
        } else {
            rustc_span::DUMMY_SP
        }
        .sinto(s)
    }

    /// Get the full definition of this item.
    pub fn full_def<'tcx, S>(&self, s: &S) -> Arc<FullDef>
    where
        S: BaseState<'tcx>,
    {
        self.full_def_maybe_instantiated(s, None)
    }

    /// Get the full definition of this item, instantiated if `args` is `Some`.
    pub fn full_def_maybe_instantiated<'tcx, S>(
        &self,
        s: &S,
        args: Option<ty::GenericArgsRef<'tcx>>,
    ) -> Arc<FullDef>
    where
        S: BaseState<'tcx>,
    {
        let s = &s.with_hax_owner(self);
        let cache_key = (self.promoted_id(), args);
        if let Some(def) = s.with_cache(|cache| cache.full_defs.get(&cache_key).cloned()) {
            return def;
        }
        let def = Arc::new(translate_full_def(s, self, args));
        s.with_cache(|cache| {
            cache.full_defs.insert(cache_key, def.clone());
        });
        def
    }
}

impl ItemRef {
    /// Get the full definition of the item, instantiated with the provided generics.
    pub fn instantiated_full_def<'tcx, S>(&self, s: &S) -> Arc<FullDef>
    where
        S: BaseState<'tcx>,
    {
        let args = self.rustc_args(s);
        self.def_id.full_def_maybe_instantiated(s, Some(args))
    }

    /// Get the drop glue shim for this. Panics if the `DefKind` isn't appropriate. Drop glue shims
    /// are normally translated by hax when safe to do so (i.e. for mono types). This method can be
    /// used if you know what you're doing and want drop glue for a poly type. This may cause ICEs.
    pub fn drop_glue_shim<'tcx, S>(&self, s: &S) -> mir::Body<'tcx>
    where
        S: BaseState<'tcx>,
    {
        let tcx = s.base().tcx;
        let s = &s.with_hax_owner(&self.def_id);
        let def_id = self.def_id.as_def_id_even_synthetic();
        let args = self.rustc_args(s);
        crate::drop_glue_shim(tcx, def_id, Some(args))
    }

    /// For `FnMut`&`Fn` closures: the MIR for the `call_once` method; it simply calls
    /// `call_mut`.
    pub fn closure_once_shim<'tcx, S>(&self, s: &S) -> Option<mir::Body<'tcx>>
    where
        S: BaseState<'tcx>,
    {
        let tcx = s.base().tcx;
        let s = &s.with_hax_owner(&self.def_id);
        let args = self.rustc_args(s);
        let def_id = self.def_id.real_rust_def_id();
        let closure_ty = inst_binder(tcx, s.typing_env(), Some(args), tcx.type_of(def_id));
        crate::closure_once_shim(tcx, closure_ty)
    }
}

/// The combination of type generics and related predicates.
#[derive(Clone, Debug)]
pub struct ParamEnv {
    /// Generic parameters of the item.
    pub generics: TyGenerics,
    /// Required predicates for the item (see `traits::utils::required_predicates`).
    pub predicates: GenericPredicates,
    /// A reference to the parent of this item, with appropriate args.
    pub parent: Option<ItemRef>,
}

impl ParamEnv {
    pub fn for_each_generics<'tcx>(
        &self,
        s: &impl BaseState<'tcx>,
        f: &mut impl FnMut(&GenericParamDef),
    ) {
        if let Some(parent) = &self.parent {
            let def = parent.def_id.full_def(s);
            def.param_env().unwrap().for_each_generics(s, f);
        }
        for param in &self.generics.params {
            f(param)
        }
    }
}

/// The kind of a constant item.
#[derive(Clone, Debug)]
pub enum ConstKind {
    /// Top-level constant: `const CONST: usize = 42;`
    TopLevel,
    /// Anonymous constant, e.g. the `1 + 2` in `[u8; 1 + 2]`
    AnonConst,
    /// An inline constant, e.g. `const { 1 + 2 }`
    InlineConst,
    /// A promoted constant, e.g. the `1 + 2` in `&(1 + 2)`
    PromotedConst,
}

/// Reflects [`rustc_hir::attrs::InlineAttr`]
#[derive(AdtInto, Clone, Debug, Hash, PartialEq, Eq)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_hir::attrs::InlineAttr, state: S as _s)]
pub enum InlineAttr {
    None,
    Hint,
    Always,
    Never,
    Force {
        attr_span: Span,
        reason: Option<Symbol>,
    },
}

/// Imbues [`rustc_hir::def::DefKind`] with a lot of extra information.

#[derive(Clone, Debug)]
pub enum FullDefKind {
    // Types
    /// ADts (`Struct`, `Enum` and `Union` map to this variant).
    Adt {
        param_env: ParamEnv,
        adt_kind: AdtKind,
        variants: IndexVec<VariantIdx, VariantDef>,
        flags: AdtFlags,
        repr: ReprOptions,
        /// Info required to construct a virtual `Drop` impl for this adt.
        destruct_impl: Box<VirtualTraitImpl>,
    },
    /// Type alias: `type Foo = Bar;`
    TyAlias {
        param_env: ParamEnv,
        ty: Ty,
    },
    /// Type from an `extern` block.
    ForeignTy,
    /// Associated type: `trait MyTrait { type Assoc; }`
    AssocTy {
        param_env: ParamEnv,
        implied_predicates: GenericPredicates,
        associated_item: AssocItem,
        /// The value for this associated type, along with proofs of the required predicates. If
        /// we're in a trait decl, this has a value iff the type has a default; if we're in a trait
        /// impl, this has a value iff the impl provides its own value for it.
        value: Option<(Ty, Vec<ImplExpr>)>,
    },
    /// Opaque type, aka `impl Trait`.
    OpaqueTy,

    // Traits
    Trait {
        param_env: ParamEnv,
        implied_predicates: GenericPredicates,
        /// The special `Self: Trait` clause.
        self_predicate: TraitPredicate,
        /// Associated items, in definition order.
        items: Vec<AssocItem>,
        /// `dyn Trait<Args.., Ty = <Self as Trait>::Ty..>` for this trait. This is `Some` iff this
        /// trait is dyn-compatible.
        dyn_self: Option<Ty>,
    },
    /// Trait alias: `trait IntIterator = Iterator<Item = i32>;`
    TraitAlias {
        param_env: ParamEnv,
        implied_predicates: GenericPredicates,
        /// The special `Self: Trait` clause.
        self_predicate: TraitPredicate,
        /// `dyn Trait<Args.., Ty = <Self as Trait>::Ty..>` for this trait. This is `Some` iff this
        /// trait is dyn-compatible.
        dyn_self: Option<Ty>,
    },
    TraitImpl {
        param_env: ParamEnv,
        /// The trait that is implemented by this impl block.
        trait_pred: TraitPredicate,
        /// `dyn Trait<Args.., Ty = <Self as Trait>::Ty..>` for the implemented trait. This is
        /// `Some` iff the trait is dyn-compatible.
        dyn_self: Option<Ty>,
        /// The `ImplExpr`s required to satisfy the predicates on the trait declaration. E.g.:
        /// ```ignore
        /// trait Foo: Bar {}
        /// impl Foo for () {} // would supply an `ImplExpr` for `Self: Bar`.
        /// ```
        implied_impl_exprs: Vec<ImplExpr>,
        /// Associated items, in the order of the trait declaration. Includes defaulted items.
        items: Vec<ImplAssocItem>,
    },
    InherentImpl {
        param_env: ParamEnv,
        /// The type to which this block applies.
        ty: Ty,
        /// Associated items, in definition order.
        items: Vec<AssocItem>,
    },

    // Functions
    Fn {
        param_env: ParamEnv,
        inline: InlineAttr,
        is_const: bool,
        sig: PolyFnSig,
    },
    /// Associated function: `impl MyStruct { fn associated() {} }` or `trait Foo { fn associated()
    /// {} }`
    AssocFn {
        param_env: ParamEnv,
        associated_item: AssocItem,
        inline: InlineAttr,
        is_const: bool,
        /// The function signature when this method is used in a vtable. `None` if this method is not
        /// vtable safe. `Some(sig)` if it is vtable safe, where `sig` is the trait method declaration's
        /// signature with `Self` replaced by `dyn Trait` and associated types normalized.
        vtable_sig: Option<PolyFnSig>,
        sig: PolyFnSig,
    },
    /// A closure, coroutine, or coroutine-closure.
    Closure {
        /// This param env is empty because the (early-bound) generics of a closure are the same as
        /// those of the item in which it is defined. We hide the special weird generics that rustc
        /// uses internally for inference on closures.
        param_env: ParamEnv,
        args: ClosureArgs,
        is_const: bool,
        inline: InlineAttr,
        /// Info required to construct a virtual `FnOnce` impl for this closure.
        fn_once_impl: Box<VirtualTraitImpl>,
        /// Info required to construct a virtual `FnMut` impl for this closure.
        fn_mut_impl: Option<Box<VirtualTraitImpl>>,
        /// Info required to construct a virtual `Fn` impl for this closure.
        fn_impl: Option<Box<VirtualTraitImpl>>,
        /// Info required to construct a virtual `Drop` impl for this closure.
        destruct_impl: Box<VirtualTraitImpl>,
        /// The signature of the `call_once` method.
        call_once_sig: PolyFnSig,
        /// The signature of the `call_mut` method, if applicable.
        call_mut_sig: Option<PolyFnSig>,
        /// The signature of the `call` method, if applicable.
        call_sig: Option<PolyFnSig>,
        /// The signature of the `call_mut` method, if applicable, with `Self` replaced by `dyn
        /// Trait` (like vtable_sig in `AssocFn`).
        call_mut_vtable_sig: Option<PolyFnSig>,
        /// The signature of the `call` method, if applicable, with `Self` replaced by `dyn
        /// Trait` (like vtable_sig in `AssocFn`).
        call_vtable_sig: Option<PolyFnSig>,
    },

    // Constants
    Const {
        param_env: ParamEnv,
        ty: Ty,
        kind: ConstKind,
        value: Option<ConstantExpr>,
    },
    /// Associated constant: `trait MyTrait { const ASSOC: usize; }`
    AssocConst {
        param_env: ParamEnv,
        associated_item: AssocItem,
        ty: Ty,
        value: Option<ConstantExpr>,
    },
    Static {
        param_env: ParamEnv,
        /// Whether it's a `unsafe static`, `safe static` (inside extern only) or just a `static`.
        safety: Safety,
        /// Whether it's a `static mut` or just a `static`.
        mutability: Mutability,
        /// Whether it's an anonymous static generated for nested allocations.
        nested: bool,
        ty: Ty,
    },

    // Crates and modules
    ExternCrate,
    Use,
    Mod {
        items: Vec<(Option<Ident>, DefId)>,
    },
    /// An `extern` block.
    ForeignMod {
        items: Vec<DefId>,
    },

    // Type-level parameters
    /// Type parameter: the `T` in `struct Vec<T> { ... }`
    TyParam,
    /// Constant generic parameter: `struct Foo<const N: usize> { ... }`
    ConstParam,
    /// Lifetime parameter: the `'a` in `struct Foo<'a> { ... }`
    LifetimeParam,

    // ADT parts
    /// Refers to the variant definition, [`DefKind::Ctor`] refers to its constructor if it exists.
    Variant,
    /// The constructor function of a tuple/unit struct or tuple/unit enum variant.
    Ctor {
        adt_def_id: DefId,
        ctor_of: CtorOf,
        variant_id: VariantIdx,
        fields: IndexVec<FieldIdx, FieldDef>,
        output_ty: Ty,
    },
    /// A field in a struct, enum or union. e.g.
    /// - `bar` in `struct Foo { bar: u8 }`
    /// - `Foo::Bar::0` in `enum Foo { Bar(u8) }`
    Field,

    // Others
    /// Macros
    Macro(hir::def::MacroKinds),
    /// A use of `global_asm!`.
    GlobalAsm,
    /// A synthetic coroutine body created by the lowering of a coroutine-closure, such as an async
    /// closure.
    SyntheticCoroutineBody,
}

fn gen_vtable_sig<'tcx>(
    // The state that owns the method DefId
    s: &impl UnderOwnerState<'tcx>,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> Option<PolyFnSig> {
    let method_def_id = s.owner_id();
    let tcx = s.base().tcx;
    let assoc_item = tcx.associated_item(method_def_id);
    let container_id = assoc_item.container_id(tcx);

    // Get the original trait method id.
    let method_decl_id = match assoc_item.container {
        ty::AssocContainer::TraitImpl(Ok(id)) => id,
        ty::AssocContainer::Trait => method_def_id,
        _ => return None,
    };
    let trait_id = tcx.trait_of_assoc(method_decl_id)?;

    let decl_assoc_item = tcx.associated_item(method_decl_id);
    if !rustc_trait_selection::traits::is_vtable_safe_method(tcx, trait_id, decl_assoc_item) {
        return None;
    }

    // Move into the context of the container (trait decl or impl) instead of the method.
    let s = &s.with_rustc_owner(container_id);
    let args = {
        let container_generics = tcx.generics_of(container_id);
        args.map(|args| args.truncate_to(tcx, container_generics))
    };

    let dyn_self = match assoc_item.container {
        ty::AssocContainer::Trait => get_trait_decl_dyn_self_ty(s, args),
        ty::AssocContainer::TraitImpl(..) => {
            // For impl methods, compute concrete dyn_self from the impl's trait reference
            let impl_def_id = assoc_item.container_id(tcx);
            let impl_trait_ref = tcx.impl_trait_ref(impl_def_id);
            // Get the concrete trait reference by rebasing the impl's trait ref args onto `container_args`
            let concrete_trait_ref = inst_binder(tcx, s.typing_env(), args, impl_trait_ref);
            dyn_self_ty(tcx, s.typing_env(), concrete_trait_ref)
        }
        ty::AssocContainer::InherentImpl => {
            unreachable!()
        }
    }?;

    // dyn_self is of form `dyn Trait<Args...>`, we extract the trait args
    let ty::Dynamic(preds, _) = dyn_self.kind() else {
        panic!("Unexpected dyn_self: {:?}", dyn_self);
    };
    // Safe to use `skip_binder` because we know the predicate we built in dyn_self_ty has no bound
    // vars.
    let ty::ExistentialPredicate::Trait(trait_ref) = preds[0].skip_binder() else {
        panic!("No principal trait found in dyn_self: {:?}", dyn_self);
    };

    // Build a full list of args for the trait: dyn_self + trait args
    // Note: trait_ref.args doesn't include Self (it's existential), so we prepend dyn_self
    let mut full_args = vec![ty::GenericArg::from(dyn_self)];
    full_args.extend(trait_ref.args.iter());
    let trait_args = tcx.mk_args(&full_args);

    // Instantiate and normalize the signature.
    let method_decl_sig = tcx.fn_sig(method_decl_id).instantiate(tcx, trait_args);
    let normalized_sig = normalize(tcx, s.typing_env(), method_decl_sig);

    Some(normalized_sig.sinto(s))
}

fn gen_closure_sig<'tcx>(
    // The state that owns the method DefId
    s: &impl UnderOwnerState<'tcx>,
    // The `Fn`/`FnMut`/`FnOnce` trait reference of the closure
    tref: Option<ty::TraitRef<'tcx>>,
    // Whether to replace the `Self` type of the trait with `dyn TheTrait`
    dyn_self: bool,
) -> Option<PolyFnSig> {
    let tref = tref?;
    let tcx = s.base().tcx;

    // Get AssocItems of `Fn` or `FnMut`
    let assoc_item = tcx.associated_items(tref.def_id);
    // Pick `call`/`call_mut`/`call_once`.
    let call_method = assoc_item
        .in_definition_order()
        .filter(|item| matches!(item.kind, ty::AssocKind::Fn { .. }))
        .exactly_one()
        .ok()
        .unwrap();
    // Get its signature
    let sig = tcx.fn_sig(call_method.def_id);
    let trait_args = if dyn_self {
        // Generate type of shim receiver
        let dyn_self = dyn_self_ty(tcx, s.typing_env(), tref).unwrap();
        // Construct signature with dyn_self
        let mut full_args = vec![ty::GenericArg::from(dyn_self)];
        full_args.extend(tref.args[1..].iter());
        tcx.mk_args(&full_args)
    } else {
        tref.args
    };

    // Instantiate and normalize the signature.
    let sig = sig.instantiate(tcx, trait_args);
    let sig = normalize(tcx, s.typing_env(), sig);

    Some(sig.sinto(s))
}

/// Construct the `FullDefKind` for this item.
///
/// If `args` is `Some`, instantiate the whole definition with these generics; otherwise keep the
/// polymorphic definition.
// Note: this is tricky to get right, we have to make sure to isntantiate every single field that
// may contain a type/const/trait reference.
fn translate_full_def_kind<'tcx, S>(
    s: &S,
    def_id: &DefId,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> FullDefKind
where
    S: BaseState<'tcx>,
{
    let s = &s.with_hax_owner(def_id);
    let def_id = def_id.real_rust_def_id();
    let tcx = s.base().tcx;
    let type_of_self = || inst_binder(tcx, s.typing_env(), args, tcx.type_of(def_id));
    let args_or_default =
        || args.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, def_id));
    match get_def_kind(tcx, def_id) {
        RDefKind::Struct { .. } | RDefKind::Union { .. } | RDefKind::Enum { .. } => {
            let def = tcx.adt_def(def_id);
            let variants = def
                .variants()
                .iter_enumerated()
                .map(|(variant_idx, variant)| {
                    let discr = if def.is_enum() {
                        def.discriminant_for_variant(tcx, variant_idx)
                    } else {
                        // Structs and unions have a single variant.
                        assert_eq!(variant_idx.index(), 0);
                        ty::util::Discr {
                            val: 0,
                            ty: tcx.types.isize,
                        }
                    };
                    VariantDef::sfrom(s, variant, discr, args)
                })
                .collect();

            let destruct_trait = tcx.lang_items().destruct_trait().unwrap();
            FullDefKind::Adt {
                param_env: get_param_env(s, args),
                adt_kind: def.adt_kind().sinto(s),
                variants,
                flags: def.flags().sinto(s),
                repr: def.repr().sinto(s),
                destruct_impl: virtual_impl_for(
                    s,
                    ty::TraitRef::new(tcx, destruct_trait, [type_of_self()]),
                ),
            }
        }
        RDefKind::TyAlias { .. } => FullDefKind::TyAlias {
            param_env: get_param_env(s, args),
            ty: type_of_self().sinto(s),
        },
        RDefKind::ForeignTy => FullDefKind::ForeignTy,
        RDefKind::AssocTy { .. } => FullDefKind::AssocTy {
            param_env: get_param_env(s, args),
            implied_predicates: get_implied_predicates(s, args),
            associated_item: AssocItem::sfrom_instantiated(s, &tcx.associated_item(def_id), args),
            value: if tcx.defaultness(def_id).has_value() {
                let ty = type_of_self();
                let args = args_or_default();
                let impl_exprs = solve_item_implied_traits(s, def_id, args);
                Some((ty.sinto(s), impl_exprs))
            } else {
                None
            },
        },
        RDefKind::OpaqueTy => FullDefKind::OpaqueTy,
        RDefKind::Trait { .. } => FullDefKind::Trait {
            param_env: get_param_env(s, args),
            implied_predicates: get_implied_predicates(s, args),
            self_predicate: get_self_predicate(s, args),
            dyn_self: get_trait_decl_dyn_self_ty(s, args).sinto(s),
            items: tcx
                .associated_items(def_id)
                .in_definition_order()
                .map(|assoc| {
                    let item_args = args.map(|args| {
                        let item_identity_args =
                            ty::GenericArgs::identity_for_item(tcx, assoc.def_id);
                        let item_args = item_identity_args.rebase_onto(tcx, def_id, args);
                        tcx.mk_args(item_args)
                    });
                    AssocItem::sfrom_instantiated(s, assoc, item_args)
                })
                .collect::<Vec<_>>(),
        },
        RDefKind::TraitAlias { .. } => FullDefKind::TraitAlias {
            param_env: get_param_env(s, args),
            implied_predicates: get_implied_predicates(s, args),
            self_predicate: get_self_predicate(s, args),
            dyn_self: get_trait_decl_dyn_self_ty(s, args).sinto(s),
        },
        RDefKind::Impl { of_trait, .. } => {
            use std::collections::HashMap;
            let param_env = get_param_env(s, args);
            if !of_trait {
                let ty = tcx.type_of(def_id);
                let ty = inst_binder(tcx, s.typing_env(), args, ty);
                let items = tcx
                    .associated_items(def_id)
                    .in_definition_order()
                    .map(|assoc| {
                        let item_args = args.map(|args| {
                            let item_identity_args =
                                ty::GenericArgs::identity_for_item(tcx, assoc.def_id);
                            let item_args = item_identity_args.rebase_onto(tcx, def_id, args);
                            tcx.mk_args(item_args)
                        });
                        AssocItem::sfrom_instantiated(s, assoc, item_args)
                    })
                    .collect::<Vec<_>>();
                FullDefKind::InherentImpl {
                    param_env,
                    ty: ty.sinto(s),
                    items,
                }
            } else {
                let trait_ref = tcx.impl_trait_ref(def_id);
                let trait_ref = inst_binder(tcx, s.typing_env(), args, trait_ref);
                let polarity = tcx.impl_polarity(def_id);
                let trait_pred = TraitPredicate {
                    trait_ref: trait_ref.sinto(s),
                    is_positive: matches!(polarity, ty::ImplPolarity::Positive),
                };
                let dyn_self = dyn_self_ty(tcx, s.typing_env(), trait_ref).sinto(s);
                // Impl exprs required by the trait.
                let required_impl_exprs =
                    solve_item_implied_traits(s, trait_ref.def_id, trait_ref.args);

                let mut item_map: HashMap<RDefId, _> = tcx
                    .associated_items(def_id)
                    .in_definition_order()
                    .map(|assoc| (assoc.trait_item_def_id().unwrap(), assoc))
                    .collect();
                let items = tcx
                    .associated_items(trait_ref.def_id)
                    .in_definition_order()
                    .map(|decl_assoc| {
                        let decl_def_id = decl_assoc.def_id;
                        // Impl exprs required by the item.
                        let required_impl_exprs;
                        let value = match item_map.remove(&decl_def_id) {
                            Some(impl_assoc) => {
                                required_impl_exprs = {
                                    let item_args =
                                        ty::GenericArgs::identity_for_item(tcx, impl_assoc.def_id);
                                    // Subtlety: we have to add the GAT arguments (if any) to the trait ref arguments.
                                    let args = item_args.rebase_onto(tcx, def_id, trait_ref.args);
                                    let state_with_id = s.with_rustc_owner(impl_assoc.def_id);
                                    solve_item_implied_traits(&state_with_id, decl_def_id, args)
                                };

                                ImplAssocItemValue::Provided {
                                    def_id: impl_assoc.def_id.sinto(s),
                                    is_override: decl_assoc.defaultness(tcx).has_value(),
                                }
                            }
                            None => {
                                required_impl_exprs = if tcx.generics_of(decl_def_id).is_own_empty()
                                {
                                    // Non-GAT case.
                                    let item_args =
                                        ty::GenericArgs::identity_for_item(tcx, decl_def_id);
                                    let args = item_args.rebase_onto(tcx, def_id, trait_ref.args);
                                    // TODO: is it the right `def_id`?
                                    let state_with_id = s.with_rustc_owner(def_id);
                                    solve_item_implied_traits(&state_with_id, decl_def_id, args)
                                } else {
                                    // FIXME: For GATs, we need a param_env that has the arguments of
                                    // the impl plus those of the associated type, but there's no
                                    // def_id with that param_env.
                                    vec![]
                                };
                                match decl_assoc.kind {
                                    ty::AssocKind::Type { .. } => {
                                        let ty = if tcx.generics_of(decl_def_id).is_own_empty() {
                                            let ty = tcx
                                                .type_of(decl_def_id)
                                                .instantiate(tcx, trait_ref.args)
                                                .sinto(s);
                                            Some(ty)
                                        } else {
                                            None
                                        };
                                        ImplAssocItemValue::DefaultedTy { ty }
                                    }
                                    ty::AssocKind::Fn { .. } => {
                                        let sig = if tcx.generics_of(decl_def_id).is_own_empty() {
                                            // The method doesn't have generics of its own, so
                                            // we can instantiate it with just the trait
                                            // generics.
                                            let sig = tcx
                                                .fn_sig(decl_def_id)
                                                .instantiate(tcx, trait_ref.args)
                                                .sinto(s);
                                            Some(sig)
                                        } else {
                                            None
                                        };
                                        ImplAssocItemValue::DefaultedFn { sig }
                                    }
                                    ty::AssocKind::Const { .. } => {
                                        ImplAssocItemValue::DefaultedConst {}
                                    }
                                }
                            }
                        };

                        ImplAssocItem {
                            name: decl_assoc.opt_name().sinto(s),
                            value,
                            required_impl_exprs,
                            decl_def_id: decl_def_id.sinto(s),
                        }
                    })
                    .collect();
                assert!(item_map.is_empty());
                FullDefKind::TraitImpl {
                    param_env,
                    trait_pred,
                    dyn_self,
                    implied_impl_exprs: required_impl_exprs,
                    items,
                }
            }
        }
        RDefKind::Fn { .. } => FullDefKind::Fn {
            param_env: get_param_env(s, args),
            inline: tcx.codegen_fn_attrs(def_id).inline.sinto(s),
            is_const: tcx.constness(def_id) == rustc_hir::Constness::Const,
            sig: inst_binder(tcx, s.typing_env(), args, tcx.fn_sig(def_id)).sinto(s),
        },
        RDefKind::AssocFn { .. } => {
            let item = tcx.associated_item(def_id);
            FullDefKind::AssocFn {
                param_env: get_param_env(s, args),
                associated_item: AssocItem::sfrom_instantiated(s, &item, args),
                inline: tcx.codegen_fn_attrs(def_id).inline.sinto(s),
                is_const: tcx.constness(def_id) == rustc_hir::Constness::Const,
                vtable_sig: gen_vtable_sig(s, args),
                sig: get_method_sig(tcx, s.typing_env(), def_id, args).sinto(s),
            }
        }
        RDefKind::Closure { .. } => {
            use ty::ClosureKind::{Fn, FnMut};
            let closure_ty = type_of_self();
            let ty::TyKind::Closure(_, closure_args) = closure_ty.kind() else {
                unreachable!()
            };
            let closure = closure_args.as_closure();
            // We lose lifetime information here. Eventually would be nice not to.
            let input_ty = erase_free_regions(tcx, closure.sig().input(0).skip_binder());
            let trait_args = [closure_ty, input_ty];
            let fn_once_trait = tcx.lang_items().fn_once_trait().unwrap();
            let fn_mut_trait = tcx.lang_items().fn_mut_trait().unwrap();
            let fn_trait = tcx.lang_items().fn_trait().unwrap();
            let destruct_trait = tcx.lang_items().destruct_trait().unwrap();

            let fn_once_tref = ty::TraitRef::new(tcx, fn_once_trait, trait_args);
            let fn_mut_tref = matches!(closure.kind(), FnMut | Fn)
                .then(|| ty::TraitRef::new(tcx, fn_mut_trait, trait_args));
            let fn_tref =
                matches!(closure.kind(), Fn).then(|| ty::TraitRef::new(tcx, fn_trait, trait_args));

            FullDefKind::Closure {
                param_env: get_param_env(s, args),
                is_const: tcx.constness(def_id) == rustc_hir::Constness::Const,
                inline: tcx.codegen_fn_attrs(def_id).inline.sinto(s),
                args: ClosureArgs::sfrom(s, def_id, closure_args),
                destruct_impl: virtual_impl_for(
                    s,
                    ty::TraitRef::new(tcx, destruct_trait, [type_of_self()]),
                ),
                fn_once_impl: virtual_impl_for(s, fn_once_tref),
                fn_mut_impl: fn_mut_tref.map(|tref| virtual_impl_for(s, tref)),
                fn_impl: fn_tref.map(|tref| virtual_impl_for(s, tref)),
                call_mut_vtable_sig: gen_closure_sig(s, fn_mut_tref, true),
                call_vtable_sig: gen_closure_sig(s, fn_tref, true),
                call_once_sig: gen_closure_sig(s, Some(fn_once_tref), false).unwrap(),
                call_mut_sig: gen_closure_sig(s, fn_mut_tref, false),
                call_sig: gen_closure_sig(s, fn_tref, false),
            }
        }
        kind @ (RDefKind::Const { .. }
        | RDefKind::AnonConst { .. }
        | RDefKind::InlineConst { .. }) => {
            let kind = match kind {
                RDefKind::Const { .. } => ConstKind::TopLevel,
                RDefKind::AnonConst { .. } => ConstKind::AnonConst,
                RDefKind::InlineConst { .. } => ConstKind::InlineConst,
                _ => unreachable!(),
            };

            let self_ty = if matches!(kind, ConstKind::InlineConst)
                && let get_ret_ty = (|body: &mir::Body<'tcx>| body.local_decls[mir::Local::ZERO].ty)
                && let Some(ret_ty) = mir_kinds::CTFE::get_mir(tcx, def_id, get_ret_ty)
                    .or_else(|| mir_kinds::Optimized::get_mir(tcx, def_id, get_ret_ty))
            {
                // Inline consts have a special `<const_ty>` param added to them for type inference
                // purposes. `tcx.type_of` returns that, which is not useful to us. Instead, we get
                // the real type from the MIR body which is sad but works.
                inst_binder(tcx, s.typing_env(), args, ty::EarlyBinder::bind(ret_ty))
            } else {
                type_of_self()
            };
            FullDefKind::Const {
                param_env: get_param_env(s, args),
                ty: self_ty.sinto(s),
                kind,
                value: const_value(s, def_id, args_or_default()),
            }
        }
        RDefKind::AssocConst { .. } => FullDefKind::AssocConst {
            param_env: get_param_env(s, args),
            associated_item: AssocItem::sfrom_instantiated(s, &tcx.associated_item(def_id), args),
            ty: type_of_self().sinto(s),
            value: const_value(s, def_id, args_or_default()),
        },
        RDefKind::Static {
            safety,
            mutability,
            nested,
            ..
        } => FullDefKind::Static {
            param_env: get_param_env(s, args),
            safety: safety.sinto(s),
            mutability: mutability.sinto(s),
            nested: nested.sinto(s),
            ty: type_of_self().sinto(s),
        },
        RDefKind::ExternCrate => FullDefKind::ExternCrate,
        RDefKind::Use => FullDefKind::Use,
        RDefKind::Mod { .. } => FullDefKind::Mod {
            items: get_mod_children(tcx, def_id).sinto(s),
        },
        RDefKind::ForeignMod { .. } => FullDefKind::ForeignMod {
            items: get_foreign_mod_children(tcx, def_id).sinto(s),
        },
        RDefKind::TyParam => FullDefKind::TyParam,
        RDefKind::ConstParam => FullDefKind::ConstParam,
        RDefKind::LifetimeParam => FullDefKind::LifetimeParam,
        RDefKind::Variant => FullDefKind::Variant,
        RDefKind::Ctor(ctor_of, _) => {
            let args = args_or_default();
            let ctor_of = ctor_of.sinto(s);

            // The def_id of the adt this ctor belongs to.
            let adt_def_id = match ctor_of {
                CtorOf::Struct => tcx.parent(def_id),
                CtorOf::Variant => tcx.parent(tcx.parent(def_id)),
            };
            let adt_def = tcx.adt_def(adt_def_id);
            let variant_id = adt_def.variant_index_with_ctor_id(def_id);
            let fields = adt_def
                .variant(variant_id)
                .fields
                .iter()
                .map(|f| FieldDef::sfrom(s, f, args))
                .collect();
            let output_ty = ty::Ty::new_adt(tcx, adt_def, args).sinto(s);
            FullDefKind::Ctor {
                adt_def_id: adt_def_id.sinto(s),
                ctor_of,
                variant_id: variant_id.sinto(s),
                fields,
                output_ty,
            }
        }
        RDefKind::Field => FullDefKind::Field,
        RDefKind::Macro(kinds) => FullDefKind::Macro(kinds),
        RDefKind::GlobalAsm => FullDefKind::GlobalAsm,
        RDefKind::SyntheticCoroutineBody => FullDefKind::SyntheticCoroutineBody,
    }
}

/// An associated item in a trait impl. This can be an item provided by the trait impl, or an item
/// that reuses the trait decl default value.

#[derive(Clone, Debug)]
pub struct ImplAssocItem {
    /// This is `None` for RPTITs.
    pub name: Option<Symbol>,
    /// The definition of the item from the trait declaration. This is an `AssocTy`, `AssocFn` or
    /// `AssocConst`.
    pub decl_def_id: DefId,
    /// The `ImplExpr`s required to satisfy the predicates on the associated type. E.g.:
    /// ```ignore
    /// trait Foo {
    ///     type Type<T>: Clone,
    /// }
    /// impl Foo for () {
    ///     type Type<T>: Arc<T>; // would supply an `ImplExpr` for `Arc<T>: Clone`.
    /// }
    /// ```
    /// Empty if this item is an associated const or fn.
    pub required_impl_exprs: Vec<ImplExpr>,
    /// The value of the implemented item.
    pub value: ImplAssocItemValue,
}

#[derive(Clone, Debug)]
pub enum ImplAssocItemValue {
    /// The item is provided by the trait impl.
    Provided {
        /// The definition of the item in the trait impl. This is an `AssocTy`, `AssocFn` or
        /// `AssocConst`.
        def_id: DefId,
        /// Whether the trait had a default value for this item (which is therefore overriden).
        is_override: bool,
    },
    /// This is an associated type that reuses the trait declaration default.
    DefaultedTy {
        /// The default type, with generics properly instantiated. `None` if the type has generics
        /// of its own, because then we'd need to resolve traits but the type doesn't have its own
        /// `DefId`.
        ty: Option<Ty>,
    },
    /// This is a non-overriden default method.
    /// FIXME: provide properly instantiated generics.
    DefaultedFn {
        /// The signature of the method, if we could translate it. `None` if the method has
        /// generics of its own, because then we'd need to resolve traits but the method doesn't
        /// have its own `DefId`.
        sig: Option<PolyFnSig>,
    },
    /// This is an associated const that reuses the trait declaration default. The default const
    /// value can be found in `decl_def`.
    DefaultedConst,
}

/// Partial data for a trait impl, used for fake trait impls that we generate ourselves such as
/// `FnOnce` and `Drop` impls.

#[derive(Clone, Debug)]
pub struct VirtualTraitImpl {
    /// The trait that is implemented by this impl block.
    pub trait_pred: TraitPredicate,
    /// The `ImplExpr`s required to satisfy the predicates on the trait declaration.
    pub implied_impl_exprs: Vec<ImplExpr>,
    /// The associated types and their predicates, in definition order.
    pub types: Vec<(Ty, Vec<ImplExpr>)>,
}

impl FullDef {
    pub fn def_id(&self) -> &DefId {
        &self.this.def_id
    }

    /// Reference to the item itself.
    pub fn this(&self) -> &ItemRef {
        &self.this
    }

    pub fn kind(&self) -> &FullDefKind {
        &self.kind
    }

    /// Returns the generics and predicates for definitions that have those.
    pub fn param_env(&self) -> Option<&ParamEnv> {
        use FullDefKind::*;
        match self.kind() {
            Adt { param_env, .. }
            | Trait { param_env, .. }
            | TraitAlias { param_env, .. }
            | TyAlias { param_env, .. }
            | AssocTy { param_env, .. }
            | Fn { param_env, .. }
            | AssocFn { param_env, .. }
            | Closure { param_env, .. }
            | Const { param_env, .. }
            | AssocConst { param_env, .. }
            | Static { param_env, .. }
            | TraitImpl { param_env, .. }
            | InherentImpl { param_env, .. } => Some(param_env),
            _ => None,
        }
    }

    /// Return the parent of this item if the item inherits the typing context from its parent.
    pub fn typing_parent<'tcx>(&self, s: &impl BaseState<'tcx>) -> Option<ItemRef> {
        use FullDefKind::*;
        match self.kind() {
            AssocTy { .. }
            | AssocFn { .. }
            | AssocConst { .. }
            | Const {
                kind: ConstKind::AnonConst | ConstKind::InlineConst | ConstKind::PromotedConst,
                ..
            } => self.param_env().unwrap().parent.clone(),
            Closure { .. } | Ctor { .. } | Variant { .. } => {
                let parent = self.def_id().parent(s).unwrap();
                // The parent has the same generics as this item.
                Some(self.this().with_def_id(s, &parent))
            }
            _ => None,
        }
    }

    /// Whether the item has any generics at all (including parent generics).
    pub fn has_any_generics(&self) -> bool {
        match self.param_env() {
            Some(p) => p.generics.parent_count != 0 || !p.generics.params.is_empty(),
            None => false,
        }
    }

    /// Whether the item has any generics of its own (ignoring parent generics).
    pub fn has_own_generics(&self) -> bool {
        match self.param_env() {
            Some(p) => !p.generics.params.is_empty(),
            None => false,
        }
    }

    /// Whether the item has any generics or predicates of its own (ignoring parent
    /// generics/predicates).
    pub fn has_own_generics_or_predicates(&self) -> bool {
        match self.param_env() {
            Some(p) => {
                let has_predicates = if let FullDefKind::AssocFn { .. }
                | FullDefKind::AssocConst { .. } = self.kind()
                {
                    // Assoc fns and consts have a special `Self: Trait` predicate inserted, which
                    // we don't want to consider as an "own predicate".
                    p.predicates.predicates.len() > 1
                } else {
                    !p.predicates.predicates.is_empty()
                };
                !p.generics.params.is_empty() || has_predicates
            }
            None => false,
        }
    }

    /// Lists the children of this item that can be named, in the way of normal rust paths. For
    /// types, this includes inherent items.
    pub fn nameable_children<'tcx>(&self, s: &impl BaseState<'tcx>) -> Vec<(Symbol, DefId)> {
        let mut children = match self.kind() {
            FullDefKind::Mod { items } => items
                .iter()
                .filter_map(|(opt_ident, def_id)| {
                    Some((opt_ident.as_ref()?.0.clone(), def_id.clone()))
                })
                .collect(),
            FullDefKind::Adt {
                adt_kind: AdtKind::Enum,
                variants,
                ..
            } => variants
                .iter()
                .map(|variant| (variant.name.clone(), variant.def_id.clone()))
                .collect(),
            FullDefKind::InherentImpl { items, .. } | FullDefKind::Trait { items, .. } => items
                .iter()
                .filter_map(|item| Some((item.name.clone()?, item.def_id.clone())))
                .collect(),
            FullDefKind::TraitImpl { items, .. } => items
                .iter()
                .filter_map(|item| Some((item.name.clone()?, item.def_id().clone())))
                .collect(),
            _ => vec![],
        };
        // Add inherent impl items if any.
        if let Some(rust_def_id) = self.def_id().as_rust_def_id() {
            let tcx = s.base().tcx;
            for impl_def_id in tcx.inherent_impls(rust_def_id) {
                children.extend(
                    tcx.associated_items(impl_def_id)
                        .in_definition_order()
                        .filter_map(|assoc| Some((assoc.opt_name()?, assoc.def_id).sinto(s))),
                );
            }
        }
        children
    }
}

impl ImplAssocItem {
    /// The relevant definition: the provided implementation if any, otherwise the default
    /// declaration from the trait declaration.
    pub fn def_id(&self) -> &DefId {
        match &self.value {
            ImplAssocItemValue::Provided { def_id, .. } => def_id,
            _ => &self.decl_def_id,
        }
    }
}

fn get_self_predicate<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> TraitPredicate {
    use ty::Upcast;
    let tcx = s.base().tcx;
    let typing_env = s.typing_env();
    let pred: ty::TraitPredicate = crate::traits::self_predicate(tcx, s.owner_id())
        .no_bound_vars()
        .unwrap()
        .upcast(tcx);
    let pred = substitute(tcx, typing_env, args, pred);
    pred.sinto(s)
}

/// Generates a `dyn Trait<Args.., Ty = <Self as Trait>::Ty..>` type for this trait.
fn get_trait_decl_dyn_self_ty<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> Option<ty::Ty<'tcx>> {
    let tcx = s.base().tcx;
    let typing_env = s.typing_env();
    let def_id = s.owner_id();

    let self_tref = ty::TraitRef::new_from_args(
        tcx,
        def_id,
        args.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, def_id)),
    );
    rustc_utils::dyn_self_ty(tcx, typing_env, self_tref).map(|ty| {
        let ty = if args.is_some() {
            erase_free_regions(tcx, ty)
        } else {
            ty
        };
        ty
    })
}

/// Do the trait resolution necessary to create a new impl for the given trait_ref. Used when we
/// generate fake trait impls e.g. for `FnOnce` and `Drop`.
fn virtual_impl_for<'tcx, S>(s: &S, trait_ref: ty::TraitRef<'tcx>) -> Box<VirtualTraitImpl>
where
    S: UnderOwnerState<'tcx>,
{
    let tcx = s.base().tcx;
    let trait_pred = TraitPredicate {
        trait_ref: trait_ref.sinto(s),
        is_positive: true,
    };
    // Impl exprs required by the trait.
    let required_impl_exprs = solve_item_implied_traits(s, trait_ref.def_id, trait_ref.args);
    let types = tcx
        .associated_items(trait_ref.def_id)
        .in_definition_order()
        .filter(|assoc| matches!(assoc.kind, ty::AssocKind::Type { .. }))
        .map(|assoc| {
            // This assumes non-GAT because this is for builtin-trait (that don't
            // have GATs).
            let ty = ty::Ty::new_projection(tcx, assoc.def_id, trait_ref.args).sinto(s);
            // Impl exprs required by the type.
            let required_impl_exprs = solve_item_implied_traits(s, assoc.def_id, trait_ref.args);
            (ty, required_impl_exprs)
        })
        .collect();
    Box::new(VirtualTraitImpl {
        trait_pred,
        implied_impl_exprs: required_impl_exprs,
        types,
    })
}

fn get_param_env<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> ParamEnv {
    let tcx = s.base().tcx;
    let def_id = s.owner_id();
    // Rustc adds generic params to closures and inline consts for impl details purposes; we hide these.
    let is_typeck_child = tcx.is_typeck_child(def_id);
    let mut generics = tcx.generics_of(def_id).sinto(s);
    if is_typeck_child {
        generics.params = Default::default();
    }

    let parent = generics.parent.as_ref().map(|parent| {
        let parent = parent.real_rust_def_id();
        let args = args.unwrap_or_else(|| ty::GenericArgs::identity_for_item(tcx, def_id));
        let parent_args = args.truncate_to(tcx, tcx.generics_of(parent));
        translate_item_ref(s, parent, parent_args)
    });
    match args {
        None => ParamEnv {
            generics,
            predicates: if is_typeck_child {
                GenericPredicates::default()
            } else {
                required_predicates(tcx, def_id, s.base().options.bounds_options).sinto(s)
            },
            parent,
        },
        // An instantiated item is monomorphic.
        Some(_) => ParamEnv {
            generics: TyGenerics {
                parent_count: 0,
                params: Default::default(),
                ..generics
            },
            predicates: GenericPredicates::default(),
            parent,
        },
    }
}

fn get_implied_predicates<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    args: Option<ty::GenericArgsRef<'tcx>>,
) -> GenericPredicates {
    let tcx = s.base().tcx;
    let def_id = s.owner_id();
    let typing_env = s.typing_env();
    let mut implied_predicates = implied_predicates(tcx, def_id, s.base().options.bounds_options);
    if args.is_some() {
        for pred in implied_predicates.iter_mut() {
            pred.clause = substitute(tcx, typing_env, args, pred.clause);
        }
    }
    implied_predicates.sinto(s)
}

fn const_value<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    def_id: RDefId,
    args: ty::GenericArgsRef<'tcx>,
) -> Option<ConstantExpr> {
    let uneval = ty::UnevaluatedConst::new(def_id, args);
    let c = eval_ty_constant(s, uneval)?;
    match c.kind() {
        ty::ConstKind::Error(..) => None,
        _ => Some(c.sinto(s)),
    }
}
