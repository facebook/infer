//! This module contains the type definition for `DefId` and the types
//! `DefId` depends on.
//!
//! This is purposely a very small isolated module:
//! `hax-engine-names-extract` uses those types, but we don't want
//! `hax-engine-names-extract` to have a build dependency on the whole
//! frontend, that double the build times for the Rust part of hax.

use crate::AdtInto;
use crate::prelude::*;

use {rustc_hir as hir, rustc_hir::def_id::DefId as RDefId, rustc_middle::ty};

sinto_reexport!(hir::Safety);
sinto_reexport!(hir::Mutability);
sinto_reexport!(hir::def::CtorKind);
sinto_reexport!(hir::def::MacroKinds);
sinto_reexport!(hir::def::CtorOf);
sinto_reexport!(rustc_span::symbol::Symbol);
sinto_reexport!(rustc_span::symbol::ByteSymbol);

/// Reflects [`rustc_hir::def::DefKind`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_hir::def::DefKind, state: S as tcx)]
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum DefKind {
    Mod,
    Struct,
    Union,
    Enum,
    Variant,
    Trait,
    TyAlias,
    ForeignTy,
    TraitAlias,
    AssocTy,
    TyParam,
    Fn,
    Const,
    ConstParam,
    Static {
        safety: Safety,
        mutability: Mutability,
        nested: bool,
    },
    Ctor(CtorOf, CtorKind),
    AssocFn,
    AssocConst,
    Macro(MacroKinds),
    ExternCrate,
    Use,
    ForeignMod,
    AnonConst,
    InlineConst,
    #[disable_mapping]
    /// Added by hax: promoted constants don't have def_ids in rustc but they do in hax.
    PromotedConst,
    OpaqueTy,
    Field,
    LifetimeParam,
    GlobalAsm,
    Impl {
        of_trait: bool,
    },
    Closure,
    SyntheticCoroutineBody,
}

pub use rustc_middle::mir::Promoted as PromotedId;

/// The crate name under which synthetic items are exported under.
const SYNTHETIC_CRATE_NAME: &str = "<synthetic>";

/// Reflects [`rustc_hir::def_id::DefId`], augmented to also give ids to promoted constants (which
/// have their own ad-hoc numbering scheme in rustc for now).
#[derive(Clone, PartialEq, Eq)]
pub struct DefId {
    pub(crate) contents: crate::id_table::hash_consing::HashConsed<DefIdContents>,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct DefIdContents {
    pub base: DefIdBase,
    /// The kind of definition this `DefId` points to.
    pub kind: crate::DefKind,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum DefIdBase {
    Real(RDefId),
    Promoted(RDefId, PromotedId),
    Synthetic(SyntheticItem, RDefId),
}

impl DefIdBase {
    pub fn underlying_def_id(self) -> Option<RDefId> {
        match self {
            Self::Real(did) | Self::Promoted(did, ..) => Some(did),
            Self::Synthetic(.., did) => Some(did),
        }
    }
    pub fn as_real(self) -> Option<RDefId> {
        match self {
            Self::Real(did) => Some(did),
            _ => None,
        }
    }
    pub fn as_promoted(self) -> Option<(RDefId, PromotedId)> {
        match self {
            Self::Promoted(did, promoted) => Some((did, promoted)),
            _ => None,
        }
    }
    pub fn as_real_or_promoted(self) -> Option<RDefId> {
        match self {
            Self::Real(did) | Self::Promoted(did, ..) => Some(did),
            _ => None,
        }
    }
    pub fn as_synthetic(self) -> Option<SyntheticItem> {
        if let Self::Synthetic(v, _) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl DefIdContents {
    pub fn make_def_id<'tcx, S: BaseState<'tcx>>(self, _s: &S) -> DefId {
        let contents = id_table::hash_consing::HashConsed::new(self);
        DefId { contents }
    }
}

impl DefId {
    /// The rustc def_id corresponding to this item, if there is one. Promoted constants don't have
    /// a rustc def_id.
    pub fn as_rust_def_id(&self) -> Option<RDefId> {
        self.base.as_real()
    }
    /// The rustc def_id of this item. Panics if this is not a real rustc item.
    pub fn real_rust_def_id(&self) -> RDefId {
        self.as_rust_def_id().unwrap()
    }
    /// The def_id of this item or its parent if this is a promoted constant.
    pub fn underlying_rust_def_id(&self) -> Option<RDefId> {
        self.base.as_real_or_promoted()
    }
    /// The def_id of this item, or its parent if this is a promoted constant, or a made-up `DefId`
    /// for synthetic items. The method is explicitly named to phase out `DefId`s for synthetic
    /// items.
    pub fn as_def_id_even_synthetic(&self) -> RDefId {
        self.base.underlying_def_id().unwrap()
    }

    pub fn is_local(&self) -> bool {
        self.base
            .underlying_def_id()
            .is_some_and(|did| did.is_local())
    }
    pub fn promoted_id(&self) -> Option<PromotedId> {
        self.base.as_promoted().map(|(_, p)| p)
    }

    fn make<'tcx, S: BaseState<'tcx>>(s: &S, def_id: RDefId) -> Self {
        let base = match s.with_global_cache(|c| c.reverse_synthetic_map.get(&def_id).copied()) {
            None => DefIdBase::Real(def_id),
            Some(synthetic) => return Self::make_synthetic(s, synthetic, def_id),
        };
        let tcx = s.base().tcx;
        let contents = DefIdContents {
            base,
            kind: get_def_kind(tcx, def_id).sinto(s),
        };
        contents.make_def_id(s)
    }

    pub fn make_synthetic<'tcx, S: BaseState<'tcx>>(
        s: &S,
        synthetic: SyntheticItem,
        def_id: RDefId,
    ) -> Self {
        let contents = DefIdContents {
            base: DefIdBase::Synthetic(synthetic, def_id),
            kind: DefKind::Struct,
        };
        contents.make_def_id(s)
    }

    /// Returns the [`SyntheticItem`] encoded by a [rustc `DefId`](RDefId), if
    /// any.
    ///
    /// Note that this method relies on rustc indexes, which are session
    /// specific. See [`Self`] documentation.
    pub fn as_synthetic<'tcx>(&self, _s: &impl BaseState<'tcx>) -> Option<SyntheticItem> {
        self.base.as_synthetic()
    }

    pub fn parent<'tcx>(&self, s: &impl BaseState<'tcx>) -> Option<DefId> {
        match self.base {
            DefIdBase::Real(def_id) => s.tcx().opt_parent(def_id),
            DefIdBase::Promoted(def_id, _) => Some(def_id),
            DefIdBase::Synthetic(..) => Some(rustc_span::def_id::CRATE_DEF_ID.to_def_id()),
        }
        .sinto(s)
    }

    pub fn crate_name<'tcx>(&self, s: &impl BaseState<'tcx>) -> Symbol {
        let tcx = s.base().tcx;
        match self.base {
            DefIdBase::Real(def_id) | DefIdBase::Promoted(def_id, ..) => {
                tcx.crate_name(def_id.krate)
            }
            DefIdBase::Synthetic(..) => Symbol::intern(SYNTHETIC_CRATE_NAME),
        }
    }

    /// The `PathItem` corresponding to this item.
    pub fn path_item<'tcx>(&self, s: &impl BaseState<'tcx>) -> DisambiguatedDefPathItem {
        match self.base {
            DefIdBase::Real(def_id) => {
                let tcx = s.base().tcx;
                // Set the def_id so the `CrateRoot` path item can fetch the crate name.
                let state_with_id = s.with_hax_owner(self);
                tcx.def_path(def_id)
                    .data
                    .last()
                    .map(|x| x.sinto(&state_with_id))
                    .unwrap_or_else(|| DisambiguatedDefPathItem {
                        disambiguator: 0,
                        data: DefPathItem::CrateRoot {
                            name: self.crate_name(s),
                        },
                    })
            }
            DefIdBase::Promoted(_, id) => DisambiguatedDefPathItem {
                data: DefPathItem::PromotedConst,
                // Reuse the promoted id as disambiguator, like for inline consts.
                disambiguator: id.as_u32(),
            },
            DefIdBase::Synthetic(synthetic, ..) => DisambiguatedDefPathItem {
                disambiguator: 0,
                data: DefPathItem::TypeNs(Symbol::intern(&synthetic.name())),
            },
        }
    }

    /// Construct a hax `DefId` for the nth promoted constant of the current item. That `DefId` has
    /// no corresponding rustc `DefId`.
    pub fn make_promoted_child<'tcx, S: BaseState<'tcx>>(
        &self,
        s: &S,
        promoted_id: PromotedId,
    ) -> Self {
        let contents = DefIdContents {
            base: DefIdBase::Promoted(self.real_rust_def_id(), promoted_id),
            kind: DefKind::PromotedConst,
        };
        contents.make_def_id(s)
    }
}

impl DefId {
    /// Gets the visibility (`pub` or not) of the definition. Returns `None` for defs that don't have a
    /// meaningful visibility.
    pub fn visibility<'tcx>(&self, tcx: ty::TyCtxt<'tcx>) -> Option<bool> {
        use DefKind::*;
        match self.kind {
            AssocConst
            | AssocFn
            | Const
            | Enum
            | Field
            | Fn
            | ForeignTy
            | Macro { .. }
            | Mod
            | Static { .. }
            | Struct
            | Trait
            | TraitAlias
            | TyAlias { .. }
            | Union
            | Use
            | Variant => {
                let def_id = self.as_rust_def_id()?;
                Some(tcx.visibility(def_id).is_public())
            }
            // These kinds don't have visibility modifiers (which would cause `visibility` to panic).
            AnonConst
            | AssocTy
            | Closure
            | ConstParam
            | Ctor { .. }
            | ExternCrate
            | ForeignMod
            | GlobalAsm
            | Impl { .. }
            | InlineConst
            | PromotedConst
            | LifetimeParam
            | OpaqueTy
            | SyntheticCoroutineBody
            | TyParam => None,
        }
    }

    /// Gets the attributes of the definition.
    pub fn attrs<'tcx>(&self, tcx: ty::TyCtxt<'tcx>) -> &'tcx [rustc_hir::Attribute] {
        use DefKind::*;
        match self.kind {
            // These kinds cause `get_attrs` to panic.
            ConstParam | LifetimeParam | TyParam | ForeignMod | InlineConst => &[],
            _ => {
                if let Some(def_id) = self.as_rust_def_id() {
                    if let Some(ldid) = def_id.as_local() {
                        tcx.hir_attrs(tcx.local_def_id_to_hir_id(ldid))
                    } else {
                        tcx.attrs_for_def(def_id)
                    }
                } else {
                    &[]
                }
            }
        }
    }
}

impl std::ops::Deref for DefId {
    type Target = DefIdContents;
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl std::fmt::Debug for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.base {
            DefIdBase::Real(def_id) => write!(f, "{def_id:?}"),
            DefIdBase::Promoted(def_id, promoted) => {
                write!(f, "{def_id:?}::promoted#{}", promoted.as_u32())
            }
            DefIdBase::Synthetic(item, _) => write!(f, "{}", item.name()),
        }
    }
}

impl std::hash::Hash for DefId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.base.hash(state);
    }
}

/// Gets the kind of the definition. Can't use `def_kind` directly because this crashes on the
/// crate root.
pub(crate) fn get_def_kind<'tcx>(tcx: ty::TyCtxt<'tcx>, def_id: RDefId) -> hir::def::DefKind {
    if def_id == rustc_span::def_id::CRATE_DEF_ID.to_def_id() {
        // Horrible hack: without this, `def_kind` crashes on the crate root. Presumably some table
        // isn't properly initialized otherwise.
        let _ = tcx.def_span(def_id);
    };
    tcx.def_kind(def_id)
}

impl<'s, S: BaseState<'s>> SInto<S, DefId> for RDefId {
    fn sinto(&self, s: &S) -> DefId {
        if let Some(def_id) = s.with_item_cache(*self, |cache| cache.def_id.clone()) {
            return def_id;
        }
        let def_id = DefId::make(s, *self);
        s.with_item_cache(*self, |cache| cache.def_id = Some(def_id.clone()));
        def_id
    }
}

/// Reflects [`rustc_hir::definitions::DefPathData`]

#[derive(Clone, Debug, Hash, PartialEq, Eq, AdtInto)]
#[args(<'ctx, S: UnderOwnerState<'ctx>>, from: rustc_hir::definitions::DefPathData, state: S as s)]
pub enum DefPathItem {
    CrateRoot {
        #[value(s.base().tcx.crate_name(s.owner_id().krate).sinto(s))]
        name: Symbol,
    },
    Impl,
    ForeignMod,
    Use,
    GlobalAsm,
    TypeNs(Symbol),
    ValueNs(Symbol),
    MacroNs(Symbol),
    LifetimeNs(Symbol),
    Closure,
    Ctor,
    LateAnonConst,
    AnonConst,
    #[disable_mapping]
    PromotedConst,
    DesugaredAnonymousLifetime,
    OpaqueTy,
    OpaqueLifetime(Symbol),
    AnonAssocTy(Symbol),
    SyntheticCoroutineBody,
    NestedStatic,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, AdtInto)]
#[args(<'a, S: UnderOwnerState<'a>>, from: rustc_hir::definitions::DisambiguatedDefPathData, state: S as s)]
/// Reflects [`rustc_hir::definitions::DisambiguatedDefPathData`]
pub struct DisambiguatedDefPathItem {
    pub data: DefPathItem,
    pub disambiguator: u32,
}
