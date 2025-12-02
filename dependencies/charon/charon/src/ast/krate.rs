use crate::ast::*;
use crate::formatter::{FmtCtx, IntoFormatter};
use crate::ids::Vector;
use crate::pretty::FmtWithCtx;
use crate::reorder_decls::DeclarationsGroups;
use derive_generic_visitor::{ControlFlow, Drive, DriveMut};
use index_vec::Idx;
use macros::{EnumAsGetters, EnumIsA, VariantIndexArity, VariantName};
use serde::{Deserialize, Serialize};
use serde_map_to_array::HashMapToArray;
use std::cmp::{Ord, PartialOrd};
use std::collections::HashMap;
use std::fmt;

generate_index_type!(FunDeclId, "Fun");
generate_index_type!(TypeDeclId, "Adt");
generate_index_type!(GlobalDeclId, "Global");
generate_index_type!(TraitDeclId, "TraitDecl");
generate_index_type!(TraitImplId, "TraitImpl");

/// The id of a translated item.
#[derive(
    Copy,
    Clone,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
    Hash,
    EnumIsA,
    EnumAsGetters,
    VariantName,
    VariantIndexArity,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::rename("AnyDeclId")]
#[charon::variants_prefix("Id")]
pub enum AnyTransId {
    Type(TypeDeclId),
    Fun(FunDeclId),
    Global(GlobalDeclId),
    TraitDecl(TraitDeclId),
    TraitImpl(TraitImplId),
}

/// Implement `TryFrom`  and `From` to convert between an enum and its variants.
macro_rules! wrap_unwrap_enum {
    ($enum:ident::$variant:ident($variant_ty:ident)) => {
        impl TryFrom<$enum> for $variant_ty {
            type Error = ();
            fn try_from(x: $enum) -> Result<Self, Self::Error> {
                match x {
                    $enum::$variant(x) => Ok(x),
                    _ => Err(()),
                }
            }
        }

        impl From<$variant_ty> for $enum {
            fn from(x: $variant_ty) -> Self {
                $enum::$variant(x)
            }
        }
    };
}

wrap_unwrap_enum!(AnyTransId::Fun(FunDeclId));
wrap_unwrap_enum!(AnyTransId::Global(GlobalDeclId));
wrap_unwrap_enum!(AnyTransId::Type(TypeDeclId));
wrap_unwrap_enum!(AnyTransId::TraitDecl(TraitDeclId));
wrap_unwrap_enum!(AnyTransId::TraitImpl(TraitImplId));
impl TryFrom<AnyTransId> for TypeId {
    type Error = ();
    fn try_from(x: AnyTransId) -> Result<Self, Self::Error> {
        Ok(TypeId::Adt(x.try_into()?))
    }
}
impl TryFrom<AnyTransId> for FunId {
    type Error = ();
    fn try_from(x: AnyTransId) -> Result<Self, Self::Error> {
        Ok(FunId::Regular(x.try_into()?))
    }
}

/// A reference to a translated item.
#[derive(
    Debug, Clone, Copy, EnumIsA, EnumAsGetters, VariantName, VariantIndexArity, Drive, DriveMut,
)]
pub enum AnyTransItem<'ctx> {
    Type(&'ctx TypeDecl),
    Fun(&'ctx FunDecl),
    Global(&'ctx GlobalDecl),
    TraitDecl(&'ctx TraitDecl),
    TraitImpl(&'ctx TraitImpl),
}

/// A mutable reference to a translated item.
#[derive(Debug, EnumIsA, EnumAsGetters, VariantName, VariantIndexArity, Drive, DriveMut)]
pub enum AnyTransItemMut<'ctx> {
    Type(&'ctx mut TypeDecl),
    Fun(&'ctx mut FunDecl),
    Global(&'ctx mut GlobalDecl),
    TraitDecl(&'ctx mut TraitDecl),
    TraitImpl(&'ctx mut TraitImpl),
}

#[derive(Default, Clone, Drive, DriveMut, Serialize, Deserialize)]
pub struct TargetInfo {
    /// The pointer size of the target in bytes.
    pub target_pointer_size: types::ByteCount,
    /// Whether the target platform uses little endian byte order.
    pub is_little_endian: bool,
}

/// The data of a translated crate.
#[derive(Default, Clone, Drive, DriveMut, Serialize, Deserialize)]
pub struct TranslatedCrate {
    /// The name of the crate.
    #[drive(skip)]
    pub crate_name: String,

    /// The options used when calling Charon. It is useful for the applications
    /// which consumed the serialized code, to check that Charon was called with
    /// the proper options.
    #[drive(skip)]
    pub options: crate::options::CliOpts,

    /// Information about the target platform for which rustc is called on for the crate.
    #[drive(skip)]
    pub target_information: TargetInfo,

    /// The names of all registered items. Available so we can know the names even of items that
    /// failed to translate.
    /// Invariant: after translation, any existing `AnyTransId` must have an associated name, even
    /// if the corresponding item wasn't translated.
    #[serde(with = "HashMapToArray::<AnyTransId, Name>")]
    pub item_names: HashMap<AnyTransId, Name>,
    /// Short names, for items whose last PathElem is unique.
    #[serde(with = "HashMapToArray::<AnyTransId, Name>")]
    pub short_names: HashMap<AnyTransId, Name>,

    /// The translated files.
    #[drive(skip)]
    pub files: Vector<FileId, File>,
    /// The translated type definitions
    pub type_decls: Vector<TypeDeclId, TypeDecl>,
    /// The translated function definitions
    pub fun_decls: Vector<FunDeclId, FunDecl>,
    /// The translated global definitions
    pub global_decls: Vector<GlobalDeclId, GlobalDecl>,
    /// The translated trait declarations
    pub trait_decls: Vector<TraitDeclId, TraitDecl>,
    /// The translated trait declarations
    pub trait_impls: Vector<TraitImplId, TraitImpl>,
    /// The re-ordered groups of declarations, initialized as empty.
    #[drive(skip)]
    pub ordered_decls: Option<DeclarationsGroups>,
}

impl TranslatedCrate {
    pub fn item_name(&self, id: impl Into<AnyTransId>) -> Option<&Name> {
        self.item_names.get(&id.into())
    }

    pub fn item_short_name(&self, id: impl Into<AnyTransId>) -> Option<&Name> {
        let id = id.into();
        self.short_names.get(&id).or_else(|| self.item_name(id))
    }

    pub fn get_item(&self, trans_id: impl Into<AnyTransId>) -> Option<AnyTransItem<'_>> {
        match trans_id.into() {
            AnyTransId::Type(id) => self.type_decls.get(id).map(AnyTransItem::Type),
            AnyTransId::Fun(id) => self.fun_decls.get(id).map(AnyTransItem::Fun),
            AnyTransId::Global(id) => self.global_decls.get(id).map(AnyTransItem::Global),
            AnyTransId::TraitDecl(id) => self.trait_decls.get(id).map(AnyTransItem::TraitDecl),
            AnyTransId::TraitImpl(id) => self.trait_impls.get(id).map(AnyTransItem::TraitImpl),
        }
    }

    pub fn get_item_mut(&mut self, trans_id: AnyTransId) -> Option<AnyTransItemMut<'_>> {
        match trans_id {
            AnyTransId::Type(id) => self.type_decls.get_mut(id).map(AnyTransItemMut::Type),
            AnyTransId::Fun(id) => self.fun_decls.get_mut(id).map(AnyTransItemMut::Fun),
            AnyTransId::Global(id) => self.global_decls.get_mut(id).map(AnyTransItemMut::Global),
            AnyTransId::TraitDecl(id) => {
                self.trait_decls.get_mut(id).map(AnyTransItemMut::TraitDecl)
            }
            AnyTransId::TraitImpl(id) => {
                self.trait_impls.get_mut(id).map(AnyTransItemMut::TraitImpl)
            }
        }
    }

    pub fn all_ids(&self) -> impl Iterator<Item = AnyTransId> + use<> {
        self.type_decls
            .all_indices()
            .map(AnyTransId::Type)
            .chain(self.trait_decls.all_indices().map(AnyTransId::TraitDecl))
            .chain(self.trait_impls.all_indices().map(AnyTransId::TraitImpl))
            .chain(self.global_decls.all_indices().map(AnyTransId::Global))
            .chain(self.fun_decls.all_indices().map(AnyTransId::Fun))
    }
    pub fn all_items(&self) -> impl Iterator<Item = AnyTransItem<'_>> {
        self.type_decls
            .iter()
            .map(AnyTransItem::Type)
            .chain(self.trait_decls.iter().map(AnyTransItem::TraitDecl))
            .chain(self.trait_impls.iter().map(AnyTransItem::TraitImpl))
            .chain(self.global_decls.iter().map(AnyTransItem::Global))
            .chain(self.fun_decls.iter().map(AnyTransItem::Fun))
    }
    pub fn all_items_mut(&mut self) -> impl Iterator<Item = AnyTransItemMut<'_>> {
        self.type_decls
            .iter_mut()
            .map(AnyTransItemMut::Type)
            .chain(self.trait_impls.iter_mut().map(AnyTransItemMut::TraitImpl))
            .chain(self.trait_decls.iter_mut().map(AnyTransItemMut::TraitDecl))
            .chain(self.fun_decls.iter_mut().map(AnyTransItemMut::Fun))
            .chain(self.global_decls.iter_mut().map(AnyTransItemMut::Global))
    }
    pub fn all_items_with_ids(&self) -> impl Iterator<Item = (AnyTransId, AnyTransItem<'_>)> {
        self.all_items().map(|item| (item.id(), item))
    }
}

impl<'ctx> AnyTransItem<'ctx> {
    pub fn id(&self) -> AnyTransId {
        match self {
            AnyTransItem::Type(d) => d.def_id.into(),
            AnyTransItem::Fun(d) => d.def_id.into(),
            AnyTransItem::Global(d) => d.def_id.into(),
            AnyTransItem::TraitDecl(d) => d.def_id.into(),
            AnyTransItem::TraitImpl(d) => d.def_id.into(),
        }
    }

    pub fn item_meta(&self) -> &'ctx ItemMeta {
        match self {
            AnyTransItem::Type(d) => &d.item_meta,
            AnyTransItem::Fun(d) => &d.item_meta,
            AnyTransItem::Global(d) => &d.item_meta,
            AnyTransItem::TraitDecl(d) => &d.item_meta,
            AnyTransItem::TraitImpl(d) => &d.item_meta,
        }
    }

    /// The generic parameters of this item.
    pub fn generic_params(&self) -> &'ctx GenericParams {
        match self {
            AnyTransItem::Type(d) => &d.generics,
            AnyTransItem::Fun(d) => &d.signature.generics,
            AnyTransItem::Global(d) => &d.generics,
            AnyTransItem::TraitDecl(d) => &d.generics,
            AnyTransItem::TraitImpl(d) => &d.generics,
        }
    }

    /// Get information about the parent of this item, if any.
    pub fn parent_info(&self) -> &'ctx ItemKind {
        match self {
            AnyTransItem::Fun(d) => &d.kind,
            AnyTransItem::Global(d) => &d.kind,
            AnyTransItem::Type(_) | AnyTransItem::TraitDecl(_) | AnyTransItem::TraitImpl(_) => {
                &ItemKind::TopLevel
            }
        }
    }

    /// See [`GenericParams::identity_args`].
    pub fn identity_args(&self) -> GenericArgs {
        self.generic_params().identity_args()
    }

    /// We can't implement `AstVisitable` because of the `'static` constraint, but it's ok because
    /// `AnyTransItem` isn't contained in any of our types.
    pub fn drive<V: VisitAst>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match *self {
            AnyTransItem::Type(d) => visitor.visit(d),
            AnyTransItem::Fun(d) => visitor.visit(d),
            AnyTransItem::Global(d) => visitor.visit(d),
            AnyTransItem::TraitDecl(d) => visitor.visit(d),
            AnyTransItem::TraitImpl(d) => visitor.visit(d),
        }
    }

    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    pub fn dyn_visit<T: AstVisitable>(&self, f: impl FnMut(&T)) {
        match *self {
            AnyTransItem::Type(d) => d.dyn_visit(f),
            AnyTransItem::Fun(d) => d.dyn_visit(f),
            AnyTransItem::Global(d) => d.dyn_visit(f),
            AnyTransItem::TraitDecl(d) => d.dyn_visit(f),
            AnyTransItem::TraitImpl(d) => d.dyn_visit(f),
        }
    }
}

impl<'ctx> AnyTransItemMut<'ctx> {
    pub fn as_ref(&self) -> AnyTransItem<'_> {
        match self {
            AnyTransItemMut::Type(d) => AnyTransItem::Type(d),
            AnyTransItemMut::Fun(d) => AnyTransItem::Fun(d),
            AnyTransItemMut::Global(d) => AnyTransItem::Global(d),
            AnyTransItemMut::TraitDecl(d) => AnyTransItem::TraitDecl(d),
            AnyTransItemMut::TraitImpl(d) => AnyTransItem::TraitImpl(d),
        }
    }
    pub fn reborrow(&mut self) -> AnyTransItemMut<'_> {
        match self {
            AnyTransItemMut::Type(d) => AnyTransItemMut::Type(d),
            AnyTransItemMut::Fun(d) => AnyTransItemMut::Fun(d),
            AnyTransItemMut::Global(d) => AnyTransItemMut::Global(d),
            AnyTransItemMut::TraitDecl(d) => AnyTransItemMut::TraitDecl(d),
            AnyTransItemMut::TraitImpl(d) => AnyTransItemMut::TraitImpl(d),
        }
    }

    /// The generic parameters of this item.
    pub fn generic_params(&mut self) -> &mut GenericParams {
        match self {
            AnyTransItemMut::Type(d) => &mut d.generics,
            AnyTransItemMut::Fun(d) => &mut d.signature.generics,
            AnyTransItemMut::Global(d) => &mut d.generics,
            AnyTransItemMut::TraitDecl(d) => &mut d.generics,
            AnyTransItemMut::TraitImpl(d) => &mut d.generics,
        }
    }

    /// We can't implement `AstVisitable` because of the `'static` constraint, but it's ok because
    /// `AnyTransItemMut` isn't contained in any of our types.
    pub fn drive_mut<V: VisitAstMut>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            AnyTransItemMut::Type(d) => visitor.visit(*d),
            AnyTransItemMut::Fun(d) => visitor.visit(*d),
            AnyTransItemMut::Global(d) => visitor.visit(*d),
            AnyTransItemMut::TraitDecl(d) => visitor.visit(*d),
            AnyTransItemMut::TraitImpl(d) => visitor.visit(*d),
        }
    }

    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    pub fn dyn_visit_mut<T: AstVisitable>(&mut self, f: impl FnMut(&mut T)) {
        match self {
            AnyTransItemMut::Type(d) => d.dyn_visit_mut(f),
            AnyTransItemMut::Fun(d) => d.dyn_visit_mut(f),
            AnyTransItemMut::Global(d) => d.dyn_visit_mut(f),
            AnyTransItemMut::TraitDecl(d) => d.dyn_visit_mut(f),
            AnyTransItemMut::TraitImpl(d) => d.dyn_visit_mut(f),
        }
    }
}

impl fmt::Display for TranslatedCrate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fmt: &FmtCtx = &self.into_fmt();
        match &self.ordered_decls {
            None => {
                // We do simple: types, globals, traits, functions
                for d in &self.type_decls {
                    writeln!(f, "{}\n", d.with_ctx(fmt))?
                }
                for d in &self.global_decls {
                    writeln!(f, "{}\n", d.with_ctx(fmt))?
                }
                for d in &self.trait_decls {
                    writeln!(f, "{}\n", d.with_ctx(fmt))?
                }
                for d in &self.trait_impls {
                    writeln!(f, "{}\n", d.with_ctx(fmt))?
                }
                for d in &self.fun_decls {
                    writeln!(f, "{}\n", d.with_ctx(fmt))?
                }
            }
            Some(ordered_decls) => {
                for gr in ordered_decls {
                    for id in gr.get_ids() {
                        writeln!(f, "{}\n", fmt.format_decl_id(id))?
                    }
                }
            }
        }
        fmt::Result::Ok(())
    }
}

impl<'tcx, 'ctx, 'a> IntoFormatter for &'a TranslatedCrate {
    type C = FmtCtx<'a>;

    fn into_fmt(self) -> Self::C {
        FmtCtx {
            translated: Some(self),
            ..Default::default()
        }
    }
}

pub trait HasVectorOf<Id: Idx>: std::ops::Index<Id, Output: Sized> {
    fn get_vector(&self) -> &Vector<Id, Self::Output>;
    fn get_vector_mut(&mut self) -> &mut Vector<Id, Self::Output>;
}

/// Delegate `Index` implementations to subfields.
macro_rules! mk_index_impls {
    ($ty:ident.$field:ident[$idx:ty]: $output:ty) => {
        impl std::ops::Index<$idx> for $ty {
            type Output = $output;
            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index]
            }
        }
        impl std::ops::IndexMut<$idx> for $ty {
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }
        impl HasVectorOf<$idx> for $ty {
            fn get_vector(&self) -> &Vector<$idx, Self::Output> {
                &self.$field
            }
            fn get_vector_mut(&mut self) -> &mut Vector<$idx, Self::Output> {
                &mut self.$field
            }
        }
    };
}
pub(crate) use mk_index_impls;

mk_index_impls!(TranslatedCrate.type_decls[TypeDeclId]: TypeDecl);
mk_index_impls!(TranslatedCrate.fun_decls[FunDeclId]: FunDecl);
mk_index_impls!(TranslatedCrate.global_decls[GlobalDeclId]: GlobalDecl);
mk_index_impls!(TranslatedCrate.trait_decls[TraitDeclId]: TraitDecl);
mk_index_impls!(TranslatedCrate.trait_impls[TraitImplId]: TraitImpl);
