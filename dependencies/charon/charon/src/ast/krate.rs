use std::cmp::{Ord, PartialOrd};
use std::fmt;

use derive_generic_visitor::{ControlFlow, Drive, DriveMut};
use index_vec::Idx;
use serde::{Deserialize, Serialize};
use serde_state::{DeserializeState, SerializeState};

use crate::ast::*;
use crate::common::serialize_map_to_array::SeqHashMapToArray;
use crate::formatter::{FmtCtx, IntoFormatter};
use crate::ids::{IndexMap, IndexVec};
use crate::pretty::FmtWithCtx;
use macros::{EnumAsGetters, EnumIsA, VariantIndexArity, VariantName};

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
    SerializeState,
    DeserializeState,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("Id")]
#[serde_state(stateless)]
pub enum ItemId {
    Type(TypeDeclId),
    TraitDecl(TraitDeclId),
    TraitImpl(TraitImplId),
    Fun(FunDeclId),
    Global(GlobalDeclId),
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

wrap_unwrap_enum!(ItemId::Fun(FunDeclId));
wrap_unwrap_enum!(ItemId::Global(GlobalDeclId));
wrap_unwrap_enum!(ItemId::Type(TypeDeclId));
wrap_unwrap_enum!(ItemId::TraitDecl(TraitDeclId));
wrap_unwrap_enum!(ItemId::TraitImpl(TraitImplId));
impl TryFrom<ItemId> for TypeId {
    type Error = ();
    fn try_from(x: ItemId) -> Result<Self, Self::Error> {
        Ok(TypeId::Adt(x.try_into()?))
    }
}
impl TryFrom<ItemId> for FunId {
    type Error = ();
    fn try_from(x: ItemId) -> Result<Self, Self::Error> {
        Ok(FunId::Regular(x.try_into()?))
    }
}

/// A translated item.
#[derive(Debug, EnumIsA, EnumAsGetters, VariantName, VariantIndexArity, Drive, DriveMut)]
pub enum ItemByVal {
    Type(TypeDecl),
    Fun(FunDecl),
    Global(GlobalDecl),
    TraitDecl(TraitDecl),
    TraitImpl(TraitImpl),
}

/// A reference to a translated item.
#[derive(
    Debug, Clone, Copy, EnumIsA, EnumAsGetters, VariantName, VariantIndexArity, Drive, DriveMut,
)]
pub enum ItemRef<'ctx> {
    Type(&'ctx TypeDecl),
    Fun(&'ctx FunDecl),
    Global(&'ctx GlobalDecl),
    TraitDecl(&'ctx TraitDecl),
    TraitImpl(&'ctx TraitImpl),
}

/// A mutable reference to a translated item.
#[derive(Debug, EnumIsA, EnumAsGetters, VariantName, VariantIndexArity, Drive, DriveMut)]
pub enum ItemRefMut<'ctx> {
    Type(&'ctx mut TypeDecl),
    Fun(&'ctx mut FunDecl),
    Global(&'ctx mut GlobalDecl),
    TraitDecl(&'ctx mut TraitDecl),
    TraitImpl(&'ctx mut TraitImpl),
}

/// A (group of) top-level declaration(s), properly reordered.
/// "G" stands for "generic"
#[derive(
    Debug, Clone, VariantIndexArity, VariantName, EnumAsGetters, EnumIsA, Serialize, Deserialize,
)]
#[charon::variants_suffix("Group")]
pub enum GDeclarationGroup<Id> {
    /// A non-recursive declaration
    NonRec(Id),
    /// A (group of mutually) recursive declaration(s)
    Rec(Vec<Id>),
}

/// A (group of) top-level declaration(s), properly reordered.
#[derive(
    Debug, Clone, VariantIndexArity, VariantName, EnumAsGetters, EnumIsA, Serialize, Deserialize,
)]
#[charon::variants_suffix("Group")]
pub enum DeclarationGroup {
    /// A type declaration group
    Type(GDeclarationGroup<TypeDeclId>),
    /// A function declaration group
    Fun(GDeclarationGroup<FunDeclId>),
    /// A global declaration group
    Global(GDeclarationGroup<GlobalDeclId>),
    ///
    TraitDecl(GDeclarationGroup<TraitDeclId>),
    ///
    TraitImpl(GDeclarationGroup<TraitImplId>),
    /// Anything that doesn't fit into these categories.
    Mixed(GDeclarationGroup<ItemId>),
}

pub type DeclarationsGroups = Vec<DeclarationGroup>;

#[derive(Default, Clone, Drive, DriveMut, Serialize, Deserialize)]
pub struct TargetInfo {
    /// The pointer size of the target in bytes.
    pub target_pointer_size: types::ByteCount,
    /// Whether the target platform uses little endian byte order.
    pub is_little_endian: bool,
}

/// The data of a translated crate.
#[derive(Default, Clone, Drive, DriveMut, SerializeState, DeserializeState)]
#[serde_state(state_implements = HashConsSerializerState)]
pub struct TranslatedCrate {
    /// The name of the crate.
    #[drive(skip)]
    pub crate_name: String,

    /// The options used when calling Charon. It is useful for the applications
    /// which consumed the serialized code, to check that Charon was called with
    /// the proper options.
    #[drive(skip)]
    #[serde_state(stateless)]
    pub options: crate::options::CliOpts,

    /// Information about the target platform for which rustc is called on for the crate.
    #[drive(skip)]
    #[serde_state(stateless)]
    pub target_information: TargetInfo,

    /// The names of all registered items. Available so we can know the names even of items that
    /// failed to translate.
    /// Invariant: after translation, any existing `ItemId` must have an associated name, even
    /// if the corresponding item wasn't translated.
    #[serde(with = "SeqHashMapToArray::<ItemId, Name>")]
    pub item_names: SeqHashMap<ItemId, Name>,
    /// Short names, for items whose last PathElem is unique.
    #[serde(with = "SeqHashMapToArray::<ItemId, Name>")]
    pub short_names: SeqHashMap<ItemId, Name>,

    /// The translated files.
    #[drive(skip)]
    #[serde_state(stateless)]
    pub files: IndexVec<FileId, File>,
    /// The translated type definitions
    pub type_decls: IndexMap<TypeDeclId, TypeDecl>,
    /// The translated function definitions
    pub fun_decls: IndexMap<FunDeclId, FunDecl>,
    /// The translated global definitions
    pub global_decls: IndexMap<GlobalDeclId, GlobalDecl>,
    /// The translated trait declarations
    pub trait_decls: IndexMap<TraitDeclId, TraitDecl>,
    /// The translated trait declarations
    pub trait_impls: IndexMap<TraitImplId, TraitImpl>,
    /// A `const UNIT: () = ();` used whenever we make a thin pointer/reference to avoid creating a
    /// local `let unit = ();` variable. It is always `Some`.
    pub unit_metadata: Option<GlobalDeclRef>,
    /// The re-ordered groups of declarations, initialized as empty.
    #[drive(skip)]
    #[serde_state(stateless)]
    pub ordered_decls: Option<DeclarationsGroups>,
}

impl TranslatedCrate {
    pub fn item_name(&self, id: impl Into<ItemId>) -> Option<&Name> {
        self.item_names.get(&id.into())
    }

    pub fn item_short_name(&self, id: impl Into<ItemId>) -> Option<&Name> {
        let id = id.into();
        self.short_names.get(&id).or_else(|| self.item_name(id))
    }

    pub fn get_item(&self, trans_id: impl Into<ItemId>) -> Option<ItemRef<'_>> {
        match trans_id.into() {
            ItemId::Type(id) => self.type_decls.get(id).map(ItemRef::Type),
            ItemId::Fun(id) => self.fun_decls.get(id).map(ItemRef::Fun),
            ItemId::Global(id) => self.global_decls.get(id).map(ItemRef::Global),
            ItemId::TraitDecl(id) => self.trait_decls.get(id).map(ItemRef::TraitDecl),
            ItemId::TraitImpl(id) => self.trait_impls.get(id).map(ItemRef::TraitImpl),
        }
    }
    pub fn get_item_mut(&mut self, trans_id: ItemId) -> Option<ItemRefMut<'_>> {
        match trans_id {
            ItemId::Type(id) => self.type_decls.get_mut(id).map(ItemRefMut::Type),
            ItemId::Fun(id) => self.fun_decls.get_mut(id).map(ItemRefMut::Fun),
            ItemId::Global(id) => self.global_decls.get_mut(id).map(ItemRefMut::Global),
            ItemId::TraitDecl(id) => self.trait_decls.get_mut(id).map(ItemRefMut::TraitDecl),
            ItemId::TraitImpl(id) => self.trait_impls.get_mut(id).map(ItemRefMut::TraitImpl),
        }
    }
    pub fn remove_item(&mut self, trans_id: ItemId) -> Option<ItemByVal> {
        match trans_id {
            ItemId::Type(id) => self.type_decls.remove(id).map(ItemByVal::Type),
            ItemId::Fun(id) => self.fun_decls.remove(id).map(ItemByVal::Fun),
            ItemId::Global(id) => self.global_decls.remove(id).map(ItemByVal::Global),
            ItemId::TraitDecl(id) => self.trait_decls.remove(id).map(ItemByVal::TraitDecl),
            ItemId::TraitImpl(id) => self.trait_impls.remove(id).map(ItemByVal::TraitImpl),
        }
    }
    pub fn set_item_slot(&mut self, id: ItemId, item: ItemByVal) {
        match item {
            ItemByVal::Type(decl) => self.type_decls.set_slot(*id.as_type().unwrap(), decl),
            ItemByVal::Fun(decl) => self.fun_decls.set_slot(*id.as_fun().unwrap(), decl),
            ItemByVal::Global(decl) => self.global_decls.set_slot(*id.as_global().unwrap(), decl),
            ItemByVal::TraitDecl(decl) => self
                .trait_decls
                .set_slot(*id.as_trait_decl().unwrap(), decl),
            ItemByVal::TraitImpl(decl) => self
                .trait_impls
                .set_slot(*id.as_trait_impl().unwrap(), decl),
        }
    }

    pub fn all_ids(&self) -> impl Iterator<Item = ItemId> + use<> {
        self.type_decls
            .all_indices()
            .map(ItemId::Type)
            .chain(self.trait_decls.all_indices().map(ItemId::TraitDecl))
            .chain(self.trait_impls.all_indices().map(ItemId::TraitImpl))
            .chain(self.global_decls.all_indices().map(ItemId::Global))
            .chain(self.fun_decls.all_indices().map(ItemId::Fun))
    }
    pub fn all_items(&self) -> impl Iterator<Item = ItemRef<'_>> {
        self.type_decls
            .iter()
            .map(ItemRef::Type)
            .chain(self.trait_decls.iter().map(ItemRef::TraitDecl))
            .chain(self.trait_impls.iter().map(ItemRef::TraitImpl))
            .chain(self.global_decls.iter().map(ItemRef::Global))
            .chain(self.fun_decls.iter().map(ItemRef::Fun))
    }
    pub fn all_items_mut(&mut self) -> impl Iterator<Item = ItemRefMut<'_>> {
        self.type_decls
            .iter_mut()
            .map(ItemRefMut::Type)
            .chain(self.trait_impls.iter_mut().map(ItemRefMut::TraitImpl))
            .chain(self.trait_decls.iter_mut().map(ItemRefMut::TraitDecl))
            .chain(self.fun_decls.iter_mut().map(ItemRefMut::Fun))
            .chain(self.global_decls.iter_mut().map(ItemRefMut::Global))
    }
    pub fn all_items_with_ids(&self) -> impl Iterator<Item = (ItemId, ItemRef<'_>)> {
        self.all_items().map(|item| (item.id(), item))
    }
}

impl ItemByVal {
    pub fn as_ref(&self) -> ItemRef<'_> {
        match self {
            Self::Type(d) => ItemRef::Type(d),
            Self::Fun(d) => ItemRef::Fun(d),
            Self::Global(d) => ItemRef::Global(d),
            Self::TraitDecl(d) => ItemRef::TraitDecl(d),
            Self::TraitImpl(d) => ItemRef::TraitImpl(d),
        }
    }
    pub fn as_mut(&mut self) -> ItemRefMut<'_> {
        match self {
            Self::Type(d) => ItemRefMut::Type(d),
            Self::Fun(d) => ItemRefMut::Fun(d),
            Self::Global(d) => ItemRefMut::Global(d),
            Self::TraitDecl(d) => ItemRefMut::TraitDecl(d),
            Self::TraitImpl(d) => ItemRefMut::TraitImpl(d),
        }
    }
}

impl<'ctx> ItemRef<'ctx> {
    pub fn id(&self) -> ItemId {
        match self {
            ItemRef::Type(d) => d.def_id.into(),
            ItemRef::Fun(d) => d.def_id.into(),
            ItemRef::Global(d) => d.def_id.into(),
            ItemRef::TraitDecl(d) => d.def_id.into(),
            ItemRef::TraitImpl(d) => d.def_id.into(),
        }
    }

    pub fn clone(&self) -> ItemByVal {
        match *self {
            Self::Type(d) => ItemByVal::Type(d.clone()),
            Self::Fun(d) => ItemByVal::Fun(d.clone()),
            Self::Global(d) => ItemByVal::Global(d.clone()),
            Self::TraitDecl(d) => ItemByVal::TraitDecl(d.clone()),
            Self::TraitImpl(d) => ItemByVal::TraitImpl(d.clone()),
        }
    }

    pub fn item_meta(&self) -> &'ctx ItemMeta {
        match self {
            Self::Type(d) => &d.item_meta,
            Self::Fun(d) => &d.item_meta,
            Self::Global(d) => &d.item_meta,
            Self::TraitDecl(d) => &d.item_meta,
            Self::TraitImpl(d) => &d.item_meta,
        }
    }
    /// The generic parameters of this item.
    pub fn generic_params(&self) -> &'ctx GenericParams {
        match self {
            ItemRef::Type(d) => &d.generics,
            ItemRef::Fun(d) => &d.generics,
            ItemRef::Global(d) => &d.generics,
            ItemRef::TraitDecl(d) => &d.generics,
            ItemRef::TraitImpl(d) => &d.generics,
        }
    }

    /// Get information about the parent of this item, if any.
    pub fn parent_info(&self) -> &'ctx ItemSource {
        match self {
            ItemRef::Fun(d) => &d.src,
            ItemRef::Global(d) => &d.src,
            ItemRef::Type(_) | ItemRef::TraitDecl(_) | ItemRef::TraitImpl(_) => {
                &ItemSource::TopLevel
            }
        }
    }

    /// See [`GenericParams::identity_args`].
    pub fn identity_args(&self) -> GenericArgs {
        self.generic_params().identity_args()
    }

    /// We can't implement `AstVisitable` because of the `'static` constraint, but it's ok because
    /// `ItemRef` isn't contained in any of our types.
    pub fn drive<V: VisitAst>(&self, visitor: &mut V) -> ControlFlow<V::Break> {
        match *self {
            ItemRef::Type(d) => visitor.visit(d),
            ItemRef::Fun(d) => visitor.visit(d),
            ItemRef::Global(d) => visitor.visit(d),
            ItemRef::TraitDecl(d) => visitor.visit(d),
            ItemRef::TraitImpl(d) => visitor.visit(d),
        }
    }

    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    pub fn dyn_visit<T: AstVisitable>(&self, f: impl FnMut(&T)) {
        match *self {
            ItemRef::Type(d) => d.dyn_visit(f),
            ItemRef::Fun(d) => d.dyn_visit(f),
            ItemRef::Global(d) => d.dyn_visit(f),
            ItemRef::TraitDecl(d) => d.dyn_visit(f),
            ItemRef::TraitImpl(d) => d.dyn_visit(f),
        }
    }
}

impl<'ctx> ItemRefMut<'ctx> {
    pub fn as_ref(&self) -> ItemRef<'_> {
        match self {
            ItemRefMut::Type(d) => ItemRef::Type(d),
            ItemRefMut::Fun(d) => ItemRef::Fun(d),
            ItemRefMut::Global(d) => ItemRef::Global(d),
            ItemRefMut::TraitDecl(d) => ItemRef::TraitDecl(d),
            ItemRefMut::TraitImpl(d) => ItemRef::TraitImpl(d),
        }
    }
    pub fn reborrow(&mut self) -> ItemRefMut<'_> {
        match self {
            ItemRefMut::Type(d) => ItemRefMut::Type(d),
            ItemRefMut::Fun(d) => ItemRefMut::Fun(d),
            ItemRefMut::Global(d) => ItemRefMut::Global(d),
            ItemRefMut::TraitDecl(d) => ItemRefMut::TraitDecl(d),
            ItemRefMut::TraitImpl(d) => ItemRefMut::TraitImpl(d),
        }
    }

    pub fn set_id(&mut self, id: ItemId) {
        match (self, id) {
            (Self::Type(d), ItemId::Type(id)) => d.def_id = id,
            (Self::Fun(d), ItemId::Fun(id)) => d.def_id = id,
            (Self::Global(d), ItemId::Global(id)) => d.def_id = id,
            (Self::TraitDecl(d), ItemId::TraitDecl(id)) => d.def_id = id,
            (Self::TraitImpl(d), ItemId::TraitImpl(id)) => d.def_id = id,
            _ => unreachable!(),
        }
    }

    pub fn item_meta(&mut self) -> &mut ItemMeta {
        match self {
            Self::Type(d) => &mut d.item_meta,
            Self::Fun(d) => &mut d.item_meta,
            Self::Global(d) => &mut d.item_meta,
            Self::TraitDecl(d) => &mut d.item_meta,
            Self::TraitImpl(d) => &mut d.item_meta,
        }
    }
    /// The generic parameters of this item.
    pub fn generic_params(&mut self) -> &mut GenericParams {
        match self {
            ItemRefMut::Type(d) => &mut d.generics,
            ItemRefMut::Fun(d) => &mut d.generics,
            ItemRefMut::Global(d) => &mut d.generics,
            ItemRefMut::TraitDecl(d) => &mut d.generics,
            ItemRefMut::TraitImpl(d) => &mut d.generics,
        }
    }

    /// We can't implement `AstVisitable` because of the `'static` constraint, but it's ok because
    /// `ItemRefMut` isn't contained in any of our types.
    pub fn drive_mut<V: VisitAstMut>(&mut self, visitor: &mut V) -> ControlFlow<V::Break> {
        match self {
            ItemRefMut::Type(d) => visitor.visit(*d),
            ItemRefMut::Fun(d) => visitor.visit(*d),
            ItemRefMut::Global(d) => visitor.visit(*d),
            ItemRefMut::TraitDecl(d) => visitor.visit(*d),
            ItemRefMut::TraitImpl(d) => visitor.visit(*d),
        }
    }

    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    pub fn dyn_visit_mut<T: AstVisitable>(&mut self, f: impl FnMut(&mut T)) {
        match self {
            ItemRefMut::Type(d) => d.dyn_visit_mut(f),
            ItemRefMut::Fun(d) => d.dyn_visit_mut(f),
            ItemRefMut::Global(d) => d.dyn_visit_mut(f),
            ItemRefMut::TraitDecl(d) => d.dyn_visit_mut(f),
            ItemRefMut::TraitImpl(d) => d.dyn_visit_mut(f),
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

pub trait HasIdxMapOf<Id: Idx>: std::ops::Index<Id, Output: Sized> {
    fn get_idx_map(&self) -> &IndexMap<Id, Self::Output>;
    fn get_idx_map_mut(&mut self) -> &mut IndexMap<Id, Self::Output>;
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
        impl HasIdxMapOf<$idx> for $ty {
            fn get_idx_map(&self) -> &IndexMap<$idx, Self::Output> {
                &self.$field
            }
            fn get_idx_map_mut(&mut self) -> &mut IndexMap<$idx, Self::Output> {
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
