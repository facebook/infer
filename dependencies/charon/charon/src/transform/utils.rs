use crate::ast::*;
use crate::formatter::FmtCtx;
use crate::pretty::FmtWithCtx;
use derive_generic_visitor::*;
use macros::EnumIsA;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

/// Each `GenericArgs` is meant for a corresponding `GenericParams`; this describes which one.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, EnumIsA, Deserialize, Drive, DriveMut)]
pub enum GenericsSource {
    /// A top-level item.
    Item(AnyTransId),
    /// A trait method.
    Method(TraitDeclId, TraitItemName),
    /// A builtin item like `Box`.
    Builtin,
    /// Some other use of generics outside the main Charon ast.
    Other,
}

impl GenericsSource {
    pub fn item<I: Into<AnyTransId>>(id: I) -> Self {
        Self::Item(id.into())
    }

    /// Return a path that represents the target item.
    pub fn item_name(&self, translated: &TranslatedCrate, fmt_ctx: &FmtCtx) -> String {
        match self {
            GenericsSource::Item(id) => translated
                .item_name(*id)
                .unwrap()
                .to_string_with_ctx(fmt_ctx),
            GenericsSource::Method(trait_id, method_name) => format!(
                "{}::{method_name}",
                translated
                    .item_name(*trait_id)
                    .unwrap()
                    .to_string_with_ctx(fmt_ctx),
            ),
            GenericsSource::Builtin => format!("<built-in>"),
            GenericsSource::Other => format!("<unknown>"),
        }
    }
}

impl TypeId {
    pub fn generics_target(&self) -> GenericsSource {
        match *self {
            TypeId::Adt(decl_id) => GenericsSource::item(decl_id),
            TypeId::Tuple | TypeId::Builtin(..) => GenericsSource::Builtin,
        }
    }
}
impl FunId {
    pub fn generics_target(&self) -> GenericsSource {
        match *self {
            FunId::Regular(fun_id) => GenericsSource::item(fun_id),
            FunId::Builtin(..) => GenericsSource::Builtin,
        }
    }
}
impl FunIdOrTraitMethodRef {
    pub fn generics_target(&self) -> GenericsSource {
        match self {
            FunIdOrTraitMethodRef::Fun(fun_id) => fun_id.generics_target(),
            FunIdOrTraitMethodRef::Trait(trait_ref, name, _) => {
                GenericsSource::Method(trait_ref.trait_decl_ref.skip_binder.id, name.clone())
            }
        }
    }
}
