use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;

use index_vec::Idx;

use crate::ast::*;
use crate::common::TAB_INCR;
use crate::ids::Vector;
use crate::pretty::FmtWithCtx;

pub trait IntoFormatter {
    type C: AstFormatter;
    fn into_fmt(self) -> Self::C;
}

/// An [`AstFormatter`] contains the context required to pretty-print the ast. An ast type can then
/// be pretty-printed using the [`FmtWithCtx`] trait.
pub trait AstFormatter: Sized {
    type Reborrow<'a>: AstFormatter + 'a
    where
        Self: 'a;

    fn get_crate(&self) -> Option<&TranslatedCrate>;

    fn set_generics<'a>(&'a self, generics: &'a GenericParams) -> Self::Reborrow<'a>;
    fn set_locals<'a>(&'a self, locals: &'a Locals) -> Self::Reborrow<'a>;
    fn push_binder<'a>(&'a self, new_params: Cow<'a, GenericParams>) -> Self::Reborrow<'a>;
    fn push_bound_regions<'a>(
        &'a self,
        regions: &'a Vector<RegionId, RegionVar>,
    ) -> Self::Reborrow<'a> {
        self.push_binder(Cow::Owned(GenericParams {
            regions: regions.clone(),
            ..Default::default()
        }))
    }

    fn increase_indent<'a>(&'a self) -> Self::Reborrow<'a>;
    fn indent(&self) -> String;

    fn format_local_id(&self, f: &mut fmt::Formatter<'_>, id: LocalId) -> fmt::Result;
    fn format_bound_var<Id: Idx + Display, T>(
        &self,
        f: &mut fmt::Formatter<'_>,
        var: DeBruijnVar<Id>,
        var_prefix: &str,
        fmt_var: impl Fn(&T) -> Option<String>,
    ) -> fmt::Result
    where
        GenericParams: HasVectorOf<Id, Output = T>;

    fn format_enum_variant(
        &self,
        f: &mut fmt::Formatter<'_>,
        type_id: TypeDeclId,
        variant_id: VariantId,
    ) -> fmt::Result {
        let variant = if let Some(translated) = self.get_crate()
            && let Some(def) = translated.type_decls.get(type_id)
            && let Some(variants) = def.kind.as_enum()
        {
            &variants.get(variant_id).unwrap().name
        } else {
            &variant_id.to_pretty_string()
        };
        write!(f, "{}::{variant}", type_id.with_ctx(self))
    }

    fn format_field_name(
        &self,
        f: &mut fmt::Formatter<'_>,
        type_id: TypeDeclId,
        opt_variant_id: Option<VariantId>,
        field_id: FieldId,
    ) -> fmt::Result {
        let field_name = if let Some(translated) = self.get_crate()
            && let Some(def) = translated.type_decls.get(type_id)
        {
            match (&def.kind, opt_variant_id) {
                (TypeDeclKind::Enum(variants), Some(variant_id)) => {
                    variants[variant_id].fields[field_id].name.as_ref()
                }
                (TypeDeclKind::Struct(fields) | TypeDeclKind::Union(fields), None) => {
                    fields[field_id].name.as_ref()
                }
                _ => None,
            }
        } else {
            None
        };
        if let Some(field_name) = field_name {
            write!(f, "{field_name}")
        } else {
            write!(f, "{field_id}")
        }
    }
}

/// Context for formatting.
#[derive(Default)]
pub struct FmtCtx<'a> {
    pub translated: Option<&'a TranslatedCrate>,
    /// Generics form a stack, where each binder introduces a new level. For DeBruijn indices to
    /// work, we keep the innermost parameters at the start of the vector.
    pub generics: BindingStack<Cow<'a, GenericParams>>,
    pub locals: Option<&'a Locals>,
    pub indent_level: usize,
}

impl<'c> AstFormatter for FmtCtx<'c> {
    type Reborrow<'a>
        = FmtCtx<'a>
    where
        Self: 'a;

    fn get_crate(&self) -> Option<&TranslatedCrate> {
        self.translated
    }

    fn set_generics<'a>(&'a self, generics: &'a GenericParams) -> Self::Reborrow<'a> {
        FmtCtx {
            generics: BindingStack::new(Cow::Borrowed(generics)),
            ..self.reborrow()
        }
    }
    fn set_locals<'a>(&'a self, locals: &'a Locals) -> Self::Reborrow<'a> {
        FmtCtx {
            locals: Some(locals),
            ..self.reborrow()
        }
    }
    fn push_binder<'a>(&'a self, new_params: Cow<'a, GenericParams>) -> Self::Reborrow<'a> {
        let mut ret = self.reborrow();
        ret.generics.push(new_params);
        ret
    }

    fn increase_indent<'a>(&'a self) -> Self::Reborrow<'a> {
        FmtCtx {
            indent_level: self.indent_level + 1,
            ..self.reborrow()
        }
    }
    fn indent(&self) -> String {
        TAB_INCR.repeat(self.indent_level)
    }

    fn format_local_id(&self, f: &mut fmt::Formatter<'_>, id: LocalId) -> fmt::Result {
        if let Some(locals) = &self.locals
            && let Some(v) = locals.locals.get(id)
        {
            write!(f, "{v}")
        } else {
            write!(f, "{}", id.to_pretty_string())
        }
    }

    fn format_bound_var<Id: Idx + Display, T>(
        &self,
        f: &mut fmt::Formatter<'_>,
        var: DeBruijnVar<Id>,
        var_prefix: &str,
        fmt_var: impl Fn(&T) -> Option<String>,
    ) -> fmt::Result
    where
        GenericParams: HasVectorOf<Id, Output = T>,
    {
        if self.generics.is_empty() {
            return write!(f, "{var_prefix}{var}");
        }
        match self.generics.get_var::<_, GenericParams>(var) {
            None => write!(f, "missing({var_prefix}{var})"),
            Some(v) => match fmt_var(v) {
                Some(name) => write!(f, "{name}"),
                None => {
                    write!(f, "{var_prefix}")?;
                    let (dbid, varid) = self.generics.as_bound_var(var);
                    if dbid == self.generics.depth() {
                        write!(f, "{varid}")
                    } else {
                        write!(f, "{var}")
                    }
                }
            },
        }
    }
}

impl<'a> FmtCtx<'a> {
    pub fn new() -> Self {
        FmtCtx::default()
    }

    pub fn get_item(&self, id: AnyTransId) -> Result<AnyTransItem<'_>, Option<&Name>> {
        let Some(translated) = &self.translated else {
            return Err(None);
        };
        translated
            .get_item(id)
            .ok_or_else(|| translated.item_short_name(id))
    }

    /// Print the whole definition.
    pub fn format_decl_id(&self, id: impl Into<AnyTransId>) -> String {
        let id = id.into();
        match self.get_item(id) {
            Ok(d) => d.to_string_with_ctx(self),
            Err(opt_name) => {
                let opt_name = opt_name
                    .map(|n| format!(" ({})", n.with_ctx(self)))
                    .unwrap_or_default();
                format!("Missing decl: {id:?}{opt_name}")
            }
        }
    }

    fn reborrow<'b>(&'b self) -> FmtCtx<'b> {
        FmtCtx {
            translated: self.translated.as_deref(),
            generics: self.generics.clone(),
            locals: self.locals.as_deref(),
            indent_level: self.indent_level,
        }
    }
}
