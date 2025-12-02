//! Utilities for pretty-printing (u)llbc.
use crate::{
    common::{TAB_INCR, repeat_except_first},
    formatter::*,
    gast,
    llbc_ast::{self as llbc, *},
    reorder_decls::*,
    transform::utils::GenericsSource,
    ullbc_ast::{self as ullbc, *},
};
use either::Either;
use itertools::Itertools;
use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
};

pub struct WithCtx<'a, C, T: ?Sized> {
    val: &'a T,
    ctx: &'a C,
}

impl<'a, C, T: ?Sized> Display for WithCtx<'a, C, T>
where
    T: FmtWithCtx<C>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.val.fmt_with_ctx(self.ctx, f)
    }
}

/// Format the AST type as a string.
pub trait FmtWithCtx<C> {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// Returns a struct that implements `Display`. This allows the following:
    /// ```text
    ///     println!("{}", self.with_ctx(ctx));
    /// ```
    fn with_ctx<'a>(&'a self, ctx: &'a C) -> WithCtx<'a, C, Self> {
        WithCtx { val: self, ctx }
    }

    fn to_string_with_ctx(&self, ctx: &C) -> String {
        self.with_ctx(ctx).to_string()
    }
}

macro_rules! impl_display_via_ctx {
    ($ty:ty) => {
        impl Display for $ty {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.fmt_with_ctx(&FmtCtx::new(), f)
            }
        }
    };
}
macro_rules! impl_debug_via_display {
    ($ty:ty) => {
        impl Debug for $ty {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                <_ as Display>::fmt(self, f)
            }
        }
    };
}

//------- Impls, sorted by name --------

impl<C: AstFormatter> FmtWithCtx<C> for AbortKind {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbortKind::Panic(name) => {
                write!(f, "panic")?;
                if let Some(name) = name {
                    write!(f, "({})", name.with_ctx(ctx))?;
                }
                Ok(())
            }
            AbortKind::UndefinedBehavior => write!(f, "undefined_behavior"),
            AbortKind::UnwindTerminate => write!(f, "unwind_terminate"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for AnyTransId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match ctx
            .get_crate()
            .and_then(|translated| translated.item_short_name(*self))
        {
            None => write!(f, "{self}"),
            Some(name) => name.fmt_with_ctx(ctx, f),
        }
    }
}

impl Display for AnyTransId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        let s = match self {
            AnyTransId::Type(x) => x.to_pretty_string(),
            AnyTransId::Fun(x) => x.to_pretty_string(),
            AnyTransId::Global(x) => x.to_pretty_string(),
            AnyTransId::TraitDecl(x) => x.to_pretty_string(),
            AnyTransId::TraitImpl(x) => x.to_pretty_string(),
        };
        f.write_str(&s)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for AnyTransItem<'_> {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnyTransItem::Type(d) => write!(f, "{}", d.with_ctx(ctx)),
            AnyTransItem::Fun(d) => write!(f, "{}", d.with_ctx(ctx)),
            AnyTransItem::Global(d) => write!(f, "{}", d.with_ctx(ctx)),
            AnyTransItem::TraitDecl(d) => write!(f, "{}", d.with_ctx(ctx)),
            AnyTransItem::TraitImpl(d) => write!(f, "{}", d.with_ctx(ctx)),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Assert {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "assert({} == {})",
            self.cond.with_ctx(ctx),
            self.expected,
        )
    }
}

impl<T> Binder<T> {
    /// Format the parameters and contents of this binder and returns the resulting strings. Note:
    /// this assumes the binder fully replaces the existing generics.
    fn fmt_split<'a, C>(&'a self, ctx: &'a C) -> (String, String)
    where
        C: AstFormatter,
        T: FmtWithCtx<C::Reborrow<'a>>,
    {
        let ctx = &ctx.push_binder(Cow::Borrowed(&self.params));
        (
            self.params.fmt_with_ctx_single_line(ctx),
            self.skip_binder.to_string_with_ctx(ctx),
        )
    }
}

impl Display for OverflowMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            OverflowMode::Panic => write!(f, "panic"),
            OverflowMode::Wrap => write!(f, "wrap"),
            OverflowMode::UB => write!(f, "ub"),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            BinOp::BitXor => write!(f, "^"),
            BinOp::BitAnd => write!(f, "&"),
            BinOp::BitOr => write!(f, "|"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Ge => write!(f, ">="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Add(mode) => write!(f, "{}.+", mode),
            BinOp::Sub(mode) => write!(f, "{}.-", mode),
            BinOp::Mul(mode) => write!(f, "{}.*", mode),
            BinOp::Div(mode) => write!(f, "{}./", mode),
            BinOp::Rem(mode) => write!(f, "{}.%", mode),
            BinOp::AddChecked => write!(f, "checked.+"),
            BinOp::SubChecked => write!(f, "checked.-"),
            BinOp::MulChecked => write!(f, "checked.*"),
            BinOp::Shl(mode) => write!(f, "{}.<<", mode),
            BinOp::Shr(mode) => write!(f, "{}.>>", mode),
            BinOp::Cmp => write!(f, "cmp"),
            BinOp::Offset => write!(f, "offset"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for llbc::Block {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for st in &self.statements {
            writeln!(f, "{}", st.with_ctx(ctx))?;
        }
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ullbc::BlockData {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{};", statement.with_ctx(ctx))?;
        }
        write!(f, "{};", self.terminator.with_ctx(ctx))?;
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for gast::Body {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Body::Unstructured(b) => write!(f, "{}", b.with_ctx(ctx)),
            Body::Structured(b) => write!(f, "{}", b.with_ctx(ctx)),
        }
    }
}

impl Display for BorrowKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        // Reuse the derived `Debug` impl to get the variant name.
        write!(f, "{self:?}")
    }
}

impl Display for BuiltinFunId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        let name = match *self {
            BuiltinFunId::BoxNew => "BoxNew",
            BuiltinFunId::ArrayToSliceShared => "ArrayToSliceShared",
            BuiltinFunId::ArrayToSliceMut => "ArrayToSliceMut",
            BuiltinFunId::ArrayRepeat => "ArrayRepeat",
            BuiltinFunId::Index(BuiltinIndexOp {
                is_array,
                mutability,
                is_range,
            }) => {
                let ty = if is_array { "Array" } else { "Slice" };
                let op = if is_range { "SubSlice" } else { "Index" };
                let mutability = mutability.variant_name();
                &format!("{ty}{op}{mutability}")
            }
            BuiltinFunId::PtrFromParts(mutability) => {
                let mutability = mutability.variant_name();
                &format!("PtrFromParts{mutability}")
            }
        };
        f.write_str(name)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Call {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let dest = self.dest.with_ctx(ctx);
        let func = self.func.with_ctx(ctx);
        let args = self.args.iter().map(|x| x.with_ctx(ctx)).format(", ");
        write!(f, "{dest} := {func}({args})")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for CastKind {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CastKind::Scalar(src, tgt) => write!(f, "cast<{src}, {tgt}>"),
            CastKind::FnPtr(src, tgt) | CastKind::RawPtr(src, tgt) => {
                write!(f, "cast<{}, {}>", src.with_ctx(ctx), tgt.with_ctx(ctx))
            }
            CastKind::Unsize(src, tgt, meta) => {
                write!(
                    f,
                    "unsize_cast<{}, {}",
                    src.with_ctx(ctx),
                    tgt.with_ctx(ctx),
                )?;
                match meta {
                    UnsizingMetadata::Length(len) => write!(f, ", {}", len.with_ctx(ctx))?,
                    UnsizingMetadata::VTablePtr(tref) => write!(f, ", {}", tref.with_ctx(ctx))?,
                    UnsizingMetadata::Unknown => {}
                }
                write!(f, ">")
            }
            CastKind::Transmute(src, tgt) => {
                write!(f, "transmute<{}, {}>", src.with_ctx(ctx), tgt.with_ctx(ctx))
            }
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ClauseDbVar {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.format_bound_var(f, *self, "@TraitClause", |_| None)
    }
}

impl_display_via_ctx!(ConstantExpr);
impl<C: AstFormatter> FmtWithCtx<C> for ConstantExpr {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.with_ctx(ctx))
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ConstGeneric {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstGeneric::Var(id) => write!(f, "{}", id.with_ctx(ctx)),
            ConstGeneric::Value(v) => write!(f, "{v}"),
            ConstGeneric::Global(id) => write!(f, "{}", id.with_ctx(ctx)),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ConstGenericDbVar {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.format_bound_var(f, *self, "@ConstGeneric", |v| Some(v.to_string()))
    }
}

impl_display_via_ctx!(ConstGenericVar);
impl<C: AstFormatter> FmtWithCtx<C> for ConstGenericVar {
    fn fmt_with_ctx(&self, _ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {} : {}", self.name, self.ty)
    }
}

impl Display for DeBruijnId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{}", self.index)
    }
}

impl<Id: Display> Display for DeBruijnVar<Id> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bound(dbid, varid) => write!(f, "{dbid}_{varid}"),
            Self::Free(varid) => write!(f, "{varid}"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for DeclarationGroup {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DeclarationGroup::*;
        match self {
            Type(g) => write!(f, "Type decls group: {}", g.with_ctx(ctx)),
            Fun(g) => write!(f, "Fun decls group: {}", g.with_ctx(ctx)),
            Global(g) => write!(f, "Global decls group: {}", g.with_ctx(ctx)),
            TraitDecl(g) => write!(f, "Trait decls group: {}", g.with_ctx(ctx)),
            TraitImpl(g) => write!(f, "Trait impls group: {}", g.with_ctx(ctx)),
            Mixed(g) => write!(f, "Mixed group: {}", g.with_ctx(ctx)),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for DynPredicate {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ctx = &ctx.push_binder(Cow::Borrowed(&self.binder.params));
        let ty = self.binder.skip_binder.with_ctx(ctx);
        let clauses = self.binder.params.formatted_clauses(ctx).format(" + ");
        write!(f, "exists<{ty}> {clauses}")
    }
}

impl_display_via_ctx!(Field);
impl<C: AstFormatter> FmtWithCtx<C> for Field {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: ", name)?
        }
        write!(f, "{}", self.ty.with_ctx(ctx))
    }
}

impl Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            FileName::Virtual(path_buf) | FileName::Local(path_buf) => {
                write!(f, "{}", path_buf.display())
            }
            FileName::NotReal(name) => write!(f, "{}", name),
        }
    }
}

impl Display for FloatTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            FloatTy::F16 => write!(f, "f16"),
            FloatTy::F32 => write!(f, "f32"),
            FloatTy::F64 => write!(f, "f64"),
            FloatTy::F128 => write!(f, "f128"),
        }
    }
}

impl Display for FloatValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        let v = &self.value;
        match self.ty {
            FloatTy::F16 => write!(f, "{v} : f16"),
            FloatTy::F32 => write!(f, "{v} : f32"),
            FloatTy::F64 => write!(f, "{v} : f64"),
            FloatTy::F128 => write!(f, "{v} : f128"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FnOperand {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FnOperand::Regular(func) => write!(f, "{}", func.with_ctx(ctx)),
            FnOperand::Move(p) => write!(f, "(move {})", p.with_ctx(ctx)),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FnPtr {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.func.as_ref() {
            FunIdOrTraitMethodRef::Fun(FunId::Regular(def_id)) => {
                write!(f, "{}", def_id.with_ctx(ctx))?
            }
            FunIdOrTraitMethodRef::Fun(FunId::Builtin(builtin)) => write!(f, "@{}", builtin)?,
            FunIdOrTraitMethodRef::Trait(trait_ref, method_id, _) => {
                write!(f, "{}::{}", trait_ref.with_ctx(ctx), &method_id.0)?
            }
        };
        write!(f, "{}", self.generics.with_ctx(ctx))
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FunDecl {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = if self.signature.is_unsafe {
            "unsafe fn"
        } else {
            "fn"
        };
        self.item_meta
            .fmt_item_intro(f, ctx, keyword, self.def_id)?;

        // Update the context
        let ctx = &ctx.set_generics(&self.signature.generics);

        // Generic parameters
        let (params, preds) = self.signature.generics.fmt_with_ctx_with_trait_clauses(ctx);
        write!(f, "{params}")?;

        // Arguments
        let mut args: Vec<String> = Vec::new();
        for (i, ty) in self.signature.inputs.iter().enumerate() {
            // The input variables start at index 1
            // TODO: use the locals to get the variable names
            let id = LocalId::new(i + 1);
            args.push(format!("{}: {}", id.to_pretty_string(), ty.with_ctx(ctx)));
        }
        let args = args.join(", ");
        write!(f, "({args})")?;

        // Return type
        if !self.signature.output.is_unit() {
            write!(f, " -> {}", self.signature.output.with_ctx(ctx))?;
        };
        write!(f, "{preds}")?;

        // Body
        match &self.body {
            Ok(body) => {
                let tab = ctx.indent();
                let body = body.with_ctx(ctx);
                write!(f, "\n{tab}{{\n{body}{tab}}}")?;
            }
            Err(Opaque) => {}
        }

        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FunDeclId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AnyTransId::from(*self).fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FunDeclRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.with_ctx(ctx);
        let generics = self.generics.with_ctx(ctx);
        write!(f, "{id}{generics}")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for FunSig {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ctx = &ctx.set_generics(&self.generics);

        // Unsafe keyword
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }

        // Generic parameters
        let (params, clauses) = self.generics.fmt_with_ctx_with_trait_clauses(ctx);
        write!(f, "fn{params}")?;

        // Arguments
        let args = self
            .inputs
            .iter()
            .map(|ty| ty.with_ctx(ctx).to_string())
            .format(", ");
        write!(f, "({args})")?;

        // Return type
        if !self.output.is_unit() {
            write!(f, " -> {}", self.output.with_ctx(ctx))?;
        }

        write!(f, "{clauses}")?;
        Ok(())
    }
}

impl<Id: Copy, C: AstFormatter> FmtWithCtx<C> for GDeclarationGroup<Id>
where
    Id: FmtWithCtx<C>,
{
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use GDeclarationGroup::*;
        match self {
            NonRec(id) => write!(f, "Non rec: {}", id.with_ctx(ctx)),
            Rec(ids) => {
                let ids = ids.iter().map(|id| id.with_ctx(ctx)).format(", ");
                write!(f, "Rec: {}", ids)
            }
        }
    }
}

impl GenericArgs {
    pub(crate) fn fmt_explicits<'a, C: AstFormatter>(
        &'a self,
        ctx: &'a C,
    ) -> impl Iterator<Item = impl Display + 'a> {
        let regions = self.regions.iter().map(|x| x.with_ctx(ctx));
        let types = self.types.iter().map(|x| x.with_ctx(ctx));
        let const_generics = self.const_generics.iter().map(|x| x.with_ctx(ctx));
        regions.map(Either::Left).chain(
            types
                .map(Either::Left)
                .chain(const_generics.map(Either::Right))
                .map(Either::Right),
        )
    }

    pub(crate) fn fmt_implicits<'a, C: AstFormatter>(
        &'a self,
        ctx: &'a C,
    ) -> impl Iterator<Item = impl Display + 'a> {
        self.trait_refs.iter().map(|x| x.with_ctx(ctx))
    }
}

impl_display_via_ctx!(GenericArgs);
impl_debug_via_display!(GenericArgs);
impl<C: AstFormatter> FmtWithCtx<C> for GenericArgs {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_explicits() {
            write!(f, "<{}>", self.fmt_explicits(ctx).format(", "))?;
        }
        if self.has_implicits() {
            write!(f, "[{}]", self.fmt_implicits(ctx).format(", "))?;
        }
        Ok(())
    }
}

impl GenericParams {
    fn formatted_params<'a, C>(&'a self, ctx: &'a C) -> impl Iterator<Item = impl Display + 'a>
    where
        C: AstFormatter,
    {
        let regions = self.regions.iter().map(|x| x.with_ctx(ctx));
        let types = self.types.iter();
        let const_generics = self.const_generics.iter();
        regions.map(Either::Left).chain(
            types
                .map(Either::Left)
                .chain(const_generics.map(Either::Right))
                .map(Either::Right),
        )
    }

    fn formatted_clauses<'a, C>(&'a self, ctx: &'a C) -> impl Iterator<Item = impl Display + 'a>
    where
        C: AstFormatter,
    {
        let trait_clauses = self.trait_clauses.iter().map(|x| x.to_string_with_ctx(ctx));
        let types_outlive = self.types_outlive.iter().map(|x| x.fmt_as_for(ctx));
        let regions_outlive = self.regions_outlive.iter().map(|x| x.fmt_as_for(ctx));
        let type_constraints = self
            .trait_type_constraints
            .iter()
            .map(|x| x.fmt_as_for(ctx));
        trait_clauses.map(Either::Left).chain(
            types_outlive
                .chain(regions_outlive)
                .chain(type_constraints)
                .map(Either::Right),
        )
    }

    pub fn fmt_with_ctx_with_trait_clauses<C>(&self, ctx: &C) -> (String, String)
    where
        C: AstFormatter,
    {
        let tab = ctx.indent();
        let params = if self.has_explicits() {
            let params = self.formatted_params(ctx).format(", ");
            format!("<{}>", params)
        } else {
            String::new()
        };
        let clauses = if self.has_predicates() {
            let clauses = self
                .formatted_clauses(ctx)
                .map(|x| format!("\n{tab}{TAB_INCR}{x},"))
                .format("");
            format!("\n{tab}where{clauses}")
        } else {
            String::new()
        };
        (params, clauses)
    }

    pub fn fmt_with_ctx_single_line<C>(&self, ctx: &C) -> String
    where
        C: AstFormatter,
    {
        if self.is_empty() {
            String::new()
        } else {
            let params = self
                .formatted_params(ctx)
                .map(Either::Left)
                .chain(self.formatted_clauses(ctx).map(Either::Right))
                .format(", ");
            format!("<{}>", params)
        }
    }
}

impl_debug_via_display!(GenericParams);
impl Display for GenericParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{}", self.fmt_with_ctx_single_line(&FmtCtx::new()))
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for GenericsSource {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericsSource::Item(id) => write!(f, "{}", id.with_ctx(ctx)),
            GenericsSource::Method(id, name) => write!(f, "{}::{name}", id.with_ctx(ctx)),
            GenericsSource::Builtin => write!(f, "<builtin>"),
            GenericsSource::Other => write!(f, "<unknown>"),
        }
    }
}

impl<T> GExprBody<T> {
    fn fmt_with_ctx_and_callback<C: AstFormatter>(
        &self,
        ctx: &C,
        f: &mut fmt::Formatter<'_>,
        fmt_body: impl FnOnce(
            &mut fmt::Formatter<'_>,
            &<<C as AstFormatter>::Reborrow<'_> as AstFormatter>::Reborrow<'_>,
            &T,
        ) -> fmt::Result,
    ) -> fmt::Result {
        // Update the context
        let ctx = &ctx.set_locals(&self.locals);
        let ctx = &ctx.increase_indent();
        let tab = ctx.indent();

        // Format the local variables
        for v in &self.locals.locals {
            write!(f, "{tab}")?;
            write!(f, "let {v}: {};", v.ty.with_ctx(ctx))?;

            write!(f, " // ")?;
            let index = v.index.index();
            if index == 0 {
                write!(f, "return")?;
            } else if index <= self.locals.arg_count {
                write!(f, "arg #{index}")?
            } else {
                match &v.name {
                    Some(_) => write!(f, "local")?,
                    None => write!(f, "anonymous local")?,
                }
            }
            writeln!(f)?;
        }

        fmt_body(f, ctx, &self.body)?;

        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for GExprBody<llbc_ast::Block> {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Inference fails when this is a closure.
        fn fmt_body<C: AstFormatter>(
            f: &mut fmt::Formatter<'_>,
            ctx: &<<C as AstFormatter>::Reborrow<'_> as AstFormatter>::Reborrow<'_>,
            body: &Block,
        ) -> Result<(), fmt::Error> {
            writeln!(f)?;
            body.fmt_with_ctx(ctx, f)?;
            Ok(())
        }
        self.fmt_with_ctx_and_callback(ctx, f, fmt_body::<C>)
    }
}
impl<C: AstFormatter> FmtWithCtx<C> for GExprBody<ullbc_ast::BodyContents> {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Inference fails when this is a closure.
        fn fmt_body<C: AstFormatter>(
            f: &mut fmt::Formatter<'_>,
            ctx: &<<C as AstFormatter>::Reborrow<'_> as AstFormatter>::Reborrow<'_>,
            body: &Vector<BlockId, BlockData>,
        ) -> Result<(), fmt::Error> {
            let tab = ctx.indent();
            let ctx = &ctx.increase_indent();
            for (bid, block) in body.iter_indexed_values() {
                writeln!(f)?;
                writeln!(f, "{tab}bb{}: {{", bid.index())?;
                writeln!(f, "{}", block.with_ctx(ctx))?;
                writeln!(f, "{tab}}}")?;
            }
            Ok(())
        }
        self.fmt_with_ctx_and_callback(ctx, f, fmt_body::<C>)
    }
}

impl<C> FmtWithCtx<C> for GlobalDecl
where
    C: AstFormatter,
{
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = match self.global_kind {
            GlobalKind::Static => "static",
            GlobalKind::AnonConst | GlobalKind::NamedConst => "const",
        };
        self.item_meta
            .fmt_item_intro(f, ctx, keyword, self.def_id)?;

        // Update the context with the generics
        let ctx = &ctx.set_generics(&self.generics);

        // Translate the parameters and the trait clauses
        let (params, preds) = self.generics.fmt_with_ctx_with_trait_clauses(ctx);

        // Type
        let ty = self.ty.with_ctx(ctx);
        write!(f, "{params}: {ty}")?;

        // Predicates
        write!(f, "{preds}")?;
        if self.generics.has_predicates() {
            write!(f, "\n")?;
        }
        write!(f, " ")?;

        // Decl name
        let initializer = self.init.with_ctx(ctx);
        write!(f, "= {initializer}()")?;

        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for GlobalDeclId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AnyTransId::from(*self).fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for GlobalDeclRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.with_ctx(ctx);
        let generics = self.generics.with_ctx(ctx);
        write!(f, "{id}{generics}")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ImplElem {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        match self {
            ImplElem::Ty(bound_ty) => {
                // Just printing the generics (not the predicates)
                let ctx = ctx.set_generics(&bound_ty.params);
                bound_ty.skip_binder.fmt_with_ctx(&ctx, f)?
            }
            ImplElem::Trait(impl_id) => {
                match ctx.get_crate().and_then(|tr| tr.trait_impls.get(*impl_id)) {
                    None => write!(f, "impl#{impl_id}")?,
                    Some(timpl) => {
                        // We need to put the first type parameter aside: it is the type for which
                        // we implement the trait.
                        let ctx = &ctx.set_generics(&timpl.generics);
                        let mut impl_trait = timpl.impl_trait.clone();
                        match impl_trait
                            .generics
                            .types
                            .remove_and_shift_ids(TypeVarId::ZERO)
                        {
                            Some(self_ty) => {
                                let self_ty = self_ty.with_ctx(ctx);
                                let impl_trait = impl_trait.with_ctx(ctx);
                                write!(f, "impl {impl_trait} for {self_ty}")?;
                            }
                            // TODO(mono): A monomorphized trait doesn't take arguments.
                            None => {
                                let impl_trait = impl_trait.with_ctx(ctx);
                                write!(f, "impl {impl_trait}")?;
                            }
                        }
                    }
                }
            }
        }
        write!(f, "}}")
    }
}

impl Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            IntTy::Isize => write!(f, "isize"),
            IntTy::I8 => write!(f, "i8"),
            IntTy::I16 => write!(f, "i16"),
            IntTy::I32 => write!(f, "i32"),
            IntTy::I64 => write!(f, "i64"),
            IntTy::I128 => write!(f, "i128"),
        }
    }
}

impl Display for UIntTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            UIntTy::Usize => write!(f, "usize"),
            UIntTy::U8 => write!(f, "u8"),
            UIntTy::U16 => write!(f, "u16"),
            UIntTy::U32 => write!(f, "u32"),
            UIntTy::U64 => write!(f, "u64"),
            UIntTy::U128 => write!(f, "u128"),
        }
    }
}

impl Display for IntegerTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            IntegerTy::Signed(int_ty) => write!(f, "{int_ty}"),
            IntegerTy::Unsigned(uint_ty) => write!(f, "{uint_ty}"),
        }
    }
}

impl ItemMeta {
    /// Format the start of an item definition, up to the name.
    pub fn fmt_item_intro<C: AstFormatter>(
        &self,
        f: &mut fmt::Formatter<'_>,
        ctx: &C,
        keyword: &str,
        id: impl Into<AnyTransId>,
    ) -> fmt::Result {
        let tab = ctx.indent();
        let full_name = self.name.with_ctx(ctx);
        let name = if let Some(tr) = ctx.get_crate()
            && let Some(short_name) = tr.short_names.get(&id.into())
        {
            writeln!(f, "// Full name: {full_name}")?;
            short_name.with_ctx(ctx)
        } else {
            full_name
        };
        if let Some(id) = &self.lang_item {
            writeln!(f, "{tab}#[lang_item(\"{id}\")]")?;
        }
        write!(f, "{tab}")?;
        if self.attr_info.public {
            write!(f, "pub ")?;
        }
        write!(f, "{keyword} {name}")
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            Literal::Scalar(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            Literal::Bool(v) => write!(f, "{v}"),
            Literal::Char(v) => write!(f, "{v}"),
            Literal::Str(v) => write!(f, "\"{}\"", v.replace("\\", "\\\\").replace("\n", "\\n")),
            Literal::ByteStr(v) => write!(f, "{v:?}"),
        }
    }
}

impl Display for LiteralTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralTy::Int(ty) => write!(f, "{ty}"),
            LiteralTy::UInt(ty) => write!(f, "{ty}"),
            LiteralTy::Float(ty) => write!(f, "{ty}"),
            LiteralTy::Char => write!(f, "char"),
            LiteralTy::Bool => write!(f, "bool"),
        }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We display both the variable name and its id because some
        // variables may have the same name (in different scopes)
        if let Some(name) = &self.name {
            write!(f, "{name}")?
        }
        write!(f, "{}", self.index.to_pretty_string())?;
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for LocalId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.format_local_id(f, *self)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Name {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.iter().map(|x| x.with_ctx(ctx)).format("::");
        write!(f, "{}", name)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for NullOp {
    fn fmt_with_ctx(&self, _ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            NullOp::SizeOf => "size_of",
            NullOp::AlignOf => "align_of",
            NullOp::OffsetOf(_) => "offset_of(?)",
            NullOp::UbChecks => "ub_checks",
        };
        write!(f, "{op}")
    }
}

impl_display_via_ctx!(Operand);
impl<C: AstFormatter> FmtWithCtx<C> for Operand {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Copy(p) => write!(f, "copy ({})", p.with_ctx(ctx)),
            Operand::Move(p) => write!(f, "move ({})", p.with_ctx(ctx)),
            Operand::Const(c) => write!(f, "const ({})", c.with_ctx(ctx)),
        }
    }
}

impl<C: AstFormatter, T, U> FmtWithCtx<C> for OutlivesPred<T, U>
where
    T: FmtWithCtx<C>,
    U: FmtWithCtx<C>,
{
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.0.with_ctx(ctx), self.1.with_ctx(ctx))
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for PathElem {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathElem::Ident(s, d) => {
                write!(f, "{s}")?;
                if !d.is_zero() {
                    write!(f, "#{}", d)?;
                }
                Ok(())
            }
            PathElem::Impl(impl_elem) => {
                write!(f, "{}", impl_elem.with_ctx(ctx))
            }
            PathElem::Monomorphized(args) => {
                write!(f, "<{}>", args.fmt_explicits(ctx).format(", "))
            }
        }
    }
}

impl_display_via_ctx!(Place);
impl<C: AstFormatter> FmtWithCtx<C> for Place {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PlaceKind::Local(var_id) => write!(f, "{}", var_id.with_ctx(ctx)),
            PlaceKind::Global(global_ref) => global_ref.fmt_with_ctx(ctx, f),
            PlaceKind::Projection(subplace, projection) => {
                let sub = subplace.with_ctx(ctx);
                match projection {
                    ProjectionElem::Deref => {
                        write!(f, "*({sub})")
                    }
                    ProjectionElem::Field(proj_kind, field_id) => match proj_kind {
                        FieldProjKind::Adt(adt_id, opt_variant_id) => {
                            write!(f, "({sub}")?;
                            if let Some(variant_id) = opt_variant_id {
                                write!(f, " as variant ")?;
                                ctx.format_enum_variant(f, *adt_id, *variant_id)?;
                            }
                            write!(f, ").")?;
                            ctx.format_field_name(f, *adt_id, *opt_variant_id, *field_id)?;
                            Ok(())
                        }
                        FieldProjKind::Tuple(_) => {
                            write!(f, "({sub}).{field_id}")
                        }
                    },
                    ProjectionElem::Index {
                        offset,
                        from_end: true,
                        ..
                    } => write!(f, "({sub})[-{}]", offset.with_ctx(ctx)),
                    ProjectionElem::Index {
                        offset,
                        from_end: false,
                        ..
                    } => write!(f, "({sub})[{}]", offset.with_ctx(ctx)),
                    ProjectionElem::Subslice {
                        from,
                        to,
                        from_end: true,
                        ..
                    } => write!(f, "({sub})[{}..-{}]", from.with_ctx(ctx), to.with_ctx(ctx)),
                    ProjectionElem::Subslice {
                        from,
                        to,
                        from_end: false,
                        ..
                    } => write!(f, "({sub})[{}..{}]", from.with_ctx(ctx), to.with_ctx(ctx)),
                }
            }
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for PolyTraitDeclRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fmt_as_for(ctx))
    }
}

impl Display for RawAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{}", self.path)?;
        if let Some(args) = &self.args {
            write!(f, "({args})")?;
        }
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for RawConstantExpr {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawConstantExpr::Literal(c) => write!(f, "{}", c.to_string()),
            RawConstantExpr::Adt(variant_id, values) => {
                // It is a bit annoying: in order to properly format the value,
                // we need the type (which contains the type def id).
                // Anyway, the printing utilities are mostly for debugging.
                let variant_id = match variant_id {
                    Some(id) => format!("Some({id})"),
                    None => "None".to_string(),
                };
                let values = values.iter().map(|v| v.with_ctx(ctx)).format(", ");
                write!(f, "ConstAdt {} [{}]", variant_id, values)
            }
            RawConstantExpr::Array(values) => {
                let values = values.iter().map(|v| v.with_ctx(ctx)).format(", ");
                write!(f, "[{}]", values)
            }
            RawConstantExpr::Global(global_ref) => {
                write!(f, "{}", global_ref.with_ctx(ctx))
            }
            RawConstantExpr::TraitConst(trait_ref, name) => {
                write!(f, "{}::{name}", trait_ref.with_ctx(ctx),)
            }
            RawConstantExpr::Ref(cv) => {
                write!(f, "&{}", cv.with_ctx(ctx))
            }
            RawConstantExpr::Ptr(rk, cv) => match rk {
                RefKind::Mut => write!(f, "&raw mut {}", cv.with_ctx(ctx)),
                RefKind::Shared => write!(f, "&raw const {}", cv.with_ctx(ctx)),
            },
            RawConstantExpr::Var(id) => write!(f, "{}", id.with_ctx(ctx)),
            RawConstantExpr::FnPtr(fp) => {
                write!(f, "{}", fp.with_ctx(ctx))
            }
            RawConstantExpr::PtrNoProvenance(v) => write!(f, "no-provenance {v}"),
            RawConstantExpr::RawMemory(bytes) => write!(f, "RawMemory({bytes:?})"),
            RawConstantExpr::Opaque(cause) => write!(f, "Opaque({cause})"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Region {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Region::Static => write!(f, "'static"),
            Region::Var(var) => write!(f, "{}", var.with_ctx(ctx)),
            Region::Erased => write!(f, "'_"),
        }
    }
}

impl<T> RegionBinder<T> {
    /// Format the parameters and contents of this binder and returns the resulting strings.
    fn fmt_split<'a, C>(&'a self, ctx: &'a C) -> (String, String)
    where
        C: AstFormatter,
        T: FmtWithCtx<C::Reborrow<'a>>,
    {
        let ctx = &ctx.push_bound_regions(&self.regions);
        (
            self.regions
                .iter()
                .map(|r| r.with_ctx(ctx))
                .format(", ")
                .to_string(),
            self.skip_binder.to_string_with_ctx(ctx),
        )
    }

    /// Formats the binder as `for<params> value`.
    fn fmt_as_for<'a, C>(&'a self, ctx: &'a C) -> String
    where
        C: AstFormatter,
        T: FmtWithCtx<C::Reborrow<'a>>,
    {
        let (regions, value) = self.fmt_split(ctx);
        let regions = if regions.is_empty() {
            "".to_string()
        } else {
            format!("for<{regions}> ",)
        };
        format!("{regions}{value}",)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for RegionDbVar {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.format_bound_var(f, *self, "'_", |v| {
            v.name.as_ref().map(|name| name.to_string())
        })
    }
}

impl_display_via_ctx!(RegionVar);
impl<C: AstFormatter> FmtWithCtx<C> for RegionVar {
    fn fmt_with_ctx(&self, _ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "'_{}", self.index),
        }
    }
}

impl_display_via_ctx!(Rvalue);
impl<C: AstFormatter> FmtWithCtx<C> for Rvalue {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rvalue::Use(x) => write!(f, "{}", x.with_ctx(ctx)),
            Rvalue::Ref(place, borrow_kind) => {
                let borrow_kind = match borrow_kind {
                    BorrowKind::Shared => "&",
                    BorrowKind::Mut => "&mut ",
                    BorrowKind::TwoPhaseMut => "&two-phase-mut ",
                    BorrowKind::UniqueImmutable => "&uniq ",
                    BorrowKind::Shallow => "&shallow ",
                };
                write!(f, "{borrow_kind}{}", place.with_ctx(ctx))
            }
            Rvalue::RawPtr(place, mutability) => {
                let ptr_kind = match mutability {
                    RefKind::Shared => "&raw const ",
                    RefKind::Mut => "&raw mut ",
                };
                write!(f, "{ptr_kind}{}", place.with_ctx(ctx))
            }

            Rvalue::BinaryOp(binop, x, y) => {
                write!(f, "{} {} {}", x.with_ctx(ctx), binop, y.with_ctx(ctx))
            }
            Rvalue::UnaryOp(unop, x) => {
                write!(f, "{}({})", unop.with_ctx(ctx), x.with_ctx(ctx))
            }
            Rvalue::NullaryOp(op, ty) => {
                write!(f, "{}<{}>", op.with_ctx(ctx), ty.with_ctx(ctx))
            }
            Rvalue::Discriminant(p) => {
                write!(f, "@discriminant({})", p.with_ctx(ctx),)
            }
            Rvalue::Aggregate(kind, ops) => {
                let ops_s = ops.iter().map(|op| op.with_ctx(ctx)).format(", ");
                match kind {
                    AggregateKind::Adt(ty_ref, variant_id, field_id) => {
                        match ty_ref.id {
                            TypeId::Tuple => write!(f, "({})", ops_s),
                            TypeId::Builtin(_) => unreachable!(),
                            TypeId::Adt(ty_id) => {
                                match variant_id {
                                    None => ty_id.fmt_with_ctx(ctx, f)?,
                                    Some(variant_id) => {
                                        ctx.format_enum_variant(f, ty_id, *variant_id)?
                                    }
                                }
                                write!(f, " {{ ")?;
                                for (comma, (i, op)) in
                                    repeat_except_first(", ").zip(ops.iter().enumerate())
                                {
                                    write!(f, "{}", comma.unwrap_or_default())?;
                                    let field_id = match *field_id {
                                        None => FieldId::new(i),
                                        Some(field_id) => {
                                            assert_eq!(i, 0); // there should be only one operand
                                            field_id
                                        }
                                    };
                                    ctx.format_field_name(f, ty_id, *variant_id, field_id)?;
                                    write!(f, ": {}", op.with_ctx(ctx))?;
                                }
                                write!(f, " }}")
                            }
                        }
                    }
                    AggregateKind::Array(..) => {
                        write!(f, "[{}]", ops_s)
                    }
                    AggregateKind::RawPtr(_, rmut) => {
                        let mutability = match rmut {
                            RefKind::Shared => "const",
                            RefKind::Mut => "mut ",
                        };
                        write!(f, "*{} ({})", mutability, ops_s)
                    }
                }
            }
            Rvalue::Len(place, ..) => write!(f, "len({})", place.with_ctx(ctx)),
            Rvalue::Repeat(op, _ty, cg) => {
                write!(f, "[{}; {}]", op.with_ctx(ctx), cg.with_ctx(ctx))
            }
            Rvalue::ShallowInitBox(op, ty) => {
                write!(
                    f,
                    "shallow_init_box::<{}>({})",
                    ty.with_ctx(ctx),
                    op.with_ctx(ctx)
                )
            }
        }
    }
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            ScalarValue::Signed(ty, v) => write!(f, "{v} : {}", ty),
            ScalarValue::Unsigned(ty, v) => write!(f, "{v} : {}", ty),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for ullbc::Statement {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tab = ctx.indent();
        use ullbc::RawStatement;
        for line in &self.comments_before {
            writeln!(f, "{tab}// {line}")?;
        }
        match &self.content {
            RawStatement::Assign(place, rvalue) => write!(
                f,
                "{tab}{} := {}",
                place.with_ctx(ctx),
                rvalue.with_ctx(ctx),
            ),
            RawStatement::SetDiscriminant(place, variant_id) => write!(
                f,
                "{tab}@discriminant({}) := {}",
                place.with_ctx(ctx),
                variant_id
            ),
            RawStatement::CopyNonOverlapping(box CopyNonOverlapping { src, dst, count }) => write!(
                f,
                "{}copy_nonoverlapping({}, {}, {})",
                tab,
                src.with_ctx(ctx),
                dst.with_ctx(ctx),
                count.with_ctx(ctx),
            ),
            RawStatement::StorageLive(var_id) => {
                write!(f, "{tab}storage_live({})", var_id.with_ctx(ctx))
            }
            RawStatement::StorageDead(var_id) => {
                write!(f, "{tab}storage_dead({})", var_id.with_ctx(ctx))
            }
            RawStatement::Deinit(place) => {
                write!(f, "{tab}deinit({})", place.with_ctx(ctx))
            }
            RawStatement::Drop(place, tref) => {
                write!(
                    f,
                    "{tab}drop[{}] {}",
                    tref.with_ctx(ctx),
                    place.with_ctx(ctx),
                )
            }
            RawStatement::Assert(assert) => write!(f, "{tab}{}", assert.with_ctx(ctx)),
            RawStatement::Nop => write!(f, "{tab}nop"),
            RawStatement::Error(s) => write!(f, "{tab}@Error({})", s),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for llbc::Statement {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tab = ctx.indent();
        use llbc::RawStatement;
        for line in &self.comments_before {
            writeln!(f, "{tab}// {line}")?;
        }
        write!(f, "{tab}")?;
        match &self.content {
            RawStatement::Assign(place, rvalue) => {
                write!(f, "{} := {}", place.with_ctx(ctx), rvalue.with_ctx(ctx),)
            }
            RawStatement::SetDiscriminant(place, variant_id) => write!(
                f,
                "@discriminant({}) := {}",
                place.with_ctx(ctx),
                variant_id
            ),
            RawStatement::CopyNonOverlapping(box CopyNonOverlapping { src, dst, count }) => write!(
                f,
                "copy_nonoverlapping({}, {}, {})",
                src.with_ctx(ctx),
                dst.with_ctx(ctx),
                count.with_ctx(ctx),
            ),
            RawStatement::StorageLive(var_id) => {
                write!(f, "storage_live({})", var_id.with_ctx(ctx))
            }
            RawStatement::StorageDead(var_id) => {
                write!(f, "storage_dead({})", var_id.with_ctx(ctx))
            }
            RawStatement::Deinit(place) => {
                write!(f, "deinit({})", place.with_ctx(ctx))
            }
            RawStatement::Drop(place, tref) => {
                write!(f, "drop[{}] {}", tref.with_ctx(ctx), place.with_ctx(ctx),)
            }
            RawStatement::Assert(assert) => {
                write!(f, "{}", assert.with_ctx(ctx),)
            }
            RawStatement::Call(call) => {
                write!(f, "{}", call.with_ctx(ctx))
            }
            RawStatement::Abort(kind) => {
                write!(f, "{}", kind.with_ctx(ctx))
            }
            RawStatement::Return => write!(f, "return"),
            RawStatement::Break(index) => write!(f, "break {index}"),
            RawStatement::Continue(index) => write!(f, "continue {index}"),
            RawStatement::Nop => write!(f, "nop"),
            RawStatement::Switch(switch) => match switch {
                Switch::If(discr, true_st, false_st) => {
                    let ctx = &ctx.increase_indent();
                    write!(
                        f,
                        "if {} {{\n{}{tab}}}\n{tab}else {{\n{}{tab}}}",
                        discr.with_ctx(ctx),
                        true_st.with_ctx(ctx),
                        false_st.with_ctx(ctx),
                    )
                }
                Switch::SwitchInt(discr, _ty, maps, otherwise) => {
                    writeln!(f, "switch {} {{", discr.with_ctx(ctx))?;
                    let ctx1 = &ctx.increase_indent();
                    let inner_tab1 = ctx1.indent();
                    let ctx2 = &ctx1.increase_indent();
                    for (pvl, st) in maps {
                        // Note that there may be several pattern values
                        let pvl = pvl.iter().format(" | ");
                        writeln!(
                            f,
                            "{inner_tab1}{} => {{\n{}{inner_tab1}}},",
                            pvl,
                            st.with_ctx(ctx2),
                        )?;
                    }
                    writeln!(
                        f,
                        "{inner_tab1}_ => {{\n{}{inner_tab1}}},",
                        otherwise.with_ctx(ctx2),
                    )?;
                    write!(f, "{tab}}}")
                }
                Switch::Match(discr, maps, otherwise) => {
                    writeln!(f, "match {} {{", discr.with_ctx(ctx))?;
                    let ctx1 = &ctx.increase_indent();
                    let inner_tab1 = ctx1.indent();
                    let ctx2 = &ctx1.increase_indent();
                    let discr_type: Option<TypeDeclId> = discr
                        .ty
                        .kind()
                        .as_adt()
                        .and_then(|tref| tref.id.as_adt())
                        .copied();
                    for (cases, st) in maps {
                        write!(f, "{inner_tab1}",)?;
                        // Note that there may be several pattern values
                        for (bar, v) in repeat_except_first(" | ").zip(cases.iter()) {
                            write!(f, "{}", bar.unwrap_or_default())?;
                            match discr_type {
                                Some(type_id) => ctx.format_enum_variant(f, type_id, *v)?,
                                None => write!(f, "{}", v.to_pretty_string())?,
                            }
                        }
                        writeln!(f, " => {{\n{}{inner_tab1}}},", st.with_ctx(ctx2),)?;
                    }
                    if let Some(otherwise) = otherwise {
                        writeln!(
                            f,
                            "{inner_tab1}_ => {{\n{}{inner_tab1}}},",
                            otherwise.with_ctx(ctx2),
                        )?;
                    }
                    write!(f, "{tab}}}")
                }
            },
            RawStatement::Loop(body) => {
                let ctx = &ctx.increase_indent();
                write!(f, "loop {{\n{}{tab}}}", body.with_ctx(ctx))
            }
            RawStatement::Error(s) => write!(f, "@ERROR({})", s),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Terminator {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tab = ctx.indent();
        for line in &self.comments_before {
            writeln!(f, "{tab}// {line}")?;
        }
        write!(f, "{tab}")?;
        match &self.content {
            RawTerminator::Goto { target } => write!(f, "goto bb{target}"),
            RawTerminator::Switch { discr, targets } => match targets {
                SwitchTargets::If(true_block, false_block) => write!(
                    f,
                    "if {} -> bb{} else -> bb{}",
                    discr.with_ctx(ctx),
                    true_block,
                    false_block
                ),
                SwitchTargets::SwitchInt(_ty, maps, otherwise) => {
                    let maps = maps
                        .iter()
                        .map(|(v, bid)| format!("{}: bb{}", v.to_string(), bid))
                        .chain([format!("otherwise: bb{otherwise}")])
                        .format(", ");
                    write!(f, "switch {} -> {}", discr.with_ctx(ctx), maps)
                }
            },
            RawTerminator::Call {
                call,
                target,
                on_unwind,
            } => {
                let call = call.with_ctx(ctx);
                write!(f, "{call} -> bb{target} (unwind: bb{on_unwind})",)
            }
            RawTerminator::Abort(kind) => write!(f, "{}", kind.with_ctx(ctx)),
            RawTerminator::Return => write!(f, "return"),
            RawTerminator::UnwindResume => write!(f, "unwind_continue"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitClause {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let clause_id = self.clause_id.to_pretty_string();
        let trait_ = self.trait_.with_ctx(ctx);
        write!(f, "[{clause_id}]: {trait_}")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitDecl {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Update the context
        let ctx = &ctx.set_generics(&self.generics);

        self.item_meta
            .fmt_item_intro(f, ctx, "trait", self.def_id)?;

        let (generics, clauses) = self.generics.fmt_with_ctx_with_trait_clauses(ctx);
        write!(f, "{generics}{clauses}")?;

        let any_item = !self.parent_clauses.is_empty()
            || !self.type_clauses.is_empty()
            || !self.consts.is_empty()
            || !self.types.is_empty()
            || self.methods().count() > 0;
        if any_item {
            write!(f, "\n{{\n")?;
            for c in &self.parent_clauses {
                writeln!(
                    f,
                    "{TAB_INCR}parent_clause{} : {}",
                    c.clause_id,
                    c.with_ctx(ctx)
                )?;
            }
            for (name, clauses) in &self.type_clauses {
                for c in clauses {
                    writeln!(
                        f,
                        "{TAB_INCR}item_clause_{name}_{} : {}",
                        c.clause_id.to_string(),
                        c.with_ctx(ctx)
                    )?;
                }
            }
            for (name, ty) in &self.consts {
                let ty = ty.with_ctx(ctx);
                writeln!(f, "{TAB_INCR}const {name} : {ty}")?;
            }
            for name in &self.types {
                writeln!(f, "{TAB_INCR}type {name}")?;
            }
            for (name, bound_fn) in self.methods() {
                let (params, fn_ref) = bound_fn.fmt_split(ctx);
                writeln!(f, "{TAB_INCR}fn {name}{params} = {fn_ref}")?;
            }
            if let Some(vtb_ref) = &self.vtable {
                writeln!(f, "{TAB_INCR}vtable: {}", vtb_ref.with_ctx(ctx))?;
            } else {
                writeln!(f, "{TAB_INCR}non-dyn-compatible")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitDeclId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AnyTransId::from(*self).fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitDeclRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_id = self.id.with_ctx(ctx);
        let generics = self.generics.with_ctx(ctx);
        write!(f, "{trait_id}{generics}")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitImpl {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let full_name = self.item_meta.name.with_ctx(ctx);
        writeln!(f, "// Full name: {full_name}")?;

        // Update the context
        let ctx = &ctx.set_generics(&self.generics);

        let (generics, clauses) = self.generics.fmt_with_ctx_with_trait_clauses(ctx);
        write!(f, "impl{generics} ")?;
        let mut impl_trait = self.impl_trait.clone();
        match impl_trait
            .generics
            .types
            .remove_and_shift_ids(TypeVarId::ZERO)
        {
            Some(self_ty) => {
                let self_ty = self_ty.with_ctx(ctx);
                let impl_trait = impl_trait.with_ctx(ctx);
                write!(f, "{impl_trait} for {self_ty}")?;
            }
            // TODO(mono): A monomorphized trait doesn't take arguments.
            None => {
                let impl_trait = impl_trait.with_ctx(ctx);
                write!(f, "{impl_trait}")?;
            }
        }
        write!(f, "{clauses}")?;

        let newline = if clauses.is_empty() {
            " ".to_string()
        } else {
            "\n".to_string()
        };
        write!(f, "{newline}{{")?;

        let any_item = !self.parent_trait_refs.is_empty()
            || !self.type_clauses.is_empty()
            || !self.consts.is_empty()
            || !self.types.is_empty()
            || self.methods().count() > 0;
        if any_item {
            writeln!(f)?;
            for (i, c) in self.parent_trait_refs.iter().enumerate() {
                let i = TraitClauseId::new(i);
                writeln!(f, "{TAB_INCR}parent_clause{i} = {}", c.with_ctx(ctx))?;
            }
            for (name, clauses) in &self.type_clauses {
                for (i, c) in clauses.iter().enumerate() {
                    let i = TraitClauseId::new(i);
                    writeln!(f, "{TAB_INCR}item_clause_{name}_{i} = {}", c.with_ctx(ctx))?;
                }
            }
            for (name, global) in &self.consts {
                writeln!(f, "{TAB_INCR}const {name} = {}", global.with_ctx(ctx))?;
            }
            for (name, ty) in &self.types {
                writeln!(f, "{TAB_INCR}type {name} = {}", ty.with_ctx(ctx))?;
            }
            for (name, bound_fn) in self.methods() {
                let (params, fn_ref) = bound_fn.fmt_split(ctx);
                writeln!(f, "{TAB_INCR}fn {name}{params} = {fn_ref}")?;
            }
        }
        if let Some(vtb_ref) = &self.vtable {
            writeln!(f, "{TAB_INCR}vtable: {}", vtb_ref.with_ctx(ctx))?;
        } else {
            writeln!(f, "{TAB_INCR}non-dyn-compatible")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitImplId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AnyTransId::from(*self).fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitImplRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.with_ctx(ctx);
        let generics = self.generics.with_ctx(ctx);
        write!(f, "{id}{generics}")
    }
}

impl Display for TraitItemName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitRefKind {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TraitRefKind::SelfId => write!(f, "Self"),
            TraitRefKind::ParentClause(sub, clause_id) => {
                let sub = sub.with_ctx(ctx);
                write!(f, "{sub}::parent_clause{clause_id}")
            }
            TraitRefKind::ItemClause(sub, type_name, clause_id) => {
                let sub = sub.with_ctx(ctx);
                // Using on purpose `to_pretty_string` instead of `with_ctx`: the clause is local
                // to the associated type, so it should not be referenced in the current context.
                let clause = clause_id.to_pretty_string();
                write!(f, "({sub}::{type_name}::[{clause}])")
            }
            TraitRefKind::TraitImpl(impl_ref) => {
                write!(f, "{}", impl_ref.with_ctx(ctx))
            }
            TraitRefKind::Clause(id) => write!(f, "{}", id.with_ctx(ctx)),
            TraitRefKind::BuiltinOrAuto {
                trait_decl_ref: tr,
                types,
                ..
            } => {
                write!(f, "{}", tr.with_ctx(ctx))?;
                if !types.is_empty() {
                    let types = types
                        .iter()
                        .map(|(name, ty, _)| {
                            let ty = ty.with_ctx(ctx);
                            format!("{name}  = {ty}")
                        })
                        .join(", ");
                    write!(f, " where {types}")?;
                }
                Ok(())
            }
            TraitRefKind::Dyn(tr) => write!(f, "{}", tr.with_ctx(ctx)),
            TraitRefKind::Unknown(msg) => write!(f, "UNKNOWN({msg})"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TraitTypeConstraint {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_ref = self.trait_ref.with_ctx(ctx);
        let ty = self.ty.with_ctx(ctx);
        write!(f, "{}::{} = {}", trait_ref, self.type_name, ty)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for RegionBinder<(Vec<Ty>, Ty)> {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Update the bound regions
        let ctx = &ctx.push_bound_regions(&self.regions);

        write!(f, "fn")?;
        if !self.regions.is_empty() {
            write!(
                f,
                "<{}>",
                self.regions.iter().map(|r| r.with_ctx(ctx)).format(", ")
            )?;
        }
        let (inputs, output) = &self.skip_binder;
        let inputs = inputs.iter().map(|x| x.with_ctx(ctx)).format(", ");
        write!(f, "({inputs})")?;
        if !output.is_unit() {
            let output = output.with_ctx(ctx);
            write!(f, " -> {output}")?;
        }
        Ok(())
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for Ty {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TyKind::Adt(tref) => {
                if tref.id.is_tuple() {
                    assert!(tref.generics.trait_refs.is_empty());
                    let generics = tref.generics.fmt_explicits(ctx).format(", ");
                    write!(f, "({generics})")
                } else {
                    write!(f, "{}", tref.with_ctx(ctx))
                }
            }
            TyKind::TypeVar(id) => write!(f, "{}", id.with_ctx(ctx)),
            TyKind::Literal(kind) => write!(f, "{kind}"),
            TyKind::Never => write!(f, "!"),
            TyKind::Ref(r, ty, kind) => {
                write!(f, "&{} ", r.with_ctx(ctx))?;
                if let RefKind::Mut = kind {
                    write!(f, "mut ")?;
                }
                write!(f, "({})", ty.with_ctx(ctx))
            }
            TyKind::RawPtr(ty, kind) => {
                write!(f, "*")?;
                match kind {
                    RefKind::Shared => write!(f, "const")?,
                    RefKind::Mut => write!(f, "mut")?,
                }
                write!(f, " {}", ty.with_ctx(ctx))
            }
            TyKind::TraitType(trait_ref, name) => {
                write!(f, "{}::{name}", trait_ref.with_ctx(ctx),)
            }
            TyKind::DynTrait(pred) => {
                write!(f, "(dyn {})", pred.with_ctx(ctx))
            }
            TyKind::FnPtr(io) => {
                write!(f, "{}", io.with_ctx(ctx))
            }
            TyKind::FnDef(binder) => {
                let (regions, value) = binder.fmt_split(ctx);
                if !regions.is_empty() {
                    write!(f, "for<{regions}> ",)?
                };
                write!(f, "{value}",)
            }
            TyKind::Error(msg) => write!(f, "type_error(\"{msg}\")"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TypeDbVar {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.format_bound_var(f, *self, "@Type", |v| Some(v.to_string()))
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TypeDecl {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = match &self.kind {
            TypeDeclKind::Struct(..) => "struct",
            TypeDeclKind::Union(..) => "union",
            TypeDeclKind::Enum(..) => "enum",
            TypeDeclKind::Alias(..) => "type",
            TypeDeclKind::Opaque | TypeDeclKind::Error(..) => "opaque type",
        };
        self.item_meta
            .fmt_item_intro(f, ctx, keyword, self.def_id)?;

        let ctx = &ctx.set_generics(&self.generics);
        let (params, preds) = self.generics.fmt_with_ctx_with_trait_clauses(ctx);
        write!(f, "{params}{preds}")?;

        let nl_or_space = if !self.generics.has_predicates() {
            " ".to_string()
        } else {
            "\n".to_string()
        };
        match &self.kind {
            TypeDeclKind::Struct(fields) => {
                write!(f, "{nl_or_space}{{")?;
                if !fields.is_empty() {
                    writeln!(f)?;
                    for field in fields {
                        writeln!(f, "  {},", field.with_ctx(ctx))?;
                    }
                }
                write!(f, "}}")
            }
            TypeDeclKind::Union(fields) => {
                write!(f, "{nl_or_space}{{")?;
                writeln!(f)?;
                for field in fields {
                    writeln!(f, "  {},", field.with_ctx(ctx))?;
                }
                write!(f, "}}")
            }
            TypeDeclKind::Enum(variants) => {
                write!(f, "{nl_or_space}{{")?;
                writeln!(f)?;
                for variant in variants {
                    writeln!(f, "  {},", variant.with_ctx(ctx))?;
                }
                write!(f, "}}")
            }
            TypeDeclKind::Alias(ty) => write!(f, " = {}", ty.with_ctx(ctx)),
            TypeDeclKind::Opaque => write!(f, ""),
            TypeDeclKind::Error(msg) => write!(f, " = ERROR({msg})"),
        }
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TypeDeclId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        AnyTransId::from(*self).fmt_with_ctx(ctx, f)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TypeDeclRef {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.with_ctx(ctx);
        let generics = self.generics.with_ctx(ctx);
        write!(f, "{id}{generics}")
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for TypeId {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeId::Tuple => Ok(()),
            TypeId::Adt(def_id) => write!(f, "{}", def_id.with_ctx(ctx)),
            TypeId::Builtin(aty) => write!(f, "{}", aty.get_name().with_ctx(ctx)),
        }
    }
}

impl_display_via_ctx!(TypeVar);
impl<C: AstFormatter> FmtWithCtx<C> for TypeVar {
    fn fmt_with_ctx(&self, _ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<C: AstFormatter> FmtWithCtx<C> for UnOp {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Not => write!(f, "~"),
            UnOp::Neg(mode) => write!(f, "{}.-", mode),
            UnOp::PtrMetadata => write!(f, "ptr_metadata"),
            UnOp::Cast(kind) => write!(f, "{}", kind.with_ctx(ctx)),
        }
    }
}

impl_display_via_ctx!(Variant);
impl<C: AstFormatter> FmtWithCtx<C> for Variant {
    fn fmt_with_ctx(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.fields.is_empty() {
            let fields = self.fields.iter().map(|f| f.with_ctx(ctx)).format(", ");
            write!(f, "({})", fields)?;
        }
        Ok(())
    }
}
