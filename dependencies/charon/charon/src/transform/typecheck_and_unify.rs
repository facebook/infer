// Check that types are consistent and unify body lifetimes (best-effort).
use derive_generic_visitor::*;
use index_vec::Idx;
use itertools::Itertools;
use petgraph::{graph::IndexType, unionfind::UnionFind};
use std::{borrow::Cow, fmt::Display};

use crate::{
    ast::*,
    errors::Level,
    formatter::{AstFormatter, FmtCtx, IntoFormatter},
    pretty::FmtWithCtx,
    transform::utils::GenericsSource,
};

use crate::transform::{TransformCtx, ctx::TransformPass};

unsafe impl IndexType for RegionId {
    fn new(x: usize) -> Self {
        RegionId::new(x)
    }
    fn index(&self) -> usize {
        <RegionId as Idx>::index(*self)
    }
    fn max() -> Self {
        RegionId::new(RegionId::MAX_INDEX)
    }
}

#[derive(Visitor)]
struct TypeCheckVisitor<'a> {
    ctx: &'a TransformCtx,
    phase: Check,
    /// Tracks an enclosing span for error reporting.
    span: Span,
    /// Track the binders seen so far.
    // We can't keep the params by reference because the visitors don't tell us that everything
    // we're visiting has lifetime `'a`.
    binder_stack: BindingStack<GenericParams>,
    /// Remember the names of the types visited up to here.
    visit_stack: Vec<&'static str>,
    body_lt_unifier: Option<UnionFind<RegionId>>,
}

impl VisitorWithSpan for TypeCheckVisitor<'_> {
    fn current_span(&mut self) -> &mut Span {
        &mut self.span
    }
}
impl VisitorWithBinderStack for TypeCheckVisitor<'_> {
    fn binder_stack_mut(&mut self) -> &mut BindingStack<GenericParams> {
        &mut self.binder_stack
    }
}

struct TypeError;

impl TypeCheckVisitor<'_> {
    fn error(&self, message: impl Display) {
        let msg = format!(
            "Type error {}:\n{message}\n\
            Visitor stack:\n  {}\n\
            Binding stack (depth {}):\n  {}",
            self.phase.name(),
            self.visit_stack.iter().rev().join("\n  "),
            self.binder_stack.len(),
            self.binder_stack
                .iter_enumerated()
                .map(|(i, params)| format!("{i}: {params}"))
                .join("\n  "),
        );
        // This is a fatal error: the output llbc is inconsistent and should not be used.
        self.ctx.span_err(self.span, &msg, Level::ERROR);
    }

    fn match_regions(&mut self, a: &Region, b: &Region) -> Result<(), TypeError> {
        match (a, b) {
            (Region::Body(a), Region::Body(b)) => {
                if let Some(unifier) = &mut self.body_lt_unifier {
                    unifier.union(*a, *b);
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Compare two types for equality. Because we can't use the full power of rustc's type
    /// equality, we err on the side of accepting.
    fn match_tys(&mut self, a: &Ty, b: &Ty) -> Result<(), TypeError> {
        match (a.kind(), b.kind()) {
            (TyKind::Adt(a), TyKind::Adt(b)) if a.id == b.id => {
                self.match_generics(&a.generics, &b.generics)?
            }
            (TyKind::Literal(a), TyKind::Literal(b)) if a == b => {}
            (TyKind::Never, TyKind::Never) => {}
            (TyKind::Array(aty, _), TyKind::Array(bty, _)) => {
                self.match_tys(aty, bty)?;
            }
            (TyKind::Slice(aty), TyKind::Slice(bty)) => {
                self.match_tys(aty, bty)?;
            }
            (TyKind::Ref(aregion, aty, akind), TyKind::Ref(bregion, bty, bkind))
                if akind == bkind =>
            {
                self.match_regions(aregion, bregion)?;
                self.match_tys(aty, bty)?;
            }
            (TyKind::RawPtr(aty, akind), TyKind::RawPtr(bty, bkind)) if akind == bkind => {
                self.match_tys(aty, bty)?
            }
            (TyKind::DynTrait(..), TyKind::DynTrait(..)) => {}
            (TyKind::FnPtr(..), TyKind::FnPtr(..)) => {}
            (TyKind::FnDef(..), TyKind::FnDef(..)) => {}
            // We can't decide type equality, so we avoid false positives here.
            (TyKind::TypeVar(_), _) | (_, TyKind::TypeVar(_)) => {}
            (TyKind::TraitType(..), _) | (_, TyKind::TraitType(..)) => {}
            (TyKind::PtrMetadata(..), _) | (_, TyKind::PtrMetadata(..)) => {}
            (TyKind::Error(_), _) | (_, TyKind::Error(_)) => {}
            _ => return Err(TypeError),
        }
        Ok(())
    }

    fn match_trait_decl_refs(
        &mut self,
        a: &TraitDeclRef,
        b: &TraitDeclRef,
    ) -> Result<(), TypeError> {
        if a.id != b.id {
            return Err(TypeError);
        }
        self.match_generics(&a.generics, &b.generics)?;
        Ok(())
    }

    fn match_poly_trait_decl_refs(
        &mut self,
        a: &RegionBinder<TraitDeclRef>,
        b: &RegionBinder<TraitDeclRef>,
    ) -> Result<(), TypeError> {
        let a = a.clone().erase();
        let b = b.clone().erase();
        self.match_trait_decl_refs(&a, &b)?;
        Ok(())
    }

    fn match_trait_ref_against_itself(&mut self, tref: &TraitRef) -> Result<(), TypeError> {
        match &tref.kind {
            TraitRefKind::TraitImpl(trait_impl_ref) => {
                if let Some(timpl) = self.ctx.translated.trait_impls.get(trait_impl_ref.id)
                    && let Ok(target_pred) = timpl
                        .impl_trait
                        .clone()
                        .try_substitute(&trait_impl_ref.generics)
                {
                    let pred = tref.trait_decl_ref.clone().erase();
                    self.match_trait_decl_refs(&pred, &target_pred)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn match_trait_refs(&mut self, a: &TraitRef, b: &TraitRef) -> Result<(), TypeError> {
        self.match_poly_trait_decl_refs(&a.trait_decl_ref, &b.trait_decl_ref)
    }

    fn match_generics(&mut self, a: &GenericArgs, b: &GenericArgs) -> Result<(), TypeError> {
        for (a, b) in a.regions.iter().zip(b.regions.iter()) {
            self.match_regions(a, b)?;
        }
        for (a, b) in a.types.iter().zip(b.types.iter()) {
            self.match_tys(a, b)?;
        }
        for (a, b) in a.trait_refs.iter().zip(b.trait_refs.iter()) {
            self.match_trait_refs(a, b)?;
        }
        Ok(())
    }
}

impl TypeCheckVisitor<'_> {
    fn check_concretization_ty_match(&self, src_ty: &Ty, tar_ty: &Ty) {
        match (src_ty.kind(), tar_ty.kind()) {
            (TyKind::Ref(.., src_kind), TyKind::Ref(.., tar_kind)) => {
                assert_eq!(src_kind, tar_kind);
            }
            (TyKind::RawPtr(.., src_kind), TyKind::RawPtr(.., tar_kind)) => {
                assert_eq!(src_kind, tar_kind);
            }
            (
                TyKind::Adt(TypeDeclRef { id: src_id, .. }),
                TyKind::Adt(TypeDeclRef { id: tar_id, .. }),
            ) => {
                assert_eq!(src_id, tar_id);
            }
            _ => {
                let fmt = &self.ctx.into_fmt();
                self.error(format!(
                    "Invalid concretization targets: from \"{}\" to \"{}\"",
                    src_ty.with_ctx(fmt),
                    tar_ty.with_ctx(fmt)
                ));
            }
        }
    }

    /// For pretty error printing. This can print values that we encounter because we track binders
    /// properly. This doesn't have the right binders to print values we get from somewhere else
    /// (namely the `GenericParam`s we get from elsewhere in the crate).
    fn val_fmt_ctx(&self) -> FmtCtx<'_> {
        let mut fmt = self.ctx.into_fmt();
        fmt.generics = self.binder_stack.map_ref(Cow::Borrowed);
        fmt
    }

    fn zip_assert_match<'a, I, A, B, FmtA>(
        &'a mut self,
        a: &IndexMap<I, A>,
        b: &IndexMap<I, B>,
        a_fmt: &FmtA,
        kind: &str,
        target: &GenericsSource,
        check_inner: impl Fn(&mut Self, &A, &B),
    ) -> ControlFlow<()>
    where
        I: Idx,
        FmtA: AstFormatter,
        A: FmtWithCtx<FmtA>,
        B: FmtWithCtx<FmtCtx<'a>>,
    {
        if a.elem_count() == b.elem_count() {
            a.iter()
                .zip(b.iter())
                .for_each(|(x, y)| check_inner(self, x, y));
            ControlFlow::Continue(())
        } else {
            let b_fmt = &self.val_fmt_ctx();
            let a = a.iter().map(|x| x.with_ctx(a_fmt)).join(", ");
            let b = b.iter().map(|x| x.with_ctx(b_fmt)).join(", ");
            let target = target.with_ctx(a_fmt);
            self.error(format!(
                "Mismatched {kind}:\
                \ntarget: {target}\
                \nexpected: [{a}]\
                \n     got: [{b}]"
            ));
            ControlFlow::Break(())
        }
    }

    fn assert_clause_matches(
        &mut self,
        _params_fmt: &FmtCtx<'_>,
        tclause: Substituted<'_, TraitParam>,
        tref: &TraitRef,
    ) {
        if let Ok(clause) = tclause.try_substitute() {
            if self
                .match_poly_trait_decl_refs(&clause.trait_, &tref.trait_decl_ref)
                .is_err()
            {
                let args_fmt = &self.val_fmt_ctx();
                let clause = clause.with_ctx(args_fmt);
                let tref_pred = tref.trait_decl_ref.with_ctx(args_fmt);
                let tref = tref.with_ctx(args_fmt);
                self.error(format!(
                    "Mismatched trait clause:\
                    \nexpected: {clause}\
                    \n     got: {tref}: {tref_pred}"
                ));
            } else if self.match_trait_ref_against_itself(tref).is_err() {
                let args_fmt = &self.val_fmt_ctx();
                let tref_pred = tref.trait_decl_ref.with_ctx(args_fmt);
                let tref = tref.with_ctx(args_fmt);
                self.error(format!(
                    "Incoherent trait reference:\
                    \n     got: {tref}: {tref_pred}"
                ));
            }
        }
    }

    fn assert_clauses_match(
        &mut self,
        params_fmt: &FmtCtx<'_>,
        clauses: Substituted<'_, IndexMap<TraitClauseId, TraitParam>>,
        trefs: &IndexMap<TraitClauseId, TraitRef>,
        kind: &str,
        target: &GenericsSource,
    ) {
        let _ = self.zip_assert_match(
            &clauses.val,
            trefs,
            params_fmt,
            kind,
            target,
            |this, tclause, tref| {
                this.assert_clause_matches(&params_fmt, clauses.rebind(tclause), tref)
            },
        );
    }

    fn assert_matches(
        &mut self,
        params_fmt: &FmtCtx<'_>,
        params: &GenericParams,
        args: &mut GenericArgs,
        target: &GenericsSource,
    ) -> ControlFlow<()> {
        self.zip_assert_match(
            &params.regions,
            &args.regions,
            params_fmt,
            "regions",
            target,
            |_, _, _| {},
        )?;
        self.zip_assert_match(
            &params.types,
            &args.types,
            params_fmt,
            "type generics",
            target,
            |_, _, _| {},
        )?;
        self.zip_assert_match(
            &params.const_generics,
            &args.const_generics,
            params_fmt,
            "const generics",
            target,
            |_, _, _| {},
        )?;
        self.assert_clauses_match(
            params_fmt,
            Substituted::new(&params.trait_clauses, args),
            &args.trait_refs,
            "trait clauses",
            target,
        );
        ControlFlow::Continue(())
    }

    fn assert_matches_item(&mut self, id: impl Into<ItemId>, args: &mut GenericArgs) {
        let id = id.into();
        let Some(item) = self.ctx.translated.get_item(id) else {
            return;
        };
        let params = item.generic_params();
        let fmt1 = self.ctx.into_fmt();
        let fmt = fmt1.push_binder(Cow::Borrowed(params));
        let _ = self.assert_matches(&fmt, params, args, &GenericsSource::item(id));
    }

    fn assert_matches_method(
        &mut self,
        trait_ref: &TraitRef,
        method_name: TraitItemName,
        args: &mut GenericArgs,
    ) {
        let trait_id = trait_ref.trait_decl_ref.skip_binder.id;
        let target = &GenericsSource::Method(trait_id, method_name);
        let Some(trait_decl) = self.ctx.translated.trait_decls.get(trait_id) else {
            return;
        };
        let Some(bound_fn) = trait_decl.methods().find(|m| m.name() == method_name) else {
            return;
        };
        if let Ok(bound_fn) = Substituted::new_for_trait_ref(bound_fn, trait_ref).try_substitute() {
            let fmt1 = self.ctx.into_fmt();
            let fmt2 = fmt1.push_binder(Cow::Borrowed(&trait_decl.generics));
            let fmt = fmt2.push_binder(Cow::Borrowed(&bound_fn.params));
            let _ = self.assert_matches(&fmt, &bound_fn.params, args, target);
        }
    }
}

impl VisitAstMut for TypeCheckVisitor<'_> {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        self.visit_stack.push(x.name());
        VisitWithSpan::new(VisitWithBinderStack::new(self)).visit(x)?;
        self.visit_stack.pop();
        Continue(())
    }

    // Check that generics are correctly bound.
    fn enter_region(&mut self, x: &mut Region) {
        if let Region::Var(var) = x {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect region var: {var}"));
            }
        }
    }
    fn enter_ty_kind(&mut self, x: &mut TyKind) {
        if let TyKind::TypeVar(var) = x {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect type var: {var}"));
            }
        }
    }
    fn enter_constant_expr(&mut self, x: &mut ConstantExpr) {
        if let ConstantExprKind::Var(var) = &x.kind {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect const-generic var: {var}"));
            }
        }
    }
    fn enter_trait_ref(&mut self, x: &mut TraitRef) {
        match &x.kind {
            TraitRefKind::Clause(var) => {
                if self.binder_stack.get_var(*var).is_none() {
                    self.error(format!("Found incorrect clause var: {var}"));
                }
            }
            TraitRefKind::BuiltinOrAuto {
                parent_trait_refs,
                types,
                ..
            } => {
                let trait_id = x.trait_decl_ref.skip_binder.id;
                let target = GenericsSource::item(trait_id);
                let Some(tdecl) = self.ctx.translated.trait_decls.get(trait_id) else {
                    return;
                };
                if tdecl
                    .item_meta
                    .lang_item
                    .as_deref()
                    .is_some_and(|s| matches!(s, "pointee_trait" | "discriminant_kind"))
                {
                    // These traits have builtin assoc types that we can't resolve.
                    return;
                }

                let substituted_clauses = Substituted::new_for_trait_ref(&tdecl.implied_clauses, x);
                let fmt = &self.ctx.into_fmt();
                self.assert_clauses_match(
                    fmt,
                    substituted_clauses,
                    parent_trait_refs,
                    "builtin trait parent clauses",
                    &target,
                );

                let types_match = types.len() == tdecl.types.len()
                    && tdecl
                        .types
                        .iter()
                        .zip(types.iter())
                        .all(|(dty, (iname, _))| dty.name() == iname);
                if !types_match {
                    let args_fmt = &self.val_fmt_ctx();
                    let target = target.with_ctx(args_fmt);
                    let a = tdecl.types.iter().map(|t| t.name()).format(", ");
                    let b = types
                        .iter()
                        .map(|(_, assoc_ty)| assoc_ty.value.with_ctx(args_fmt))
                        .format(", ");
                    self.error(format!(
                        "Mismatched types in builtin trait ref:\
                        \ntarget: {target}\
                        \nexpected: [{a}]\
                        \n     got: [{b}]"
                    ));
                }
            }
            _ => {}
        }
    }

    // Check that generics match the parameters of the target item.
    fn enter_type_decl_ref(&mut self, x: &mut TypeDeclRef) {
        match x.id {
            TypeId::Adt(id) => self.assert_matches_item(id, &mut x.generics),
            // TODO: check builtin generics.
            TypeId::Tuple => {}
            TypeId::Builtin(_) => {}
        }
    }
    fn enter_fun_decl_ref(&mut self, x: &mut FunDeclRef) {
        self.assert_matches_item(x.id, &mut x.generics);
    }
    fn enter_fn_ptr(&mut self, x: &mut FnPtr) {
        match x.kind.as_ref() {
            FnPtrKind::Fun(FunId::Regular(id)) => self.assert_matches_item(*id, &mut x.generics),
            // TODO: check builtin generics.
            FnPtrKind::Fun(FunId::Builtin(_)) => {}
            FnPtrKind::Trait(trait_ref, method_name, _) => {
                self.assert_matches_method(trait_ref, *method_name, &mut x.generics);
            }
        }
    }
    fn visit_rvalue(&mut self, x: &mut Rvalue) -> ::std::ops::ControlFlow<Self::Break> {
        match x {
            Rvalue::UnaryOp(UnOp::Cast(CastKind::Concretize(src, tar)), _) => {
                self.check_concretization_ty_match(src, tar);
            }
            _ => {}
        }
        Continue(())
    }
    fn visit_body(&mut self, body: &mut Body) -> ControlFlow<Self::Break> {
        self.body_lt_unifier = match body {
            Body::Unstructured(GExprBody {
                bound_body_regions, ..
            })
            | Body::Structured(GExprBody {
                bound_body_regions, ..
            }) => Some(UnionFind::new(*bound_body_regions)),
            _ => None,
        };

        self.visit_inner(body)?;

        if let Some(mut unifier) = self.body_lt_unifier.take() {
            body.dyn_visit_mut(|r: &mut Region| {
                if let Region::Body(body_id) = r {
                    *body_id = unifier.find_mut(*body_id);
                }
            });
        }
        Continue(())
    }
    fn enter_global_decl_ref(&mut self, x: &mut GlobalDeclRef) {
        self.assert_matches_item(x.id, &mut x.generics);
    }
    fn enter_trait_decl_ref(&mut self, x: &mut TraitDeclRef) {
        // TODO: don't we need to pass the trait_self for correctness here?
        self.assert_matches_item(x.id, &mut x.generics);
    }
    fn enter_trait_impl_ref(&mut self, x: &mut TraitImplRef) {
        self.assert_matches_item(x.id, &mut x.generics);
    }
    fn enter_trait_impl(&mut self, timpl: &mut TraitImpl) {
        let Some(tdecl) = self.ctx.translated.trait_decls.get(timpl.impl_trait.id) else {
            return;
        };

        let fmt1 = self.ctx.into_fmt();
        let tdecl_fmt = fmt1.push_binder(Cow::Borrowed(&tdecl.generics));
        let impl_tref_kind = TraitRefKind::TraitImpl(TraitImplRef {
            id: timpl.def_id,
            generics: Box::new(timpl.generics.identity_args()),
        });
        let implied_clauses = Substituted::new_for_trait(
            &tdecl.implied_clauses,
            &timpl.impl_trait.generics,
            &impl_tref_kind,
        );
        self.assert_clauses_match(
            &tdecl_fmt,
            implied_clauses,
            &timpl.implied_trait_refs,
            "trait parent clauses",
            &GenericsSource::item(timpl.impl_trait.id),
        );
        // TODO: check type clauses
        let types_match = timpl.types.len() == tdecl.types.len()
            && tdecl
                .types
                .iter()
                .zip(timpl.types.iter())
                .all(|(dty, (iname, _))| dty.name() == iname);
        if !types_match {
            self.error(
                "The associated types supplied by the trait impl don't match the trait decl.",
            )
        }
        let consts_match = timpl.consts.len() == tdecl.consts.len()
            && tdecl
                .consts
                .iter()
                .zip(timpl.consts.iter())
                .all(|(dconst, (iname, _))| &dconst.name == iname);
        if !consts_match {
            self.error(
                "The associated consts supplied by the trait impl don't match the trait decl.",
            )
        }
        let methods_match = timpl.methods.len() == tdecl.methods.len();
        if !methods_match && self.phase != Check::PostTranslation {
            let decl_methods = tdecl
                .methods()
                .map(|m| format!("- {}", m.name()))
                .join("\n");
            let impl_methods = timpl
                .methods()
                .map(|(name, _)| format!("- {name}"))
                .join("\n");
            self.error(format!(
                "The methods supplied by the trait impl don't match the trait decl.\n\
                Trait methods:\n{decl_methods}\n\
                Impl methods:\n{impl_methods}"
            ))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Check {
    /// Check that generics match and unify body lifetimes on a best-effort basis.
    PostTranslation,
    /// Check that generics match.
    PostTransformation,
}

impl Check {
    fn name(&self) -> &'static str {
        match self {
            Check::PostTranslation => "after translation",
            Check::PostTransformation => "after transformations",
        }
    }
}

impl TransformPass for Check {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_item_mut(|ctx, mut item| {
            let mut visitor = TypeCheckVisitor {
                ctx,
                phase: *self,
                span: Span::dummy(),
                binder_stack: BindingStack::empty(),
                visit_stack: Default::default(),
                body_lt_unifier: None,
            };
            let _ = item.drive_mut(&mut visitor);
        });
    }
}
