//! Check that all supplied generic types match the corresponding generic parameters.
use derive_generic_visitor::*;
use index_vec::Idx;
use itertools::Itertools;
use std::{borrow::Cow, fmt::Display};

use crate::{
    ast::*,
    errors::Level,
    formatter::{AstFormatter, FmtCtx, IntoFormatter},
    pretty::FmtWithCtx,
    transform::utils::GenericsSource,
};

use super::{TransformCtx, ctx::TransformPass};

#[derive(Visitor)]
struct CheckGenericsVisitor<'a> {
    ctx: &'a TransformCtx,
    phase: &'static str,
    /// Tracks an enclosing span for error reporting.
    span: Span,
    /// Track the binders seen so far.
    // We can't keep the params by reference because the visitors don't tell us that everything
    // we're visiting has lifetime `'a`.
    binder_stack: BindingStack<GenericParams>,
    /// Remember the names of the types visited up to here.
    visit_stack: Vec<&'static str>,
}

impl VisitorWithSpan for CheckGenericsVisitor<'_> {
    fn current_span(&mut self) -> &mut Span {
        &mut self.span
    }
}
impl VisitorWithBinderStack for CheckGenericsVisitor<'_> {
    fn binder_stack_mut(&mut self) -> &mut BindingStack<GenericParams> {
        &mut self.binder_stack
    }
}

impl CheckGenericsVisitor<'_> {
    fn error(&self, message: impl Display) {
        let msg = format!(
            "Found inconsistent generics {}:\n{message}\n\
            Visitor stack:\n  {}\n\
            Binding stack (depth {}):\n  {}",
            self.phase,
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

    /// For pretty error printing. This can print values that we encounter because we track binders
    /// properly. This doesn't have the right binders to print values we get from somewhere else
    /// (namely the `GenericParam`s we get from elsewhere in the crate).
    fn val_fmt_ctx(&self) -> FmtCtx<'_> {
        let mut fmt = self.ctx.into_fmt();
        fmt.generics = self.binder_stack.map_ref(Cow::Borrowed);
        fmt
    }

    fn zip_assert_match<I, A, B, FmtA, FmtB>(
        &self,
        a: &Vector<I, A>,
        b: &Vector<I, B>,
        a_fmt: &FmtA,
        b_fmt: &FmtB,
        kind: &str,
        target: &GenericsSource,
        check_inner: impl Fn(&A, &B),
    ) where
        I: Idx,
        FmtA: AstFormatter,
        A: FmtWithCtx<FmtA>,
        B: FmtWithCtx<FmtB>,
    {
        if a.elem_count() == b.elem_count() {
            a.iter().zip(b.iter()).for_each(|(x, y)| check_inner(x, y));
        } else {
            let a = a.iter().map(|x| x.with_ctx(a_fmt)).join(", ");
            let b = b.iter().map(|x| x.with_ctx(b_fmt)).join(", ");
            let target = target.with_ctx(a_fmt);
            self.error(format!(
                "Mismatched {kind}:\
                \ntarget: {target}\
                \nexpected: [{a}]\
                \n     got: [{b}]"
            ))
        }
    }

    fn assert_clause_matches(
        &self,
        params_fmt: &FmtCtx<'_>,
        tclause: &TraitClause,
        tref: &TraitRef,
    ) {
        let clause_trait_id = tclause.trait_.skip_binder.id;
        let ref_trait_id = tref.trait_decl_ref.skip_binder.id;
        if clause_trait_id != ref_trait_id {
            let args_fmt = &self.val_fmt_ctx();
            let tclause = tclause.with_ctx(params_fmt);
            let tref_pred = tref.trait_decl_ref.with_ctx(args_fmt);
            let tref = tref.with_ctx(args_fmt);
            self.error(format!(
                "Mismatched trait clause:\
                \nexpected: {tclause}\
                \n     got: {tref}: {tref_pred}"
            ));
        }
    }

    fn assert_matches(
        &self,
        params_fmt: &FmtCtx<'_>,
        params: &GenericParams,
        args: &GenericArgs,
        target: &GenericsSource,
    ) {
        let args_fmt = &self.val_fmt_ctx();
        self.zip_assert_match(
            &params.regions,
            &args.regions,
            params_fmt,
            args_fmt,
            "regions",
            target,
            |_, _| {},
        );
        self.zip_assert_match(
            &params.types,
            &args.types,
            params_fmt,
            args_fmt,
            "type generics",
            target,
            |_, _| {},
        );
        self.zip_assert_match(
            &params.const_generics,
            &args.const_generics,
            params_fmt,
            args_fmt,
            "const generics",
            target,
            |_, _| {},
        );
        self.zip_assert_match(
            &params.trait_clauses,
            &args.trait_refs,
            params_fmt,
            args_fmt,
            "trait clauses",
            target,
            |tclause, tref| self.assert_clause_matches(params_fmt, tclause, tref),
        );
    }

    fn assert_matches_item(&self, id: impl Into<AnyTransId>, args: &GenericArgs) {
        let id = id.into();
        let Some(item) = self.ctx.translated.get_item(id) else {
            return;
        };
        let params = item.generic_params();
        let fmt1 = self.ctx.into_fmt();
        let fmt = fmt1.push_binder(Cow::Borrowed(params));
        self.assert_matches(&fmt, params, args, &GenericsSource::item(id));
    }

    fn assert_matches_method(
        &self,
        trait_id: TraitDeclId,
        method_name: &TraitItemName,
        args: &GenericArgs,
    ) {
        let target = &GenericsSource::Method(trait_id, method_name.clone());
        let Some(trait_decl) = self.ctx.translated.trait_decls.get(trait_id) else {
            return;
        };
        let Some((_, bound_fn)) = trait_decl.methods().find(|(n, _)| n == method_name) else {
            return;
        };
        let params = &bound_fn.params;
        let fmt1 = self.ctx.into_fmt();
        let fmt2 = fmt1.push_binder(Cow::Borrowed(&trait_decl.generics));
        let fmt = fmt2.push_binder(Cow::Borrowed(params));
        self.assert_matches(&fmt, params, args, target);
    }
}

impl VisitAst for CheckGenericsVisitor<'_> {
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &T) -> ControlFlow<Self::Break> {
        self.visit_stack.push(x.name());
        VisitWithSpan::new(VisitWithBinderStack::new(self)).visit(x)?;
        self.visit_stack.pop();
        Continue(())
    }

    // Check that generics are correctly bound.
    fn enter_region(&mut self, x: &Region) {
        if let Region::Var(var) = x {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect region var: {var}"));
            }
        }
    }
    fn enter_ty_kind(&mut self, x: &TyKind) {
        if let TyKind::TypeVar(var) = x {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect type var: {var}"));
            }
        }
    }
    fn enter_const_generic(&mut self, x: &ConstGeneric) {
        if let ConstGeneric::Var(var) = x {
            if self.binder_stack.get_var(*var).is_none() {
                self.error(format!("Found incorrect const-generic var: {var}"));
            }
        }
    }
    fn enter_trait_ref_kind(&mut self, x: &TraitRefKind) {
        match x {
            TraitRefKind::Clause(var) => {
                if self.binder_stack.get_var(*var).is_none() {
                    self.error(format!("Found incorrect clause var: {var}"));
                }
            }
            TraitRefKind::BuiltinOrAuto {
                trait_decl_ref,
                parent_trait_refs,
                types,
            } => {
                let trait_id = trait_decl_ref.skip_binder.id;
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
                let fmt = &self.ctx.into_fmt();
                let args_fmt = &self.val_fmt_ctx();
                self.zip_assert_match(
                    &tdecl.parent_clauses,
                    parent_trait_refs,
                    fmt,
                    args_fmt,
                    "builtin trait parent clauses",
                    &target,
                    |tclause, tref| self.assert_clause_matches(&fmt, tclause, tref),
                );
                let types_match = types.len() == tdecl.types.len()
                    && tdecl
                        .types
                        .iter()
                        .zip(types.iter())
                        .all(|(dname, (iname, _, _))| dname == iname);
                if !types_match {
                    let target = target.with_ctx(args_fmt);
                    let a = tdecl.types.iter().format(", ");
                    let b = types
                        .iter()
                        .map(|(_, ty, _)| ty.with_ctx(args_fmt))
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
    fn enter_type_decl_ref(&mut self, x: &TypeDeclRef) {
        match x.id {
            TypeId::Adt(id) => self.assert_matches_item(id, &x.generics),
            // TODO: check builtin generics.
            TypeId::Tuple => {}
            TypeId::Builtin(_) => {}
        }
    }
    fn enter_fun_decl_ref(&mut self, x: &FunDeclRef) {
        self.assert_matches_item(x.id, &x.generics);
    }
    fn enter_fn_ptr(&mut self, x: &FnPtr) {
        match x.func.as_ref() {
            FunIdOrTraitMethodRef::Fun(FunId::Regular(id)) => {
                self.assert_matches_item(*id, &x.generics)
            }
            // TODO: check builtin generics.
            FunIdOrTraitMethodRef::Fun(FunId::Builtin(_)) => {}
            FunIdOrTraitMethodRef::Trait(trait_ref, method_name, _) => {
                let trait_id = trait_ref.trait_decl_ref.skip_binder.id;
                self.assert_matches_method(trait_id, method_name, &x.generics);
            }
        }
    }
    fn enter_global_decl_ref(&mut self, x: &GlobalDeclRef) {
        self.assert_matches_item(x.id, &x.generics);
    }
    fn enter_trait_decl_ref(&mut self, x: &TraitDeclRef) {
        self.assert_matches_item(x.id, &x.generics);
    }
    fn enter_trait_impl_ref(&mut self, x: &TraitImplRef) {
        self.assert_matches_item(x.id, &x.generics);
    }
    fn enter_trait_impl(&mut self, timpl: &TraitImpl) {
        let Some(tdecl) = self.ctx.translated.trait_decls.get(timpl.impl_trait.id) else {
            return;
        };
        // See `lift_associated_item_clauses`
        assert!(timpl.type_clauses.is_empty());
        assert!(tdecl.type_clauses.is_empty());

        let fmt1 = self.ctx.into_fmt();
        let tdecl_fmt = fmt1.push_binder(Cow::Borrowed(&tdecl.generics));
        let args_fmt = &self.val_fmt_ctx();
        self.zip_assert_match(
            &tdecl.parent_clauses,
            &timpl.parent_trait_refs,
            &tdecl_fmt,
            args_fmt,
            "trait parent clauses",
            &GenericsSource::item(timpl.impl_trait.id),
            |tclause, tref| self.assert_clause_matches(&tdecl_fmt, tclause, tref),
        );
        let types_match = timpl.types.len() == tdecl.types.len()
            && tdecl
                .types
                .iter()
                .zip(timpl.types.iter())
                .all(|(dname, (iname, _))| dname == iname);
        if !types_match {
            self.error(
                "The associated types supplied by the trait impl don't match the trait decl.",
            )
        }
        let consts_match = timpl.consts.len() == tdecl.consts.len()
            && tdecl
                .types
                .iter()
                .zip(timpl.types.iter())
                .all(|(dname, (iname, _))| dname == iname);
        if !consts_match {
            self.error(
                "The associated consts supplied by the trait impl don't match the trait decl.",
            )
        }
        let methods_match = timpl.methods.len() == tdecl.methods.len();
        if !methods_match && self.phase != "after translation" {
            let decl_methods = tdecl
                .methods()
                .map(|(name, _)| format!("- {name}"))
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

// The argument is a name to disambiguate the two times we run this check.
pub struct Check(pub &'static str);
impl TransformPass for Check {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        for item in ctx.translated.all_items() {
            // Hack: the items generated by monomorphisation have incorrect generics.
            if item
                .item_meta()
                .name
                .name
                .last()
                .unwrap()
                .is_monomorphized()
            {
                continue;
            }
            let mut visitor = CheckGenericsVisitor {
                ctx,
                phase: self.0,
                span: Span::dummy(),
                binder_stack: BindingStack::empty(),
                visit_stack: Default::default(),
            };
            let _ = item.drive(&mut visitor);
        }
    }
}
