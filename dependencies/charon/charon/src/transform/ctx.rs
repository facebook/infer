use crate::ast::*;
use crate::errors::{ErrorCtx, Level};
use crate::formatter::{FmtCtx, IntoFormatter};
use crate::llbc_ast;
use crate::options::TranslateOptions;
use crate::pretty::FmtWithCtx;
use crate::ullbc_ast;
use std::cell::RefCell;
use std::{fmt, mem};

/// Simpler context used for rustc-independent code transformation. This only depends on rustc for
/// its error reporting machinery.
pub struct TransformCtx {
    /// The options that control transformation.
    pub options: TranslateOptions,
    /// The translated data.
    pub translated: TranslatedCrate,
    /// Context for tracking and reporting errors.
    pub errors: RefCell<ErrorCtx>,
}

/// A pass that modifies ullbc bodies.
pub trait UllbcPass: Sync {
    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut ullbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Ok(body) = &mut decl.body {
            self.transform_body(ctx, body.as_unstructured_mut().unwrap())
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            let body = decl
                .body
                .as_mut()
                .map(|body| body.as_unstructured_mut().unwrap())
                .map_err(|opaque| *opaque);
            self.log_before_body(ctx, &decl.item_meta.name, body.as_deref());
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(
        &self,
        ctx: &TransformCtx,
        name: &Name,
        body: Result<&ullbc_ast::ExprBody, &Opaque>,
    ) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = if let Ok(body) = body {
            body.to_string_with_ctx(fmt_ctx)
        } else {
            "<opaque>".to_owned()
        };
        trace!(
            "# About to run pass [{}] on `{}`:\n{}",
            self.name(),
            name.with_ctx(fmt_ctx),
            body_str,
        );
    }
}

/// A pass that modifies llbc bodies.
pub trait LlbcPass: Sync {
    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut llbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Ok(body) = &mut decl.body {
            self.transform_body(ctx, body.as_structured_mut().unwrap())
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            let body = decl
                .body
                .as_mut()
                .map(|body| body.as_structured_mut().unwrap())
                .map_err(|opaque| *opaque);
            self.log_before_body(ctx, &decl.item_meta.name, body.as_deref());
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(
        &self,
        ctx: &TransformCtx,
        name: &Name,
        body: Result<&llbc_ast::ExprBody, &Opaque>,
    ) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = if let Ok(body) = body {
            body.to_string_with_ctx(fmt_ctx)
        } else {
            "<opaque>".to_owned()
        };
        trace!(
            "# About to run pass [{}] on `{}`:\n{}",
            self.name(),
            name.with_ctx(fmt_ctx),
            body_str,
        );
    }
}

/// A pass that transforms the crate data.
pub trait TransformPass: Sync {
    fn transform_ctx(&self, ctx: &mut TransformCtx);

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }
}

impl<'ctx> TransformCtx {
    pub(crate) fn has_errors(&self) -> bool {
        self.errors.borrow().has_errors()
    }

    /// Span an error and register the error.
    pub(crate) fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.errors
            .borrow_mut()
            .span_err(&self.translated, span, msg, level)
    }

    pub(crate) fn opacity_for_name(&self, name: &Name) -> ItemOpacity {
        self.options.opacity_for_name(&self.translated, name)
    }

    pub(crate) fn with_def_id<F, T>(
        &mut self,
        def_id: impl Into<AnyTransId>,
        def_id_is_local: bool,
        f: F,
    ) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let mut errors = self.errors.borrow_mut();
        let current_def_id = mem::replace(&mut errors.def_id, Some(def_id.into()));
        let current_def_id_is_local = mem::replace(&mut errors.def_id_is_local, def_id_is_local);
        drop(errors); // important: release the refcell "lock"
        let ret = f(self);
        let mut errors = self.errors.borrow_mut();
        errors.def_id = current_def_id;
        errors.def_id_is_local = current_def_id_is_local;
        ret
    }

    /// Mutably iterate over the bodies.
    /// Warning: we replace each body with `Err(Opaque)` while inspecting it so we can keep access
    /// to the rest of the crate.
    pub(crate) fn for_each_body(&mut self, mut f: impl FnMut(&mut Self, &mut Body)) {
        let fn_ids = self.translated.fun_decls.all_indices();
        for id in fn_ids {
            if let Some(decl) = self.translated.fun_decls.get_mut(id) {
                if let Ok(mut body) = mem::replace(&mut decl.body, Err(Opaque)) {
                    let fun_decl_id = decl.def_id;
                    let is_local = decl.item_meta.is_local;
                    self.with_def_id(fun_decl_id, is_local, |ctx| f(ctx, &mut body));
                    self.translated.fun_decls[id].body = Ok(body);
                }
            }
        }
    }

    /// Mutably iterate over the function declarations.
    /// Warning: each inspected fundecl becomes inaccessible from `ctx` during the course of this function.
    pub(crate) fn for_each_fun_decl(&mut self, mut f: impl FnMut(&mut Self, &mut FunDecl)) {
        let fn_ids = self.translated.fun_decls.all_indices();
        for id in fn_ids {
            if let Some(mut decl) = self.translated.fun_decls.remove(id) {
                let fun_decl_id = decl.def_id;
                let is_local = decl.item_meta.is_local;
                self.with_def_id(fun_decl_id, is_local, |ctx| f(ctx, &mut decl));
                self.translated.fun_decls.set_slot(id, decl);
            }
        }
    }

    /// Iterate mutably over all items, keeping access to `self`. To make this work, we move out
    /// each item before iterating over it.
    pub fn for_each_item_mut(
        &mut self,
        mut f: impl for<'a> FnMut(&'a mut Self, AnyTransItemMut<'a>),
    ) {
        macro_rules! for_each {
            ($vector:ident, $kind:ident) => {
                for id in self.translated.$vector.all_indices() {
                    if let Some(mut decl) = self.translated.$vector.remove(id) {
                        f(self, AnyTransItemMut::$kind(&mut decl));
                        self.translated.$vector.set_slot(id, decl);
                    }
                }
            };
        }
        for_each!(type_decls, Type);
        for_each!(fun_decls, Fun);
        for_each!(global_decls, Global);
        for_each!(trait_decls, TraitDecl);
        for_each!(trait_impls, TraitImpl);
    }
}

impl<'a> IntoFormatter for &'a TransformCtx {
    type C = FmtCtx<'a>;

    fn into_fmt(self) -> Self::C {
        self.translated.into_fmt()
    }
}

impl fmt::Display for TransformCtx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.translated.fmt(f)
    }
}
