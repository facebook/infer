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
    /// Whether the pass should run.
    fn should_run(&self, _options: &TranslateOptions) -> bool {
        true
    }

    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut ullbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Some(body) = decl.body.as_unstructured_mut() {
            self.transform_body(ctx, body)
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            self.log_before_body(ctx, &decl.item_meta.name, &decl.body);
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(&self, ctx: &TransformCtx, name: &Name, body: &Body) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = body.to_string_with_ctx(fmt_ctx);
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
    /// Whether the pass should run.
    fn should_run(&self, _options: &TranslateOptions) -> bool {
        true
    }

    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut llbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Some(body) = decl.body.as_structured_mut() {
            self.transform_body(ctx, body)
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            self.log_before_body(ctx, &decl.item_meta.name, &decl.body);
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(&self, ctx: &TransformCtx, name: &Name, body: &Body) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = body.to_string_with_ctx(fmt_ctx);
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
    /// Whether the pass should run.
    fn should_run(&self, _options: &TranslateOptions) -> bool {
        true
    }

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
        def_id: impl Into<ItemId>,
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
                if decl.body.has_contents() {
                    let mut body = mem::replace(&mut decl.body, Body::Opaque);
                    let fun_decl_id = decl.def_id;
                    let is_local = decl.item_meta.is_local;
                    self.with_def_id(fun_decl_id, is_local, |ctx| f(ctx, &mut body));
                    self.translated.fun_decls[id].body = body;
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
    /// each item before iterating over it. Items added during traversal will not be iterated over.
    pub fn for_each_item_mut(&mut self, mut f: impl for<'a> FnMut(&'a mut Self, ItemRefMut<'a>)) {
        for id in self.translated.all_ids() {
            if let Some(mut decl) = self.translated.remove_item(id) {
                f(self, decl.as_mut());
                self.translated.set_item_slot(id, decl);
            }
        }
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

/// A helper trait that captures common operations in body transformation.
pub trait BodyTransformCtx: Sized {
    fn get_crate(&self) -> &TranslatedCrate;
    fn get_options(&self) -> &TranslateOptions;
    fn get_params(&self) -> &GenericParams;
    fn get_locals_mut(&mut self) -> &mut Locals;

    fn insert_storage_live_stmt(&mut self, local: LocalId);
    fn insert_storage_dead_stmt(&mut self, local: LocalId);
    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue);

    fn into_fmt(&self) -> FmtCtx<'_> {
        self.get_crate().into_fmt()
    }

    /// Create a local & return the place pointing to it
    fn fresh_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let var = self.get_locals_mut().new_var(name, ty);
        self.insert_storage_live_stmt(var.local_id().unwrap());
        var
    }

    /// Assign an rvalue to a place, unless the rvalue is a move in which case we just use the
    /// moved place.
    fn rval_to_place(&mut self, rvalue: Rvalue, ty: Ty) -> Place {
        if let Rvalue::Use(Operand::Move(place)) = rvalue {
            place
        } else {
            let var = self.fresh_var(None, ty);
            self.insert_assn_stmt(var.clone(), rvalue);
            var
        }
    }

    /// When `from_end` is true, we need to compute `len(p) - last_arg` instead of just using `last_arg`.
    /// Otherwise, we simply return `last_arg`.
    /// New local variables are created as needed.
    ///
    /// The `last_arg` is either the `offset` for `Index` or the `to` for `Subslice` for the projections.
    fn compute_subslice_end_idx(
        &mut self,
        len_place: &Place,
        last_arg: Operand,
        from_end: bool,
    ) -> Operand {
        if from_end {
            // `storage_live(len_var)`
            // `len_var = len(p)`
            let len_var = self.fresh_var(None, Ty::mk_usize());
            let len = match len_place.ty().kind() {
                TyKind::Array(_, len) => Some(len.clone()),
                TyKind::Slice(_) => None,
                _ => panic!(
                    "called `compute_subslice_end_idx` on something that isn't an array or slice: {:?}",
                    len_place.ty()
                ),
            };
            self.insert_assn_stmt(
                len_var.clone(),
                Rvalue::Len(len_place.clone(), len_place.ty().clone(), len),
            );

            // `storage_live(index_var)`
            // `index_var = len_var - last_arg`
            // `storage_dead(len_var)`
            let index_var = self.fresh_var(None, Ty::mk_usize());
            self.insert_assn_stmt(
                index_var.clone(),
                Rvalue::BinaryOp(
                    BinOp::Sub(OverflowMode::UB),
                    Operand::Copy(len_var.clone()),
                    last_arg,
                ),
            );
            self.insert_storage_dead_stmt(len_var.local_id().unwrap());
            Operand::Copy(index_var)
        } else {
            last_arg
        }
    }

    fn is_sized_type_var(&mut self, ty: &Ty) -> bool {
        match ty.kind() {
            TyKind::TypeVar(..) => {
                if self.get_options().hide_marker_traits {
                    // If we're hiding `Sized`, let's consider everything to be sized.
                    return true;
                }
                let params = self.get_params();
                for clause in &params.trait_clauses {
                    let tref = clause.trait_.clone().erase();
                    // Check if it is `Sized<T>`
                    if tref.generics.types[0] == *ty
                        && self
                            .get_crate()
                            .trait_decls
                            .get(tref.id)
                            .and_then(|decl| decl.item_meta.lang_item.clone())
                            == Some("sized".into())
                    {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Emit statements that compute the metadata of the given place. Returns an operand containing the
    /// metadata value.
    ///
    /// E.g., for:
    /// ```ignore
    /// let x = &(*ptr).field;
    /// ```
    /// if `(*ptr).field` is a DST like `[i32]`, this will get the metadata from the appropriate
    /// pointer:
    /// ```ignore
    /// let len = ptr.metadata;
    /// ```
    /// and return `Operand::Move(len)`.
    ///
    fn compute_place_metadata(&mut self, place: &Place) -> Operand {
        /// No metadata. We use the `unit_metadata` global to avoid having to define unit locals
        /// everywhere.
        fn no_metadata<T: BodyTransformCtx>(ctx: &T) -> Operand {
            let unit_meta = ctx.get_crate().unit_metadata.clone().unwrap();
            Operand::Copy(Place::new_global(unit_meta, Ty::mk_unit()))
        }

        /// Compute the metadata for a place. Return `None` if the place has no metadata.
        fn compute_place_metadata_inner<T: BodyTransformCtx>(
            ctx: &mut T,
            place: &Place,
            metadata_ty: &Ty,
        ) -> Option<Operand> {
            let (subplace, proj) = place.as_projection()?;
            match proj {
                // The outermost deref we encountered gives us the metadata of the place.
                ProjectionElem::Deref => {
                    let metadata_place = subplace
                        .clone()
                        .project(ProjectionElem::PtrMetadata, metadata_ty.clone());
                    Some(Operand::Copy(metadata_place))
                }
                ProjectionElem::Field { .. } => {
                    compute_place_metadata_inner(ctx, subplace, metadata_ty)
                }
                // Indexing for array & slice will only result in sized types, hence no metadata
                ProjectionElem::Index { .. } => None,
                // Ptr metadata is always sized.
                ProjectionElem::PtrMetadata { .. } => None,
                // Subslice must have metadata length, compute the metadata here as `to` - `from`
                ProjectionElem::Subslice { from, to, from_end } => {
                    let to_idx = ctx.compute_subslice_end_idx(subplace, *to.clone(), *from_end);
                    let diff_place = ctx.fresh_var(None, Ty::mk_usize());
                    ctx.insert_assn_stmt(
                        diff_place.clone(),
                        // Overflow is UB and should have been prevented by a bound check beforehand.
                        Rvalue::BinaryOp(BinOp::Sub(OverflowMode::UB), to_idx, *from.clone()),
                    );
                    Some(Operand::Copy(diff_place))
                }
            }
        }
        trace!(
            "getting ptr metadata for place: {}",
            place.with_ctx(&self.into_fmt())
        );
        let metadata_ty = place.ty().get_ptr_metadata(&self.get_crate()).into_type();
        if metadata_ty.is_unit()
            || matches!(metadata_ty.kind(), TyKind::PtrMetadata(ty) if self.is_sized_type_var(ty))
        {
            // If the type var is known to be `Sized`, then no metadata is needed
            return no_metadata(self);
        }
        trace!(
            "computed metadata type: {}",
            metadata_ty.with_ctx(&self.into_fmt())
        );
        compute_place_metadata_inner(self, place, &metadata_ty).unwrap_or_else(|| no_metadata(self))
    }

    /// Create a `&` borrow of the place.
    fn borrow(&mut self, place: Place, kind: BorrowKind) -> Rvalue {
        let ptr_metadata = self.compute_place_metadata(&place);
        Rvalue::Ref {
            place,
            kind,
            ptr_metadata,
        }
    }
    /// Create a `&raw` borrow of the place.
    fn raw_borrow(&mut self, place: Place, kind: RefKind) -> Rvalue {
        let ptr_metadata = self.compute_place_metadata(&place);
        Rvalue::RawPtr {
            place,
            kind,
            ptr_metadata,
        }
    }

    /// Store a `&` borrow of the place into a new place.
    fn borrow_to_new_var(&mut self, place: Place, kind: BorrowKind, name: Option<String>) -> Place {
        let ref_ty = TyKind::Ref(Region::Erased, place.ty().clone(), kind.into()).into_ty();
        let target_place = self.fresh_var(name, ref_ty);
        let rvalue = self.borrow(place, kind);
        self.insert_assn_stmt(target_place.clone(), rvalue);
        target_place
    }
    /// Store a `&raw` borrow of the place into a new place.
    fn raw_borrow_to_new_var(
        &mut self,
        place: Place,
        kind: RefKind,
        name: Option<String>,
    ) -> Place {
        let ref_ty = TyKind::RawPtr(place.ty().clone(), kind).into_ty();
        let target_place = self.fresh_var(name, ref_ty);
        let rvalue = self.raw_borrow(place, kind);
        self.insert_assn_stmt(target_place.clone(), rvalue);
        target_place
    }
}

pub struct UllbcStatementTransformCtx<'a> {
    pub ctx: &'a mut TransformCtx,
    pub params: &'a GenericParams,
    pub locals: &'a mut Locals,
    /// Span of the statement being explored
    pub span: Span,
    /// Statements to prepend to the statement currently being explored.
    pub statements: Vec<ullbc_ast::Statement>,
}

impl BodyTransformCtx for UllbcStatementTransformCtx<'_> {
    fn get_crate(&self) -> &TranslatedCrate {
        &self.ctx.translated
    }
    fn get_options(&self) -> &TranslateOptions {
        &self.ctx.options
    }
    fn get_params(&self) -> &GenericParams {
        self.params
    }
    fn get_locals_mut(&mut self) -> &mut Locals {
        self.locals
    }

    fn insert_storage_live_stmt(&mut self, local: LocalId) {
        self.statements.push(ullbc_ast::Statement::new(
            self.span,
            ullbc_ast::StatementKind::StorageLive(local),
        ));
    }

    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue) {
        self.statements.push(ullbc_ast::Statement::new(
            self.span,
            ullbc_ast::StatementKind::Assign(place, rvalue),
        ));
    }

    fn insert_storage_dead_stmt(&mut self, local: LocalId) {
        self.statements.push(ullbc_ast::Statement::new(
            self.span,
            ullbc_ast::StatementKind::StorageDead(local),
        ));
    }
}

pub struct LlbcStatementTransformCtx<'a> {
    pub ctx: &'a mut TransformCtx,
    pub params: &'a GenericParams,
    pub locals: &'a mut Locals,
    /// Span of the statement being explored
    pub span: Span,
    /// Statements to prepend to the statement currently being explored.
    pub statements: Vec<llbc_ast::Statement>,
}

impl BodyTransformCtx for LlbcStatementTransformCtx<'_> {
    fn get_crate(&self) -> &TranslatedCrate {
        &self.ctx.translated
    }
    fn get_options(&self) -> &TranslateOptions {
        &self.ctx.options
    }
    fn get_params(&self) -> &GenericParams {
        self.params
    }
    fn get_locals_mut(&mut self) -> &mut Locals {
        self.locals
    }

    fn insert_storage_live_stmt(&mut self, local: LocalId) {
        self.statements.push(llbc_ast::Statement::new(
            self.span,
            llbc_ast::StatementKind::StorageLive(local),
        ));
    }

    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue) {
        self.statements.push(llbc_ast::Statement::new(
            self.span,
            llbc_ast::StatementKind::Assign(place, rvalue),
        ));
    }

    fn insert_storage_dead_stmt(&mut self, local: LocalId) {
        self.statements.push(llbc_ast::Statement::new(
            self.span,
            llbc_ast::StatementKind::StorageDead(local),
        ));
    }
}

impl FunDecl {
    pub fn transform_ullbc_statements(
        &mut self,
        ctx: &mut TransformCtx,
        mut f: impl FnMut(&mut UllbcStatementTransformCtx, &mut ullbc_ast::Statement),
    ) {
        if let Some(body) = self.body.as_unstructured_mut() {
            let mut ctx = UllbcStatementTransformCtx {
                ctx,
                params: &self.generics,
                locals: &mut body.locals,
                span: self.item_meta.span,
                statements: Vec::new(),
            };
            body.body.iter_mut().for_each(|block| {
                ctx.statements = Vec::with_capacity(block.statements.len());
                for mut st in mem::take(&mut block.statements) {
                    ctx.span = st.span;
                    f(&mut ctx, &mut st);
                    ctx.statements.push(st);
                }
                block.statements = mem::take(&mut ctx.statements);
            });
        }
    }

    pub fn transform_ullbc_terminators(
        &mut self,
        ctx: &mut TransformCtx,
        mut f: impl FnMut(&mut UllbcStatementTransformCtx, &mut ullbc_ast::Terminator),
    ) {
        if let Some(body) = self.body.as_unstructured_mut() {
            let mut ctx = UllbcStatementTransformCtx {
                ctx,
                params: &self.generics,
                locals: &mut body.locals,
                span: self.item_meta.span,
                statements: Vec::new(),
            };
            body.body.iter_mut().for_each(|block| {
                ctx.span = block.terminator.span;
                ctx.statements = mem::take(&mut block.statements);
                f(&mut ctx, &mut block.terminator);
                block.statements = mem::take(&mut ctx.statements);
            });
        }
    }

    pub fn transform_ullbc_operands(
        &mut self,
        ctx: &mut TransformCtx,
        mut f: impl FnMut(&mut UllbcStatementTransformCtx, &mut Operand),
    ) {
        self.transform_ullbc_statements(ctx, |ctx, st| {
            st.kind.dyn_visit_in_body_mut(|op: &mut Operand| f(ctx, op));
        });
        self.transform_ullbc_terminators(ctx, |ctx, st| {
            st.kind.dyn_visit_in_body_mut(|op: &mut Operand| f(ctx, op));
        });
    }

    pub fn transform_llbc_statements(
        &mut self,
        ctx: &mut TransformCtx,
        mut f: impl FnMut(&mut LlbcStatementTransformCtx, &mut llbc_ast::Statement),
    ) {
        if let Some(body) = self.body.as_structured_mut() {
            let mut ctx = LlbcStatementTransformCtx {
                ctx,
                locals: &mut body.locals,
                statements: Vec::new(),
                span: self.item_meta.span,
                params: &self.generics,
            };
            body.body.visit_blocks_bwd(|block: &mut llbc_ast::Block| {
                ctx.statements = Vec::with_capacity(block.statements.len());
                for mut st in mem::take(&mut block.statements) {
                    ctx.span = st.span;
                    f(&mut ctx, &mut st);
                    ctx.statements.push(st);
                }
                block.statements = mem::take(&mut ctx.statements)
            })
        }
    }
}
