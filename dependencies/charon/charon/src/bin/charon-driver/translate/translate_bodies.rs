//! Translate functions from the rust compiler MIR to our internal representation.
//! Our internal representation is very close to MIR, but is more convenient for
//! us to handle, and easier to maintain - rustc's representation can evolve
//! independently.

use itertools::Itertools;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::ops::Deref;
use std::ops::DerefMut;
use std::panic;
use std::rc::Rc;

use rustc_middle::mir;
use rustc_middle::ty;

use super::translate_crate::*;
use super::translate_ctx::*;
use charon_lib::ast::ullbc_ast::StatementKind;
use charon_lib::ast::ullbc_ast_utils::BodyBuilder;
use charon_lib::ast::*;
use charon_lib::formatter::FmtCtx;
use charon_lib::formatter::IntoFormatter;
use charon_lib::ids::IndexMap;
use charon_lib::pretty::FmtWithCtx;
use charon_lib::ullbc_ast::*;

/// A translation context for function bodies.
pub(crate) struct BodyTransCtx<'tcx, 'tctx, 'ictx> {
    /// The translation context for the item.
    pub i_ctx: &'ictx mut ItemTransCtx<'tcx, 'tctx>,
    /// List of body locals.
    pub local_decls: &'ictx rustc_index::IndexVec<mir::Local, mir::LocalDecl<'tcx>>,

    /// What kind of drops we get in this body.
    pub drop_kind: DropKind,
    /// The (regular) variables in the current function body.
    pub locals: Locals,
    /// The map from rust variable indices to translated variables indices.
    pub locals_map: HashMap<usize, LocalId>,
    /// The translated blocks.
    pub blocks: IndexMap<BlockId, BlockData>,
    /// The map from rust blocks to translated blocks.
    /// Note that when translating terminators like DropAndReplace, we might have
    /// to introduce new blocks which don't appear in the original MIR.
    pub blocks_map: HashMap<mir::BasicBlock, BlockId>,
    /// We register the blocks to translate in a stack, so as to avoid
    /// writing the translation functions as recursive functions. We do
    /// so because we had stack overflows in the past.
    pub blocks_stack: VecDeque<mir::BasicBlock>,
}

impl<'tcx, 'tctx, 'ictx> BodyTransCtx<'tcx, 'tctx, 'ictx> {
    pub(crate) fn new(
        i_ctx: &'ictx mut ItemTransCtx<'tcx, 'tctx>,
        body: &'ictx Rc<mir::Body<'tcx>>,
        drop_kind: DropKind,
    ) -> Self {
        i_ctx.lifetime_freshener = Some(IndexMap::new());
        BodyTransCtx {
            i_ctx,
            local_decls: &body.local_decls,
            drop_kind,
            locals: Default::default(),
            locals_map: Default::default(),
            blocks: Default::default(),
            blocks_map: Default::default(),
            blocks_stack: Default::default(),
        }
    }
}

impl<'tcx, 'tctx, 'ictx> Deref for BodyTransCtx<'tcx, 'tctx, 'ictx> {
    type Target = ItemTransCtx<'tcx, 'tctx>;
    fn deref(&self) -> &Self::Target {
        self.i_ctx
    }
}
impl<'tcx, 'tctx, 'ictx> DerefMut for BodyTransCtx<'tcx, 'tctx, 'ictx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.i_ctx
    }
}

/// A translation context for function blocks.
pub(crate) struct BlockTransCtx<'tcx, 'tctx, 'ictx, 'bctx> {
    /// The translation context for the item.
    pub b_ctx: &'bctx mut BodyTransCtx<'tcx, 'tctx, 'ictx>,
    /// List of currently translated statements
    pub statements: Vec<Statement>,
}

impl<'tcx, 'tctx, 'ictx, 'bctx> BlockTransCtx<'tcx, 'tctx, 'ictx, 'bctx> {
    pub(crate) fn new(b_ctx: &'bctx mut BodyTransCtx<'tcx, 'tctx, 'ictx>) -> Self {
        BlockTransCtx {
            b_ctx,
            statements: Vec::new(),
        }
    }
}

impl<'tcx, 'tctx, 'ictx, 'bctx> Deref for BlockTransCtx<'tcx, 'tctx, 'ictx, 'bctx> {
    type Target = BodyTransCtx<'tcx, 'tctx, 'ictx>;
    fn deref(&self) -> &Self::Target {
        self.b_ctx
    }
}
impl<'tcx, 'tctx, 'ictx, 'bctx> DerefMut for BlockTransCtx<'tcx, 'tctx, 'ictx, 'bctx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.b_ctx
    }
}

pub fn translate_variant_id(id: hax::VariantIdx) -> VariantId {
    VariantId::new(id.as_usize())
}

pub fn translate_field_id(id: hax::FieldIdx) -> FieldId {
    FieldId::new(id.index())
}

/// Translate a `BorrowKind`
fn translate_borrow_kind(borrow_kind: mir::BorrowKind) -> BorrowKind {
    match borrow_kind {
        mir::BorrowKind::Shared => BorrowKind::Shared,
        mir::BorrowKind::Mut { kind } => match kind {
            mir::MutBorrowKind::Default => BorrowKind::Mut,
            mir::MutBorrowKind::TwoPhaseBorrow => BorrowKind::TwoPhaseMut,
            mir::MutBorrowKind::ClosureCapture => BorrowKind::UniqueImmutable,
        },
        mir::BorrowKind::Fake(mir::FakeBorrowKind::Shallow) => BorrowKind::Shallow,
        // This one is used only in deref patterns.
        mir::BorrowKind::Fake(mir::FakeBorrowKind::Deep) => unimplemented!(),
    }
}

impl<'tcx> ItemTransCtx<'tcx, '_> {
    /// Translate the MIR body of this definition if it has one. Catches any error and returns
    /// `Body::Error` instead
    pub fn translate_def_body(&mut self, span: Span, def: &hax::FullDef) -> Body {
        match self.translate_def_body_inner(span, def) {
            Ok(body) => body,
            Err(e) => Body::Error(e),
        }
    }

    fn translate_def_body_inner(&mut self, span: Span, def: &hax::FullDef) -> Result<Body, Error> {
        // Retrieve the body
        if let Some(body) = self.get_mir(def.this(), span)? {
            Ok(self.translate_body(span, body, &def.source_text))
        } else {
            if let hax::FullDefKind::Const { value, .. }
            | hax::FullDefKind::AssocConst { value, .. } = def.kind()
                && let Some(value) = value
            {
                // For globals we can generate a body by evaluating the global.
                // TODO: we lost the MIR of some consts on a rustc update. A trait assoc const
                // default value no longer has a cross-crate MIR so it's unclear how to retreive
                // the value. See the `trait-default-const-cross-crate` test.
                let c = self.translate_constant_expr(span, &value)?;
                let mut bb = BodyBuilder::new(span, 0);
                let ret = bb.new_var(None, c.ty.clone());
                bb.push_statement(StatementKind::Assign(
                    ret,
                    Rvalue::Use(Operand::Const(Box::new(c))),
                ));
                Ok(Body::Unstructured(bb.build()))
            } else {
                Ok(Body::Missing)
            }
        }
    }

    /// Translate a function body. Catches errors and returns `Body::Error` instead.
    /// That's the entrypoint of this module.
    pub fn translate_body(
        &mut self,
        span: Span,
        body: mir::Body<'tcx>,
        source_text: &Option<String>,
    ) -> Body {
        let drop_kind = match body.phase {
            mir::MirPhase::Built | mir::MirPhase::Analysis(..) => DropKind::Conditional,
            mir::MirPhase::Runtime(..) => DropKind::Precise,
        };
        let mut ctx = panic::AssertUnwindSafe(&mut *self);
        let body = panic::AssertUnwindSafe(body);
        // Stopgap measure because there are still many panics in charon and hax.
        let res = panic::catch_unwind(move || {
            let body = Rc::new({ body }.0);
            let ctx = BodyTransCtx::new(&mut *ctx, &body, drop_kind);
            ctx.translate_body(&body, source_text)
        });
        match res {
            Ok(Ok(body)) => body,
            // Translation error
            Ok(Err(e)) => Body::Error(e),
            // Panic
            Err(_) => {
                let e = register_error!(self, span, "Thread panicked when extracting body.");
                Body::Error(e)
            }
        }
    }
}

impl<'tcx> BodyTransCtx<'tcx, '_, '_> {
    pub(crate) fn translate_local(&self, local: &mir::Local) -> Option<LocalId> {
        self.locals_map.get(&local.index()).copied()
    }

    pub(crate) fn push_var(&mut self, rid: mir::Local, ty: Ty, name: Option<String>, span: Span) {
        let local_id = self.locals.locals.push_with(|index| Local {
            index,
            name,
            span,
            ty,
        });
        self.locals_map.insert(rid.as_usize(), local_id);
    }

    /// Translate a function's local variables by adding them in the environment.
    fn translate_body_locals(&mut self, body: &mir::Body<'tcx>) -> Result<(), Error> {
        // Translate the parameters
        for (index, var) in body.local_decls.iter_enumerated() {
            // Find the name of the variable
            let name: Option<String> = hax::name_of_local(index, &body.var_debug_info);

            // Translate the type
            let span = self.translate_span(&var.source_info.span);
            let ty = self.translate_rustc_ty(span, &var.ty)?;

            // Add the variable to the environment
            self.push_var(index, ty, name, span);
        }

        Ok(())
    }

    /// Translate a basic block id and register it, if it hasn't been done.
    fn translate_basic_block_id(&mut self, block_id: mir::BasicBlock) -> BlockId {
        match self.blocks_map.get(&block_id) {
            Some(id) => *id,
            // Generate a fresh id - this also registers the block
            None => {
                // Push to the stack of blocks awaiting translation
                self.blocks_stack.push_back(block_id);
                let id = self.blocks.reserve_slot();
                // Insert in the map
                self.blocks_map.insert(block_id, id);
                id
            }
        }
    }

    fn translate_basic_block(
        &mut self,
        source_scopes: &rustc_index::IndexVec<mir::SourceScope, mir::SourceScopeData>,
        block: &mir::BasicBlockData<'tcx>,
    ) -> Result<BlockData, Error> {
        // Translate the statements
        let mut block_ctx = BlockTransCtx::new(self);
        for statement in &block.statements {
            trace!("statement: {:?}", statement);
            block_ctx.translate_statement(source_scopes, statement)?;
        }

        // Translate the terminator
        let terminator = block.terminator.as_ref().unwrap();
        let terminator = block_ctx.translate_terminator(source_scopes, terminator)?;

        Ok(BlockData {
            statements: block_ctx.statements,
            terminator,
        })
    }

    /// Gather all the lines that start with `//` inside the given span.
    fn translate_body_comments(
        &mut self,
        source_text: &Option<String>,
        charon_span: Span,
    ) -> Vec<(usize, Vec<String>)> {
        if let Some(body_text) = source_text {
            let mut comments = body_text
                .lines()
                // Iter through the lines of this body in reverse order.
                .rev()
                .enumerate()
                // Compute the absolute line number
                .filter_map(|(i, line)| Some((charon_span.data.end.line.checked_sub(i)?, line)))
                // Extract the comment if this line starts with `//`
                .map(|(line_nbr, line)| (line_nbr, line.trim_start().strip_prefix("//")))
                .peekable()
                .batching(|iter| {
                    // Get the next line. This is not a comment: it's either the last line of the
                    // body or a line that wasn't consumed by `peeking_take_while`.
                    let (line_nbr, _first) = iter.next()?;
                    // Collect all the comments before this line.
                    let mut comments = iter
                        // `peeking_take_while` ensures we don't consume a line that returns
                        // `false`. It will be consumed by the next round of `batching`.
                        .peeking_take_while(|(_, opt_comment)| opt_comment.is_some())
                        .map(|(_, opt_comment)| opt_comment.unwrap())
                        .map(|s| s.strip_prefix(" ").unwrap_or(s))
                        .map(str::to_owned)
                        .collect_vec();
                    comments.reverse();
                    Some((line_nbr, comments))
                })
                .filter(|(_, comments)| !comments.is_empty())
                .collect_vec();
            comments.reverse();
            comments
        } else {
            Vec::new()
        }
    }

    fn translate_body(
        mut self,
        mir_body: &mir::Body<'tcx>,
        source_text: &Option<String>,
    ) -> Result<Body, Error> {
        // Compute the span information
        let span = self.translate_span(&mir_body.span);

        // Initialize the local variables
        trace!("Translating the body locals");
        self.locals.arg_count = mir_body.arg_count;
        self.translate_body_locals(&mir_body)?;

        // Translate the expression body
        trace!("Translating the expression body");

        // Register the start block
        let id = self.translate_basic_block_id(rustc_index::Idx::new(mir::START_BLOCK.as_usize()));
        assert!(id == START_BLOCK_ID);

        // For as long as there are blocks in the stack, translate them
        while let Some(mir_block_id) = self.blocks_stack.pop_front() {
            let mir_block = mir_body.basic_blocks.get(mir_block_id).unwrap();
            let block_id = self.translate_basic_block_id(mir_block_id);
            let block = self.translate_basic_block(&mir_body.source_scopes, mir_block)?;
            self.blocks.set_slot(block_id, block);
        }

        // Create the body
        let comments = self.translate_body_comments(source_text, span);
        Ok(Body::Unstructured(ExprBody {
            span,
            locals: self.locals,
            bound_body_regions: self.i_ctx.lifetime_freshener.take().unwrap().slot_count(),
            body: self.blocks.make_contiguous(),
            comments,
        }))
    }
}

impl<'tcx> BlockTransCtx<'tcx, '_, '_, '_> {
    fn translate_binaryop_kind(&mut self, _span: Span, binop: mir::BinOp) -> Result<BinOp, Error> {
        Ok(match binop {
            mir::BinOp::BitXor => BinOp::BitXor,
            mir::BinOp::BitAnd => BinOp::BitAnd,
            mir::BinOp::BitOr => BinOp::BitOr,
            mir::BinOp::Eq => BinOp::Eq,
            mir::BinOp::Lt => BinOp::Lt,
            mir::BinOp::Le => BinOp::Le,
            mir::BinOp::Ne => BinOp::Ne,
            mir::BinOp::Ge => BinOp::Ge,
            mir::BinOp::Gt => BinOp::Gt,
            mir::BinOp::Add => BinOp::Add(OverflowMode::Wrap),
            mir::BinOp::AddUnchecked => BinOp::Add(OverflowMode::UB),
            mir::BinOp::Sub => BinOp::Sub(OverflowMode::Wrap),
            mir::BinOp::SubUnchecked => BinOp::Sub(OverflowMode::UB),
            mir::BinOp::Mul => BinOp::Mul(OverflowMode::Wrap),
            mir::BinOp::MulUnchecked => BinOp::Mul(OverflowMode::UB),
            mir::BinOp::Div => BinOp::Div(OverflowMode::UB),
            mir::BinOp::Rem => BinOp::Rem(OverflowMode::UB),
            mir::BinOp::AddWithOverflow => BinOp::AddChecked,
            mir::BinOp::SubWithOverflow => BinOp::SubChecked,
            mir::BinOp::MulWithOverflow => BinOp::MulChecked,
            mir::BinOp::Shl => BinOp::Shl(OverflowMode::Wrap),
            mir::BinOp::ShlUnchecked => BinOp::Shl(OverflowMode::UB),
            mir::BinOp::Shr => BinOp::Shr(OverflowMode::Wrap),
            mir::BinOp::ShrUnchecked => BinOp::Shr(OverflowMode::UB),
            mir::BinOp::Cmp => BinOp::Cmp,
            mir::BinOp::Offset => BinOp::Offset,
        })
    }

    fn translate_place(
        &mut self,
        span: Span,
        mir_place: &mir::Place<'tcx>,
    ) -> Result<Place, Error> {
        use hax::{HasBase, SInto};
        use rustc_middle::ty;

        let tcx = self.hax_state.base().tcx;
        let local_decls = self.local_decls;
        let ptr_size = self.t_ctx.translated.target_information.target_pointer_size;

        let mut place_ty: mir::PlaceTy = mir::Place::from(mir_place.local).ty(local_decls, tcx);
        let var_id = self.translate_local(&mir_place.local).unwrap();
        let mut place = self.locals.place_for_var(var_id);
        for elem in mir_place.projection.as_slice() {
            use mir::ProjectionElem::*;
            if let TyKind::Error(msg) = place.ty().kind() {
                return Err(Error {
                    span,
                    msg: msg.clone(),
                });
            }
            let projected_place_ty = place_ty.projection_ty(tcx, *elem);
            let next_place_ty = projected_place_ty.ty.sinto(&self.hax_state);
            let next_place_ty = self.translate_ty(span, &next_place_ty)?;
            let proj_elem = match elem {
                Deref => ProjectionElem::Deref,
                Field(index, _) => {
                    let TyKind::Adt(tref) = place.ty().kind() else {
                        raise_error!(
                            self,
                            span,
                            "found unexpected type in field projection: {}",
                            next_place_ty.with_ctx(&self.into_fmt())
                        )
                    };
                    let field_id = translate_field_id(*index);
                    match place_ty.ty.kind() {
                        ty::Adt(adt_def, _) => {
                            let variant = place_ty.variant_index;
                            let variant_id = variant.map(translate_variant_id);
                            let generics = &tref.generics;
                            match tref.id {
                                TypeId::Adt(type_id) => {
                                    assert!(
                                        ((adt_def.is_struct() || adt_def.is_union())
                                            && variant.is_none())
                                            || (adt_def.is_enum() && variant.is_some())
                                    );
                                    let field_proj = FieldProjKind::Adt(type_id, variant_id);
                                    ProjectionElem::Field(field_proj, field_id)
                                }
                                TypeId::Tuple => {
                                    assert!(generics.regions.is_empty());
                                    assert!(variant.is_none());
                                    assert!(generics.const_generics.is_empty());
                                    let field_proj =
                                        FieldProjKind::Tuple(generics.types.elem_count());
                                    ProjectionElem::Field(field_proj, field_id)
                                }
                                TypeId::Builtin(BuiltinTy::Box) => {
                                    // Some sanity checks
                                    assert!(generics.regions.is_empty());
                                    assert!(generics.types.elem_count() == 2);
                                    assert!(generics.const_generics.is_empty());
                                    assert!(field_id == FieldId::ZERO);
                                    // We pretend this is a deref.
                                    ProjectionElem::Deref
                                }
                                _ => {
                                    raise_error!(self, span, "Unexpected field projection")
                                }
                            }
                        }
                        ty::Tuple(_types) => {
                            let field_proj = FieldProjKind::Tuple(tref.generics.types.elem_count());
                            ProjectionElem::Field(field_proj, field_id)
                        }
                        // We get there when we access one of the fields of the the state
                        // captured by a closure.
                        ty::Closure(..) => {
                            let type_id = *tref.id.as_adt().unwrap();
                            let field_proj = FieldProjKind::Adt(type_id, None);
                            ProjectionElem::Field(field_proj, field_id)
                        }
                        _ => panic!(),
                    }
                }
                Index(local) => {
                    let var_id = self.translate_local(local).unwrap();
                    let local = self.locals.place_for_var(var_id);
                    let offset = Operand::Copy(local);
                    ProjectionElem::Index {
                        offset: Box::new(offset),
                        from_end: false,
                    }
                }
                &ConstantIndex {
                    offset, from_end, ..
                } => {
                    let offset = Operand::Const(Box::new(
                        ScalarValue::mk_usize(ptr_size, offset).to_constant(),
                    ));
                    ProjectionElem::Index {
                        offset: Box::new(offset),
                        from_end,
                    }
                }
                &Subslice { from, to, from_end } => {
                    let from = Operand::Const(Box::new(
                        ScalarValue::mk_usize(ptr_size, from).to_constant(),
                    ));
                    let to =
                        Operand::Const(Box::new(ScalarValue::mk_usize(ptr_size, to).to_constant()));
                    ProjectionElem::Subslice {
                        from: Box::new(from),
                        to: Box::new(to),
                        from_end,
                    }
                }
                OpaqueCast(..) => {
                    raise_error!(self, span, "Unexpected ProjectionElem::OpaqueCast");
                }
                Downcast { .. } => {
                    // We keep the same `Place`, the variant is tracked in the `PlaceTy` and we can
                    // access it next loop iteration.
                    place_ty = projected_place_ty;
                    continue;
                }
                UnwrapUnsafeBinder { .. } => {
                    raise_error!(self, span, "unsupported feature: unsafe binders");
                }
            };
            place = place.project(proj_elem, next_place_ty);
            place_ty = projected_place_ty;
        }
        Ok(place)
    }

    /// Translate an operand
    fn translate_operand(
        &mut self,
        span: Span,
        operand: &mir::Operand<'tcx>,
    ) -> Result<Operand, Error> {
        Ok(match operand {
            mir::Operand::Copy(place) => {
                let p = self.translate_place(span, place)?;
                Operand::Copy(p)
            }
            mir::Operand::Move(place) => {
                let p = self.translate_place(span, place)?;
                Operand::Move(p)
            }
            mir::Operand::Constant(const_op) => {
                let const_op = self.catch_sinto(span, &const_op)?;
                match &const_op.kind {
                    hax::ConstOperandKind::Value(constant) => {
                        let constant = self.translate_constant_expr(span, constant)?;
                        Operand::Const(Box::new(constant))
                    }
                    hax::ConstOperandKind::Promoted(item) => {
                        // A promoted constant that could not be evaluated.
                        let global_ref = self.translate_global_decl_ref(span, item)?;
                        let constant = ConstantExpr {
                            kind: ConstantExprKind::Global(global_ref),
                            ty: self.translate_ty(span, &const_op.ty)?,
                        };
                        Operand::Const(Box::new(constant))
                    }
                }
            }
            mir::Operand::RuntimeChecks(check) => {
                let op = match check {
                    mir::RuntimeChecks::UbChecks => NullOp::UbChecks,
                    mir::RuntimeChecks::OverflowChecks => NullOp::OverflowChecks,
                    mir::RuntimeChecks::ContractChecks => NullOp::ContractChecks,
                };
                let local = self.locals.new_var(None, Ty::mk_bool());
                self.statements.push(Statement {
                    span,
                    kind: StatementKind::StorageLive(local.as_local().unwrap()),
                    comments_before: vec![],
                });
                self.statements.push(Statement {
                    span,
                    kind: StatementKind::Assign(
                        local.clone(),
                        Rvalue::NullaryOp(op, Ty::mk_bool()),
                    ),
                    comments_before: vec![],
                });
                Operand::Move(local)
            }
        })
    }

    /// Translate an rvalue
    fn translate_mir_rvalue(
        &mut self,
        span: Span,
        rvalue: &mir::Rvalue<'tcx>,
        tgt_ty: &Ty,
    ) -> Result<Rvalue, Error> {
        match rvalue {
            mir::Rvalue::Use(operand) => Ok(Rvalue::Use(self.translate_operand(span, operand)?)),
            mir::Rvalue::CopyForDeref(place) => {
                // According to the documentation, it seems to be an optimisation
                // for drop elaboration. We treat it as a regular copy.
                let place = self.translate_place(span, place)?;
                Ok(Rvalue::Use(Operand::Copy(place)))
            }
            mir::Rvalue::Repeat(operand, cnst) => {
                let c = self.translate_ty_constant_expr(span, cnst)?;
                let op = self.translate_operand(span, operand)?;
                let ty = op.ty().clone();
                // Remark: we could desugar this into a function call later.
                Ok(Rvalue::Repeat(op, ty, Box::new(c)))
            }
            mir::Rvalue::Ref(_region, borrow_kind, place) => {
                let place = self.translate_place(span, place)?;
                let borrow_kind = translate_borrow_kind(*borrow_kind);
                Ok(Rvalue::Ref {
                    place,
                    kind: borrow_kind,
                    // Will be fixed by the cleanup pass `insert_ptr_metadata`.
                    ptr_metadata: Operand::Const(Box::new(ConstantExpr {
                        kind: ConstantExprKind::Opaque("Missing metadata".to_string()),
                        ty: Ty::mk_unit(),
                    })),
                })
            }
            mir::Rvalue::RawPtr(mtbl, place) => {
                let mtbl = match mtbl {
                    mir::RawPtrKind::Mut => RefKind::Mut,
                    mir::RawPtrKind::Const => RefKind::Shared,
                    mir::RawPtrKind::FakeForPtrMetadata => RefKind::Shared,
                };
                let place = self.translate_place(span, place)?;
                Ok(Rvalue::RawPtr {
                    place,
                    kind: mtbl,
                    // Will be fixed by the cleanup pass `insert_ptr_metadata`.
                    ptr_metadata: Operand::Const(Box::new(ConstantExpr {
                        kind: ConstantExprKind::Opaque("Missing metadata".to_string()),
                        ty: Ty::mk_unit(),
                    })),
                })
            }
            mir::Rvalue::Cast(cast_kind, mir_operand, rust_tgt_ty) => {
                let op_ty = mir_operand.ty(self.local_decls, self.tcx);
                let tgt_ty = self.translate_rustc_ty(span, rust_tgt_ty)?;

                // Translate the operand
                let mut operand = self.translate_operand(span, mir_operand)?;
                let src_ty = operand.ty().clone();

                let cast_kind = match cast_kind {
                    mir::CastKind::IntToInt
                    | mir::CastKind::IntToFloat
                    | mir::CastKind::FloatToInt
                    | mir::CastKind::FloatToFloat => {
                        let tgt_ty = *tgt_ty.kind().as_literal().unwrap();
                        let src_ty = *src_ty.kind().as_literal().unwrap();
                        CastKind::Scalar(src_ty, tgt_ty)
                    }
                    mir::CastKind::PtrToPtr
                    | mir::CastKind::PointerCoercion(
                        ty::adjustment::PointerCoercion::MutToConstPointer,
                        ..,
                    )
                    | mir::CastKind::PointerCoercion(
                        ty::adjustment::PointerCoercion::ArrayToPointer,
                        ..,
                    )
                    | mir::CastKind::FnPtrToPtr
                    | mir::CastKind::PointerExposeProvenance
                    | mir::CastKind::PointerWithExposedProvenance => {
                        CastKind::RawPtr(src_ty, tgt_ty)
                    }
                    mir::CastKind::PointerCoercion(
                        ty::adjustment::PointerCoercion::ClosureFnPointer(_),
                        ..,
                    ) => {
                        let hax_op_ty: hax::Ty = self.catch_sinto(span, &op_ty)?;
                        // We model casts of closures to function pointers by generating a new
                        // function item without the closure's state, that calls the actual closure.
                        let hax::TyKind::Closure(closure, ..) = hax_op_ty.kind() else {
                            unreachable!("Non-closure type in PointerCoercion::ClosureFnPointer");
                        };
                        let fn_ref: RegionBinder<FunDeclRef> =
                            self.translate_stateless_closure_as_fn_ref(span, closure)?;
                        let fn_ptr_bound: RegionBinder<FnPtr> = fn_ref.map(FunDeclRef::into);
                        let fn_ptr: FnPtr = self.erase_region_binder(fn_ptr_bound.clone());
                        let src_ty = TyKind::FnDef(fn_ptr_bound).into_ty();
                        operand = Operand::Const(Box::new(ConstantExpr {
                            kind: ConstantExprKind::FnDef(fn_ptr),
                            ty: src_ty.clone(),
                        }));
                        CastKind::FnPtr(src_ty, tgt_ty)
                    }
                    mir::CastKind::PointerCoercion(
                        ty::adjustment::PointerCoercion::UnsafeFnPointer
                        | ty::adjustment::PointerCoercion::ReifyFnPointer(_),
                        ..,
                    ) => CastKind::FnPtr(src_ty, tgt_ty),
                    mir::CastKind::Transmute => CastKind::Transmute(src_ty, tgt_ty),
                    // TODO
                    mir::CastKind::Subtype => CastKind::Transmute(src_ty, tgt_ty),
                    mir::CastKind::PointerCoercion(ty::adjustment::PointerCoercion::Unsize, ..) => {
                        let meta =
                            hax::compute_unsizing_metadata(&self.hax_state, op_ty, *rust_tgt_ty);
                        let meta = match &meta {
                            hax::UnsizingMetadata::Length(len) => {
                                let len = self.translate_constant_expr(span, len)?;
                                UnsizingMetadata::Length(Box::new(len))
                            }
                            hax::UnsizingMetadata::DirectVTable(impl_expr) => {
                                let tref = self.translate_trait_impl_expr(span, impl_expr)?;
                                let vtable =
                                    self.translate_vtable_instance_const(span, impl_expr)?;
                                UnsizingMetadata::VTable(tref, vtable)
                            }
                            hax::UnsizingMetadata::NestedVTable(dyn_impl_expr) => {
                                // This binds a fake `T: SrcTrait` variable.
                                let binder = self.translate_dyn_binder(
                                    span,
                                    dyn_impl_expr,
                                    |ctx, _, impl_expr| {
                                        ctx.translate_trait_impl_expr(span, impl_expr)
                                    },
                                )?;

                                // Compute the supertrait path from the source tref to the target
                                // tref.
                                let mut target_tref = &binder.skip_binder;
                                let mut clause_path: Vec<(TraitDeclId, TraitClauseId)> = vec![];
                                while let TraitRefKind::ParentClause(tref, id) = &target_tref.kind {
                                    clause_path.push((tref.trait_decl_ref.skip_binder.id, *id));
                                    target_tref = tref;
                                }

                                let mut field_path = vec![];
                                for &(trait_id, clause_id) in &clause_path {
                                    if let Ok(ItemRef::TraitDecl(tdecl)) =
                                        self.get_or_translate(trait_id.into())
                                        && let &vtable_decl_id =
                                            tdecl.vtable.as_ref().unwrap().id.as_adt().unwrap()
                                        && let Ok(ItemRef::Type(vtable_decl)) =
                                            self.get_or_translate(vtable_decl_id.into())
                                    {
                                        let ItemSource::VTableTy { supertrait_map, .. } =
                                            &vtable_decl.src
                                        else {
                                            unreachable!()
                                        };
                                        field_path.push(supertrait_map[clause_id].unwrap());
                                    } else {
                                        break;
                                    }
                                }

                                if field_path.len() == clause_path.len() {
                                    UnsizingMetadata::VTableUpcast(field_path)
                                } else {
                                    UnsizingMetadata::Unknown
                                }
                            }
                            hax::UnsizingMetadata::Unknown => UnsizingMetadata::Unknown,
                        };
                        CastKind::Unsize(src_ty, tgt_ty.clone(), meta)
                    }
                };
                let unop = UnOp::Cast(cast_kind);
                Ok(Rvalue::UnaryOp(unop, operand))
            }
            mir::Rvalue::BinaryOp(binop, (left, right)) => Ok(Rvalue::BinaryOp(
                self.translate_binaryop_kind(span, *binop)?,
                self.translate_operand(span, left)?,
                self.translate_operand(span, right)?,
            )),
            mir::Rvalue::UnaryOp(unop, operand) => {
                let operand = self.translate_operand(span, operand)?;
                let unop = match unop {
                    mir::UnOp::Not => UnOp::Not,
                    mir::UnOp::Neg => UnOp::Neg(OverflowMode::Wrap),
                    mir::UnOp::PtrMetadata => match operand {
                        Operand::Copy(p) | Operand::Move(p) => {
                            return Ok(Rvalue::Use(Operand::Copy(
                                p.project(ProjectionElem::PtrMetadata, tgt_ty.clone()),
                            )));
                        }
                        Operand::Const(_) => {
                            panic!("unexpected metadata operand")
                        }
                    },
                };
                Ok(Rvalue::UnaryOp(unop, operand))
            }
            mir::Rvalue::Discriminant(place) => {
                let place = self.translate_place(span, place)?;
                // We should always know the enum type; it can't be a generic.
                if !place
                    .ty()
                    .kind()
                    .as_adt()
                    .is_some_and(|tref| tref.id.is_adt())
                {
                    raise_error!(
                        self,
                        span,
                        "Unexpected scrutinee type for ReadDiscriminant: {}",
                        place.ty().with_ctx(&self.into_fmt())
                    )
                }
                Ok(Rvalue::Discriminant(place))
            }
            mir::Rvalue::Aggregate(aggregate_kind, operands) => {
                // It seems this instruction is not present in certain passes:
                // for example, it seems it is not used in optimized MIR, where
                // ADT initialization is split into several instructions, for
                // instance:
                // ```
                // p = Pair { x:xv, y:yv };
                // ```
                // Might become:
                // ```
                // p.x = x;
                // p.y = yv;
                // ```

                // First translate the operands
                let operands_t: Vec<Operand> = operands
                    .iter()
                    .map(|op| self.translate_operand(span, op))
                    .try_collect()?;
                let ptr_size = self.t_ctx.translated.target_information.target_pointer_size;

                match aggregate_kind {
                    mir::AggregateKind::Array(ty) => {
                        let t_ty = self.translate_rustc_ty(span, ty)?;
                        let c = ConstantExpr::mk_usize(
                            ScalarValue::from_uint(
                                ptr_size,
                                UIntTy::Usize,
                                operands_t.len() as u128,
                            )
                            .unwrap(),
                        );
                        Ok(Rvalue::Aggregate(
                            AggregateKind::Array(t_ty, Box::new(c)),
                            operands_t,
                        ))
                    }
                    mir::AggregateKind::Tuple => {
                        let tref = TypeDeclRef::new(TypeId::Tuple, GenericArgs::empty());
                        Ok(Rvalue::Aggregate(
                            AggregateKind::Adt(tref, None, None),
                            operands_t,
                        ))
                    }
                    mir::AggregateKind::Adt(def_id, variant_idx, generics, _, field_index) => {
                        use ty::AdtKind;
                        trace!("{:?}", rvalue);

                        let adt_kind = self.tcx.adt_def(def_id).adt_kind();
                        let item = hax::translate_item_ref(&self.hax_state, *def_id, generics);
                        let tref = self.translate_type_decl_ref(span, &item)?;
                        let variant_id = match adt_kind {
                            AdtKind::Struct | AdtKind::Union => None,
                            AdtKind::Enum => Some(translate_variant_id(*variant_idx)),
                        };
                        let field_id = match adt_kind {
                            AdtKind::Struct | AdtKind::Enum => None,
                            AdtKind::Union => Some(translate_field_id(field_index.unwrap())),
                        };

                        let akind = AggregateKind::Adt(tref, variant_id, field_id);
                        Ok(Rvalue::Aggregate(akind, operands_t))
                    }
                    mir::AggregateKind::Closure(def_id, generics) => {
                        let args = hax::ClosureArgs::sfrom(&self.hax_state, *def_id, generics);
                        let tref = self.translate_closure_type_ref(span, &args)?;
                        let akind = AggregateKind::Adt(tref, None, None);
                        Ok(Rvalue::Aggregate(akind, operands_t))
                    }
                    mir::AggregateKind::RawPtr(ty, mutability) => {
                        // TODO: replace with a call to `ptr::from_raw_parts`.
                        let t_ty = self.translate_rustc_ty(span, ty)?;
                        let mutability = if mutability.is_mut() {
                            RefKind::Mut
                        } else {
                            RefKind::Shared
                        };

                        let akind = AggregateKind::RawPtr(t_ty, mutability);

                        Ok(Rvalue::Aggregate(akind, operands_t))
                    }
                    mir::AggregateKind::Coroutine(..)
                    | mir::AggregateKind::CoroutineClosure(..) => {
                        raise_error!(self, span, "Coroutines are not supported");
                    }
                }
            }
            mir::Rvalue::ShallowInitBox(op, ty) => {
                let op = self.translate_operand(span, op)?;
                let ty = self.translate_rustc_ty(span, ty)?;
                Ok(Rvalue::ShallowInitBox(op, ty))
            }
            mir::Rvalue::ThreadLocalRef(_) => {
                raise_error!(
                    self,
                    span,
                    "charon does not support thread local references"
                );
            }
            mir::Rvalue::WrapUnsafeBinder { .. } => {
                raise_error!(
                    self,
                    span,
                    "charon does not support unsafe lifetime binders"
                );
            }
        }
    }

    /// Translate a statement
    ///
    /// We return an option, because we ignore some statements (`Nop`, `StorageLive`...)
    fn translate_statement(
        &mut self,
        source_scopes: &rustc_index::IndexVec<mir::SourceScope, mir::SourceScopeData>,
        statement: &mir::Statement<'tcx>,
    ) -> Result<(), Error> {
        trace!("About to translate statement (MIR) {:?}", statement);
        let span = self.translate_span_from_source_info(source_scopes, &statement.source_info);

        let t_statement: Option<StatementKind> = match &statement.kind {
            mir::StatementKind::Assign((place, rvalue)) => {
                let t_place = self.translate_place(span, place)?;
                let t_rvalue = self.translate_mir_rvalue(span, rvalue, t_place.ty())?;
                Some(StatementKind::Assign(t_place, t_rvalue))
            }
            mir::StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                let t_place = self.translate_place(span, place)?;
                let variant_id = translate_variant_id(*variant_index);
                Some(StatementKind::SetDiscriminant(t_place, variant_id))
            }
            mir::StatementKind::StorageLive(local) => {
                let var_id = self.translate_local(local).unwrap();
                Some(StatementKind::StorageLive(var_id))
            }
            mir::StatementKind::StorageDead(local) => {
                let var_id = self.translate_local(local).unwrap();
                Some(StatementKind::StorageDead(var_id))
            }
            // This asserts the operand true on pain of UB. We treat it like a normal assertion.
            mir::StatementKind::Intrinsic(mir::NonDivergingIntrinsic::Assume(op)) => {
                let op = self.translate_operand(span, op)?;
                Some(StatementKind::Assert {
                    assert: Assert {
                        cond: op,
                        expected: true,
                        check_kind: None,
                    },
                    on_failure: AbortKind::UndefinedBehavior,
                })
            }
            mir::StatementKind::Intrinsic(mir::NonDivergingIntrinsic::CopyNonOverlapping(
                mir::CopyNonOverlapping { src, dst, count },
            )) => {
                let src = self.translate_operand(span, src)?;
                let dst = self.translate_operand(span, dst)?;
                let count = self.translate_operand(span, count)?;
                Some(StatementKind::CopyNonOverlapping(Box::new(
                    CopyNonOverlapping { src, dst, count },
                )))
            }
            mir::StatementKind::PlaceMention(place) => {
                let place = self.translate_place(span, place)?;
                // We only translate this for places with projections, as
                // no UB can arise from simply mentioning a local variable.
                if place.is_local() {
                    None
                } else {
                    Some(StatementKind::PlaceMention(place))
                }
            }
            // This is for the stacked borrows memory model.
            mir::StatementKind::Retag(_, _) => None,
            // These two are only there to make borrow-checking accept less code, and are removed
            // in later MIRs.
            mir::StatementKind::FakeRead(..) => None,
            // There are user-provided type annotations with no semantic effect (since we get a
            // fully-typechecked MIR (TODO: this isn't quite true with opaque types, we should
            // really use promoted MIR)).
            mir::StatementKind::AscribeUserType(..) => None,
            // Used for coverage instrumentation.
            mir::StatementKind::Coverage(_) => None,
            // Used in the interpreter to check that const code doesn't run for too long or even
            // indefinitely.
            mir::StatementKind::ConstEvalCounter => None,
            // Semantically equivalent to `Nop`, used only for rustc lints.
            mir::StatementKind::BackwardIncompatibleDropHint { .. } => None,
            mir::StatementKind::Nop => None,
        };

        // Add the span information
        let Some(t_statement) = t_statement else {
            return Ok(());
        };
        let statement = Statement::new(span, t_statement);
        self.statements.push(statement);
        Ok(())
    }

    /// Translate a terminator
    fn translate_terminator(
        &mut self,
        source_scopes: &rustc_index::IndexVec<mir::SourceScope, mir::SourceScopeData>,
        terminator: &mir::Terminator<'tcx>,
    ) -> Result<Terminator, Error> {
        trace!("About to translate terminator (MIR) {:?}", terminator);
        let span = self.translate_span_from_source_info(source_scopes, &terminator.source_info);

        // Translate the terminator
        use mir::TerminatorKind;
        let t_terminator: ullbc_ast::TerminatorKind = match &terminator.kind {
            TerminatorKind::Goto { target } => {
                let target = self.translate_basic_block_id(*target);
                ullbc_ast::TerminatorKind::Goto { target }
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                let discr = self.translate_operand(span, discr)?;
                let targets = self.translate_switch_targets(span, discr.ty(), targets)?;
                ullbc_ast::TerminatorKind::Switch { discr, targets }
            }
            TerminatorKind::UnwindResume => ullbc_ast::TerminatorKind::UnwindResume,
            TerminatorKind::UnwindTerminate { .. } => {
                ullbc_ast::TerminatorKind::Abort(AbortKind::UnwindTerminate)
            }
            TerminatorKind::Return => ullbc_ast::TerminatorKind::Return,
            // A MIR `Unreachable` terminator indicates undefined behavior of the rust abstract
            // machine.
            TerminatorKind::Unreachable => {
                ullbc_ast::TerminatorKind::Abort(AbortKind::UndefinedBehavior)
            }
            TerminatorKind::Drop {
                place,
                target,
                unwind,
                ..
            } => self.translate_drop(span, place, target, unwind)?,
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                unwind,
                fn_span: _,
                ..
            } => self.translate_function_call(span, func, args, destination, target, unwind)?,
            TerminatorKind::Assert {
                cond,
                expected,
                msg,
                target,
                unwind,
            } => {
                let on_unwind = self.translate_unwind_action(span, unwind);
                let kind = self.translate_assert_kind(span, msg)?;
                let assert = Assert {
                    cond: self.translate_operand(span, cond)?,
                    expected: *expected,
                    check_kind: Some(kind),
                };
                let target = self.translate_basic_block_id(*target);
                ullbc_ast::TerminatorKind::Assert {
                    assert,
                    target,
                    on_unwind,
                }
            }
            TerminatorKind::FalseEdge {
                real_target,
                imaginary_target: _,
            } => {
                // False edges are used to make the borrow checker a bit conservative.
                // We translate them as Gotos.
                // Also note that they are used in some passes, and not in some others
                // (they are present in mir_promoted, but not mir_optimized).
                let target = self.translate_basic_block_id(*real_target);
                ullbc_ast::TerminatorKind::Goto { target }
            }
            TerminatorKind::FalseUnwind {
                real_target,
                unwind: _,
            } => {
                // We consider this to be a goto
                let target = self.translate_basic_block_id(*real_target);
                ullbc_ast::TerminatorKind::Goto { target }
            }
            TerminatorKind::InlineAsm { .. } => {
                raise_error!(self, span, "Inline assembly is not supported");
            }
            TerminatorKind::CoroutineDrop
            | TerminatorKind::TailCall { .. }
            | TerminatorKind::Yield { .. } => {
                raise_error!(self, span, "Unsupported terminator: {:?}", terminator.kind);
            }
        };

        // Add the span information
        Ok(Terminator::new(span, t_terminator))
    }

    /// Translate switch targets
    fn translate_switch_targets(
        &mut self,
        span: Span,
        switch_ty: &Ty,
        targets: &mir::SwitchTargets,
    ) -> Result<SwitchTargets, Error> {
        // Convert all the test values to the proper values.
        let otherwise = targets.otherwise();
        let targets = targets.iter().collect_vec();
        let switch_ty = *switch_ty.kind().as_literal().unwrap();
        match switch_ty {
            LiteralTy::Bool => {
                assert_eq!(targets.len(), 1);
                let (val, target) = targets.first().unwrap();
                // It seems the block targets are inverted
                assert_eq!(*val, 0);
                let if_block = self.translate_basic_block_id(otherwise);
                let then_block = self.translate_basic_block_id(*target);
                Ok(SwitchTargets::If(if_block, then_block))
            }
            LiteralTy::Char => {
                let targets: Vec<(Literal, BlockId)> = targets
                    .iter()
                    .copied()
                    .map(|(v, tgt)| {
                        let v = Literal::char_from_le_bytes(v);
                        let tgt = self.translate_basic_block_id(tgt);
                        (v, tgt)
                    })
                    .collect();
                let otherwise = self.translate_basic_block_id(otherwise);
                Ok(SwitchTargets::SwitchInt(
                    LiteralTy::Char,
                    targets,
                    otherwise,
                ))
            }
            LiteralTy::Int(int_ty) => {
                let targets: Vec<(Literal, BlockId)> = targets
                    .iter()
                    .copied()
                    .map(|(v, tgt)| {
                        let v = Literal::Scalar(ScalarValue::from_le_bytes(
                            IntegerTy::Signed(int_ty),
                            v.to_le_bytes(),
                        ));
                        let tgt = self.translate_basic_block_id(tgt);
                        (v, tgt)
                    })
                    .collect();
                let otherwise = self.translate_basic_block_id(otherwise);
                Ok(SwitchTargets::SwitchInt(switch_ty, targets, otherwise))
            }
            LiteralTy::UInt(uint_ty) => {
                let targets: Vec<(Literal, BlockId)> = targets
                    .iter()
                    .map(|(v, tgt)| {
                        let v = Literal::Scalar(ScalarValue::from_le_bytes(
                            IntegerTy::Unsigned(uint_ty),
                            v.to_le_bytes(),
                        ));
                        let tgt = self.translate_basic_block_id(*tgt);
                        (v, tgt)
                    })
                    .collect();
                let otherwise = self.translate_basic_block_id(otherwise);
                Ok(SwitchTargets::SwitchInt(
                    LiteralTy::UInt(uint_ty),
                    targets,
                    otherwise,
                ))
            }
            _ => raise_error!(self, span, "Can't match on type {switch_ty}"),
        }
    }

    /// Translate a function call statement.
    /// Note that `body` is the body of the function being translated, not of the
    /// function referenced in the function call: we need it in order to translate
    /// the blocks we go to after the function call returns.
    #[allow(clippy::too_many_arguments)]
    fn translate_function_call(
        &mut self,
        span: Span,
        func: &mir::Operand<'tcx>,
        args: &[hax::Spanned<mir::Operand<'tcx>>],
        destination: &mir::Place<'tcx>,
        target: &Option<mir::BasicBlock>,
        unwind: &mir::UnwindAction,
    ) -> Result<TerminatorKind, Error> {
        let tcx = self.tcx;
        let op_ty = func.ty(self.local_decls, tcx);
        // There are two cases, depending on whether this is a "regular"
        // call to a top-level function identified by its id, or if we
        // are using a local function pointer (i.e., the operand is a "move").
        let lval = self.translate_place(span, destination)?;
        let on_unwind = self.translate_unwind_action(span, unwind);
        // Translate the function operand.
        let fn_operand = match op_ty.kind() {
            ty::TyKind::FnDef(def_id, generics) => {
                // The type of the value is one of the singleton types that corresponds to each function,
                // which is enough information.
                let item = &hax::translate_item_ref(&self.hax_state, *def_id, *generics);
                trace!("func: {:?}", item.def_id);
                let fun_def = self.hax_def(item)?;
                let item_src =
                    TransItemSource::from_item(item, TransItemSourceKind::Fun, self.monomorphize());
                let name = self.t_ctx.translate_name(&item_src)?;
                let panic_lang_items = &["panic", "panic_fmt", "begin_panic"];
                let panic_names = &[&["core", "panicking", "assert_failed"], EXPLICIT_PANIC_NAME];

                if fun_def
                    .lang_item
                    .as_ref()
                    .is_some_and(|lang_it| panic_lang_items.iter().contains(&lang_it.as_str()))
                    || panic_names.iter().any(|panic| name.equals_ref_name(panic))
                {
                    // If the call is `panic!`, then the target is `None`.
                    // I don't know in which other cases it can be `None`.
                    assert!(target.is_none());
                    // We ignore the arguments
                    // TODO: shouldn't we do something with the unwind edge?
                    return Ok(TerminatorKind::Abort(AbortKind::Panic(Some(name))));
                } else {
                    let fn_ptr = self.translate_fn_ptr(span, item, TransItemSourceKind::Fun)?;
                    FnOperand::Regular(fn_ptr)
                }
            }
            _ => {
                // Call to a function pointer.
                let op = self.translate_operand(span, func)?;
                FnOperand::Dynamic(op)
            }
        };
        let args = self.translate_arguments(span, args)?;
        let call = Call {
            func: fn_operand,
            args,
            dest: lval,
        };

        let target = match target {
            Some(target) => self.translate_basic_block_id(*target),
            None => {
                let abort =
                    Terminator::new(span, TerminatorKind::Abort(AbortKind::UndefinedBehavior));
                self.blocks.push(abort.into_block())
            }
        };

        Ok(TerminatorKind::Call {
            call,
            target,
            on_unwind,
        })
    }

    /// Translate a drop terminator
    #[allow(clippy::too_many_arguments)]
    fn translate_drop(
        &mut self,
        span: Span,
        place: &mir::Place<'tcx>,
        target: &mir::BasicBlock,
        unwind: &mir::UnwindAction,
    ) -> Result<TerminatorKind, Error> {
        let tref = {
            let place_ty = place.ty(self.local_decls, self.tcx).ty;
            let impl_expr = &hax::solve_destruct(&self.hax_state, place_ty);
            self.translate_trait_impl_expr(span, impl_expr)?
        };
        let place = self.translate_place(span, place)?;
        let target = self.translate_basic_block_id(*target);
        let on_unwind = self.translate_unwind_action(span, unwind);

        Ok(TerminatorKind::Drop {
            kind: self.drop_kind,
            place,
            tref,
            target,
            on_unwind,
        })
    }

    // construct unwind block for the terminators
    fn translate_unwind_action(&mut self, span: Span, unwind: &mir::UnwindAction) -> BlockId {
        let on_unwind = match unwind {
            mir::UnwindAction::Continue => {
                let unwind_continue = Terminator::new(span, TerminatorKind::UnwindResume);
                self.blocks.push(unwind_continue.into_block())
            }
            mir::UnwindAction::Unreachable => {
                let abort =
                    Terminator::new(span, TerminatorKind::Abort(AbortKind::UndefinedBehavior));
                self.blocks.push(abort.into_block())
            }
            mir::UnwindAction::Terminate(..) => {
                let abort =
                    Terminator::new(span, TerminatorKind::Abort(AbortKind::UnwindTerminate));
                self.blocks.push(abort.into_block())
            }
            mir::UnwindAction::Cleanup(bb) => self.translate_basic_block_id(*bb),
        };
        on_unwind
    }

    fn translate_assert_kind(
        &mut self,
        span: Span,
        kind: &mir::AssertKind<mir::Operand<'tcx>>,
    ) -> Result<BuiltinAssertKind, Error> {
        match kind {
            mir::AssertKind::BoundsCheck { len, index } => {
                let len = self.translate_operand(span, len)?;
                let index = self.translate_operand(span, index)?;
                Ok(BuiltinAssertKind::BoundsCheck { len, index })
            }
            mir::AssertKind::Overflow(binop, left, right) => {
                let binop = self.translate_binaryop_kind(span, *binop)?;
                let left = self.translate_operand(span, left)?;
                let right = self.translate_operand(span, right)?;
                Ok(BuiltinAssertKind::Overflow(binop, left, right))
            }
            mir::AssertKind::OverflowNeg(operand) => {
                let operand = self.translate_operand(span, operand)?;
                Ok(BuiltinAssertKind::OverflowNeg(operand))
            }
            mir::AssertKind::DivisionByZero(operand) => {
                let operand = self.translate_operand(span, operand)?;
                Ok(BuiltinAssertKind::DivisionByZero(operand))
            }
            mir::AssertKind::RemainderByZero(operand) => {
                let operand = self.translate_operand(span, operand)?;
                Ok(BuiltinAssertKind::RemainderByZero(operand))
            }
            mir::AssertKind::MisalignedPointerDereference { required, found } => {
                let required = self.translate_operand(span, required)?;
                let found = self.translate_operand(span, found)?;
                Ok(BuiltinAssertKind::MisalignedPointerDereference { required, found })
            }
            mir::AssertKind::NullPointerDereference => {
                Ok(BuiltinAssertKind::NullPointerDereference)
            }
            mir::AssertKind::InvalidEnumConstruction(operand) => {
                let operand = self.translate_operand(span, operand)?;
                Ok(BuiltinAssertKind::InvalidEnumConstruction(operand))
            }
            mir::AssertKind::ResumedAfterDrop(..)
            | mir::AssertKind::ResumedAfterPanic(..)
            | mir::AssertKind::ResumedAfterReturn(..) => {
                raise_error!(self, span, "Coroutines are not supported");
            }
        }
    }

    /// Evaluate function arguments in a context, and return the list of computed
    /// values.
    fn translate_arguments(
        &mut self,
        span: Span,
        args: &[hax::Spanned<mir::Operand<'tcx>>],
    ) -> Result<Vec<Operand>, Error> {
        let mut t_args: Vec<Operand> = Vec::new();
        for arg in args.iter().map(|x| &x.node) {
            // Translate
            let op = self.translate_operand(span, arg)?;
            t_args.push(op);
        }
        Ok(t_args)
    }
}

impl<'a> IntoFormatter for &'a BodyTransCtx<'_, '_, '_> {
    type C = FmtCtx<'a>;
    fn into_fmt(self) -> Self::C {
        FmtCtx {
            locals: Some(&self.locals),
            ..self.i_ctx.into_fmt()
        }
    }
}
