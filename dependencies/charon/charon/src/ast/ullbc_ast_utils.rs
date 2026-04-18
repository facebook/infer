//! Implementations for [crate::ullbc_ast]
use smallvec::{SmallVec, smallvec};

use crate::ids::IndexVec;
use crate::meta::Span;
use crate::ullbc_ast::*;
use std::collections::HashMap;
use std::mem;

impl SwitchTargets {
    pub fn targets(&self) -> SmallVec<[BlockId; 2]> {
        match self {
            SwitchTargets::If(then_tgt, else_tgt) => {
                smallvec![*then_tgt, *else_tgt]
            }
            SwitchTargets::SwitchInt(_, targets, otherwise) => targets
                .iter()
                .map(|(_, t)| t)
                .chain([otherwise])
                .copied()
                .collect(),
        }
    }
    pub fn targets_mut(&mut self) -> SmallVec<[&mut BlockId; 2]> {
        match self {
            SwitchTargets::If(then_tgt, else_tgt) => {
                smallvec![then_tgt, else_tgt]
            }
            SwitchTargets::SwitchInt(_, targets, otherwise) => targets
                .iter_mut()
                .map(|(_, t)| t)
                .chain([otherwise])
                .collect(),
        }
    }
}

impl Statement {
    pub fn new(span: Span, kind: StatementKind) -> Self {
        Statement {
            span,
            kind,
            comments_before: vec![],
        }
    }
}

impl Terminator {
    pub fn new(span: Span, kind: TerminatorKind) -> Self {
        Terminator {
            span,
            kind,
            comments_before: vec![],
        }
    }
    pub fn goto(span: Span, target: BlockId) -> Self {
        Self::new(span, TerminatorKind::Goto { target })
    }
    /// Whether this terminator is an unconditional error (panic).
    pub fn is_error(&self) -> bool {
        use TerminatorKind::*;
        match &self.kind {
            Abort(..) => true,
            Goto { .. }
            | Switch { .. }
            | Return
            | Call { .. }
            | Drop { .. }
            | UnwindResume
            | Assert { .. } => false,
        }
    }

    pub fn into_block(self) -> BlockData {
        BlockData {
            statements: vec![],
            terminator: self,
        }
    }

    pub fn targets(&self) -> SmallVec<[BlockId; 2]> {
        match &self.kind {
            TerminatorKind::Goto { target } => {
                smallvec![*target]
            }
            TerminatorKind::Switch { targets, .. } => targets.targets(),
            TerminatorKind::Call {
                target, on_unwind, ..
            }
            | TerminatorKind::Drop {
                target, on_unwind, ..
            }
            | TerminatorKind::Assert {
                target, on_unwind, ..
            } => smallvec![*target, *on_unwind],
            TerminatorKind::Abort(..) | TerminatorKind::Return | TerminatorKind::UnwindResume => {
                smallvec![]
            }
        }
    }
    pub fn targets_mut(&mut self) -> SmallVec<[&mut BlockId; 2]> {
        match &mut self.kind {
            TerminatorKind::Goto { target } => {
                smallvec![target]
            }
            TerminatorKind::Switch { targets, .. } => targets.targets_mut(),
            TerminatorKind::Call {
                target, on_unwind, ..
            }
            | TerminatorKind::Drop {
                target, on_unwind, ..
            }
            | TerminatorKind::Assert {
                target, on_unwind, ..
            } => smallvec![target, on_unwind],
            TerminatorKind::Abort(..) | TerminatorKind::Return | TerminatorKind::UnwindResume => {
                smallvec![]
            }
        }
    }
}

impl BlockData {
    /// Build a block that's just a goto terminator.
    pub fn new_goto(span: Span, target: BlockId) -> Self {
        BlockData {
            statements: vec![],
            terminator: Terminator::goto(span, target),
        }
    }
    pub fn as_goto(&self) -> Option<BlockId> {
        if let TerminatorKind::Goto { target } = self.terminator.kind {
            Some(target)
        } else {
            None
        }
    }
    pub fn as_trivial_goto(&self) -> Option<BlockId> {
        self.as_goto().filter(|_| {
            self.statements
                .iter()
                .all(|st| matches!(st.kind, StatementKind::Nop))
        })
    }

    pub fn as_abort(&self) -> Option<AbortKind> {
        if self.statements.iter().all(|st| st.kind.is_storage_live())
            && let TerminatorKind::Abort(abort) = &self.terminator.kind
        {
            Some(abort.clone())
        } else {
            None
        }
    }

    /// Build a block that's UB to reach.
    pub fn new_unreachable() -> Self {
        Terminator::new(
            Span::dummy(),
            TerminatorKind::Abort(AbortKind::UndefinedBehavior),
        )
        .into_block()
    }

    pub fn targets(&self) -> SmallVec<[BlockId; 2]> {
        self.terminator.targets()
    }

    pub fn targets_ignoring_unwind(&self) -> SmallVec<[BlockId; 2]> {
        match &self.terminator.kind {
            TerminatorKind::Goto { target } => {
                smallvec![*target]
            }
            TerminatorKind::Switch { targets, .. } => targets.targets(),
            TerminatorKind::Call { target, .. }
            | TerminatorKind::Drop { target, .. }
            | TerminatorKind::Assert { target, .. } => {
                smallvec![*target]
            }
            TerminatorKind::Abort(..) | TerminatorKind::Return | TerminatorKind::UnwindResume => {
                smallvec![]
            }
        }
    }

    /// Apply a transformer to all the statements.
    ///
    /// The transformer should:
    /// - mutate the current statement in place
    /// - return the sequence of statements to introduce before the current statement
    pub fn transform<F: FnMut(&mut Statement) -> Vec<Statement>>(&mut self, mut f: F) {
        self.transform_sequences_fwd(|slice| {
            let new_statements = f(&mut slice[0]);
            if new_statements.is_empty() {
                vec![]
            } else {
                vec![(0, new_statements)]
            }
        });
    }

    /// Helper, see `transform_sequences_fwd` and `transform_sequences_bwd`.
    fn transform_sequences<F>(&mut self, mut f: F, forward: bool)
    where
        F: FnMut(&mut [Statement]) -> Vec<(usize, Vec<Statement>)>,
    {
        let mut to_insert = vec![];
        let mut final_len = self.statements.len();
        if forward {
            for i in 0..self.statements.len() {
                let new_to_insert = f(&mut self.statements[i..]);
                to_insert.extend(new_to_insert.into_iter().map(|(j, stmts)| {
                    final_len += stmts.len();
                    (i + j, stmts)
                }));
            }
        } else {
            for i in (0..self.statements.len()).rev() {
                let new_to_insert = f(&mut self.statements[i..]);
                to_insert.extend(new_to_insert.into_iter().map(|(j, stmts)| {
                    final_len += stmts.len();
                    (i + j, stmts)
                }));
            }
        }
        if !to_insert.is_empty() {
            to_insert.sort_by_key(|(i, _)| *i);
            // Make it so the first element is always at the end so we can pop it.
            to_insert.reverse();
            // Construct the merged list of statements.
            let old_statements = mem::replace(&mut self.statements, Vec::with_capacity(final_len));
            for (i, stmt) in old_statements.into_iter().enumerate() {
                while let Some((j, _)) = to_insert.last()
                    && *j == i
                {
                    let (_, mut stmts) = to_insert.pop().unwrap();
                    self.statements.append(&mut stmts);
                }
                self.statements.push(stmt);
            }
        }
    }

    /// Apply a transformer to all the statements.
    ///
    /// The transformer should:
    /// - mutate the current statements in place
    /// - return a list of `(i, statements)` where `statements` will be inserted before index `i`.
    pub fn transform_sequences_fwd<F>(&mut self, f: F)
    where
        F: FnMut(&mut [Statement]) -> Vec<(usize, Vec<Statement>)>,
    {
        self.transform_sequences(f, true);
    }

    /// Apply a transformer to all the statements.
    ///
    /// The transformer should:
    /// - mutate the current statements in place
    /// - return a list of `(i, statements)` where `statements` will be inserted before index `i`.
    pub fn transform_sequences_bwd<F>(&mut self, f: F)
    where
        F: FnMut(&mut [Statement]) -> Vec<(usize, Vec<Statement>)>,
    {
        self.transform_sequences(f, false);
    }
}

impl ExprBody {
    /// Returns a map from blocks in this body to their abort kind, if they correspond to an
    /// abort block (ie. a block with no statements and an [TerminatorKind::Abort] terminator).
    pub fn as_abort_map(&self) -> HashMap<BlockId, AbortKind> {
        self.body
            .iter_indexed()
            .filter_map(|(bid, block)| block.as_abort().map(|abort| (bid, abort)))
            .collect()
    }

    pub fn transform_sequences_fwd<F>(&mut self, mut f: F)
    where
        F: FnMut(BlockId, &mut Locals, &mut [Statement]) -> Vec<(usize, Vec<Statement>)>,
    {
        for (id, block) in &mut self.body.iter_mut_indexed() {
            block.transform_sequences_fwd(|seq| f(id, &mut self.locals, seq));
        }
    }

    pub fn transform_sequences_bwd<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Locals, &mut [Statement]) -> Vec<(usize, Vec<Statement>)>,
    {
        for block in &mut self.body {
            block.transform_sequences_bwd(|seq| f(&mut self.locals, seq));
        }
    }

    /// Apply a function to all the statements, in a bottom-up manner.
    pub fn visit_statements<F: FnMut(&mut Statement)>(&mut self, mut f: F) {
        for block in self.body.iter_mut().rev() {
            for st in block.statements.iter_mut().rev() {
                f(st);
            }
        }
    }
}

/// Helper to construct a small ullbc body.
pub struct BodyBuilder {
    /// The span to use for everything.
    pub span: Span,
    /// Body under construction.
    pub body: ExprBody,
    /// Block onto which we're adding statements. Its terminator is always `Return`.
    pub current_block: BlockId,
    /// Block to unwind to; created on demand.
    pub unwind_block: Option<BlockId>,
}

fn mk_block(span: Span, term: TerminatorKind) -> BlockData {
    BlockData {
        statements: vec![],
        terminator: Terminator::new(span, term),
    }
}

impl BodyBuilder {
    pub fn new(span: Span, arg_count: usize) -> Self {
        let mut body: ExprBody = GExprBody {
            span,
            locals: Locals::new(arg_count),
            bound_body_regions: 0,
            body: IndexVec::new(),
            comments: vec![],
        };
        let current_block = body.body.push(BlockData {
            statements: Default::default(),
            terminator: Terminator::new(span, TerminatorKind::Return),
        });
        Self {
            span,
            body,
            current_block,
            unwind_block: None,
        }
    }

    /// Finalize the builder by returning the built body.
    pub fn build(mut self) -> ExprBody {
        // Replace erased regions with fresh ones.
        let mut freshener: IndexMap<RegionId, ()> = IndexMap::new();
        self.body.dyn_visit_mut(|r: &mut Region| {
            if r.is_erased() || r.is_body() {
                *r = Region::Body(freshener.push(()));
            }
        });
        self.body.bound_body_regions = freshener.slot_count();
        // Return the built body.
        self.body
    }

    /// Create a new local. Adds a `StorageLive` statement if the local is not one of the special
    /// ones (return or function argument).
    pub fn new_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let place = self.body.locals.new_var(name, ty);
        let local_id = place.as_local().unwrap();
        if !self.body.locals.is_return_or_arg(local_id) {
            self.push_statement(StatementKind::StorageLive(local_id));
        }
        place
    }

    /// Helper.
    fn current_block(&mut self) -> &mut BlockData {
        &mut self.body.body[self.current_block]
    }

    pub fn push_statement(&mut self, kind: StatementKind) {
        let st = Statement::new(self.span, kind);
        self.current_block().statements.push(st);
    }

    fn unwind_block(&mut self) -> BlockId {
        *self.unwind_block.get_or_insert_with(|| {
            self.body
                .body
                .push(mk_block(self.span, TerminatorKind::UnwindResume))
        })
    }

    pub fn call(&mut self, call: Call) {
        let next_block = self
            .body
            .body
            .push(mk_block(self.span, TerminatorKind::Return));
        let term = TerminatorKind::Call {
            target: next_block,
            call,
            on_unwind: self.unwind_block(),
        };
        self.current_block().terminator.kind = term;
        self.current_block = next_block;
    }

    pub fn insert_drop(&mut self, place: Place, tref: TraitRef) {
        let next_block = self
            .body
            .body
            .push(mk_block(self.span, TerminatorKind::Return));
        let term = TerminatorKind::Drop {
            kind: DropKind::Precise,
            place: place,
            tref: tref,
            target: next_block,
            on_unwind: self.unwind_block(),
        };
        self.current_block().terminator.kind = term;
        self.current_block = next_block;
    }
}
