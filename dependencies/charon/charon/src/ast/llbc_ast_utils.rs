//! Implementations for [crate::llbc_ast]

use crate::llbc_ast::*;
use crate::meta;
use crate::meta::Span;
use derive_generic_visitor::*;
use std::mem;

/// Combine the span information from a [Switch]
pub fn combine_switch_targets_span(targets: &Switch) -> Span {
    match targets {
        Switch::If(_, st1, st2) => meta::combine_span(&st1.span, &st2.span),
        Switch::SwitchInt(_, _, branches, otherwise) => {
            let branches = branches.iter().map(|b| &b.1.span);
            let mbranches = meta::combine_span_iter(branches);
            meta::combine_span(&mbranches, &otherwise.span)
        }
        Switch::Match(_, branches, otherwise) => {
            let branches = branches.iter().map(|b| &b.1.span);
            let mbranches = meta::combine_span_iter(branches);
            if let Some(otherwise) = otherwise {
                meta::combine_span(&mbranches, &otherwise.span)
            } else {
                mbranches
            }
        }
    }
}

impl Switch {
    pub fn iter_targets(&self) -> impl Iterator<Item = &Block> {
        use itertools::Either;
        match self {
            Switch::If(_, exp1, exp2) => Either::Left([exp1, exp2].into_iter()),
            Switch::SwitchInt(_, _, targets, otherwise) => Either::Right(Either::Left(
                targets.iter().map(|(_, tgt)| tgt).chain([otherwise]),
            )),
            Switch::Match(_, targets, otherwise) => Either::Right(Either::Right(
                targets.iter().map(|(_, tgt)| tgt).chain(otherwise.as_ref()),
            )),
        }
    }

    pub fn iter_targets_mut(&mut self) -> impl Iterator<Item = &mut Block> {
        use itertools::Either;
        match self {
            Switch::If(_, exp1, exp2) => Either::Left([exp1, exp2].into_iter()),
            Switch::SwitchInt(_, _, targets, otherwise) => Either::Right(Either::Left(
                targets.iter_mut().map(|(_, tgt)| tgt).chain([otherwise]),
            )),
            Switch::Match(_, targets, otherwise) => Either::Right(Either::Right(
                targets
                    .iter_mut()
                    .map(|(_, tgt)| tgt)
                    .chain(otherwise.as_mut()),
            )),
        }
    }
}

impl StatementId {
    pub fn fresh() -> StatementId {
        use std::sync::atomic::AtomicUsize;
        use std::sync::atomic::Ordering;
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        StatementId::new(id)
    }
}

impl Statement {
    pub fn new(span: Span, content: RawStatement) -> Self {
        Statement {
            span,
            id: StatementId::fresh(),
            content,
            comments_before: vec![],
        }
    }

    pub fn into_box(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn into_block(self) -> Block {
        Block {
            span: self.span,
            statements: vec![self],
        }
    }
}

impl Block {
    pub fn from_seq(seq: Vec<Statement>) -> Option<Self> {
        if seq.is_empty() {
            None
        } else {
            let span = seq
                .iter()
                .map(|st| st.span)
                .reduce(|a, b| meta::combine_span(&a, &b))
                .unwrap();
            Some(Block {
                span,
                statements: seq,
            })
        }
    }

    pub fn merge(mut self, mut other: Self) -> Self {
        self.span = meta::combine_span(&self.span, &other.span);
        self.statements.append(&mut other.statements);
        self
    }

    pub fn then(mut self, r: Statement) -> Self {
        self.span = meta::combine_span(&self.span, &r.span);
        self.statements.push(r);
        self
    }

    pub fn then_opt(self, other: Option<Statement>) -> Self {
        if let Some(other) = other {
            self.then(other)
        } else {
            self
        }
    }

    /// Apply a function to all the statements, in a top-down manner.
    pub fn visit_statements<F: FnMut(&mut Statement)>(&mut self, f: F) {
        let _ = BlockVisitor::new(|_| {}, f).visit(self);
    }

    /// Apply a transformer to all the statements, in a bottom-up manner.
    ///
    /// The transformer should:
    /// - mutate the current statement in place
    /// - return the sequence of statements to introduce before the current statement
    pub fn transform<F: FnMut(&mut Statement) -> Vec<Statement>>(&mut self, mut f: F) {
        self.transform_sequences(|slice| f(&mut slice[0]));
    }

    /// Apply a transformer to all the statements, in a bottom-up manner. Compared to `transform`,
    /// this also gives access to the following statements if any. Statements that are not part of
    /// a sequence will be traversed as `[st]`. Statements that are will be traversed twice: once
    /// as `[st]`, and then as `[st, ..]` with the following statements if any.
    ///
    /// The transformer should:
    /// - mutate the current statements in place
    /// - return the sequence of statements to introduce before the current statements
    pub fn transform_sequences<F: FnMut(&mut [Statement]) -> Vec<Statement>>(&mut self, mut f: F) {
        self.visit_blocks_bwd(|blk: &mut Block| {
            let mut final_len = blk.statements.len();
            let mut to_insert = vec![];
            for i in (0..blk.statements.len()).rev() {
                let new_to_insert = f(&mut blk.statements[i..]);
                final_len += new_to_insert.len();
                to_insert.push((i, new_to_insert));
            }
            if !to_insert.is_empty() {
                to_insert.sort_by_key(|(i, _)| *i);
                // Make it so the first element is always at the end so we can pop it.
                to_insert.reverse();
                // Construct the merged list of statements.
                let old_statements =
                    mem::replace(&mut blk.statements, Vec::with_capacity(final_len));
                for (i, stmt) in old_statements.into_iter().enumerate() {
                    while let Some((j, _)) = to_insert.last()
                        && *j == i
                    {
                        let (_, mut stmts) = to_insert.pop().unwrap();
                        blk.statements.append(&mut stmts);
                    }
                    blk.statements.push(stmt);
                }
            }
        })
    }

    /// Visit `self` and its sub-blocks in a bottom-up (post-order) traversal.
    pub fn visit_blocks_bwd<F: FnMut(&mut Block)>(&mut self, f: F) {
        let _ = BlockVisitor::new(f, |_| {}).visit(self);
    }
}

/// Small visitor to visit statements and blocks.
#[derive(Visitor)]
pub struct BlockVisitor<F: FnMut(&mut Block), G: FnMut(&mut Statement)> {
    exit_blk: F,
    enter_stmt: G,
}

impl<F: FnMut(&mut Block), G: FnMut(&mut Statement)> BlockVisitor<F, G> {
    pub fn new(exit_blk: F, enter_stmt: G) -> Self {
        Self {
            exit_blk,
            enter_stmt,
        }
    }
}

impl<F: FnMut(&mut Block), G: FnMut(&mut Statement)> VisitBodyMut for BlockVisitor<F, G> {
    fn exit_llbc_block(&mut self, x: &mut Block) {
        (self.exit_blk)(x)
    }
    fn enter_llbc_statement(&mut self, x: &mut Statement) {
        (self.enter_stmt)(x)
    }
}
