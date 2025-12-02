//! Take all the comments found in the original body and assign them to statements.
use std::mem;

use crate::ast::*;
use crate::transform::TransformCtx;

use super::ctx::TransformPass;

trait IsStatement {
    fn get_span(&self) -> Span;
    fn get_comments_before(&mut self) -> &mut Vec<String>;
}

impl IsStatement for llbc_ast::Statement {
    fn get_span(&self) -> Span {
        self.span
    }
    fn get_comments_before(&mut self) -> &mut Vec<String> {
        &mut self.comments_before
    }
}
impl IsStatement for ullbc_ast::Statement {
    fn get_span(&self) -> Span {
        self.span
    }
    fn get_comments_before(&mut self) -> &mut Vec<String> {
        &mut self.comments_before
    }
}
impl IsStatement for ullbc_ast::Terminator {
    fn get_span(&self) -> Span {
        self.span
    }
    fn get_comments_before(&mut self) -> &mut Vec<String> {
        &mut self.comments_before
    }
}

struct CommentsCtx {
    comments: Vec<(usize, Vec<String>)>,
}
impl CommentsCtx {
    fn visit<St: IsStatement>(&mut self, st: &mut St) {
        let st_line = st.get_span().span.beg.line;
        self.comments = mem::take(&mut self.comments)
            .into_iter()
            .filter_map(|(line, comments)| {
                if line <= st_line {
                    st.get_comments_before().extend(comments);
                    None
                } else {
                    Some((line, comments))
                }
            })
            .collect();
    }
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|_ctx, fun| {
            if let Ok(body) = &mut fun.body {
                // Constraints in the ideal case:
                // - each comment should be assigned to exactly one statement;
                // - the order of comments in the source should refine the partial order of control flow;
                // - a comment should come before the statement it was applied to.

                // This is a pretty simple heuristic which is good enough for now.
                let mut ctx = CommentsCtx {
                    comments: match body {
                        Body::Unstructured(b) => b.comments.clone(),
                        Body::Structured(b) => b.comments.clone(),
                    },
                };
                match body {
                    Body::Unstructured(b) => {
                        for block in &mut b.body {
                            for st in &mut block.statements {
                                // Many assignments have a `storage_live` before them; we don't
                                // want to put the comment there.
                                if !st.content.is_storage_live() {
                                    ctx.visit(st);
                                }
                            }
                            ctx.visit(&mut block.terminator);
                        }
                    }
                    Body::Structured(b) => b.body.visit_statements(|st| {
                        if !st.content.is_storage_live() {
                            ctx.visit(st);
                        }
                    }),
                }
            }
        });
    }
}
