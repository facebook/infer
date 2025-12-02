//! The MIR code often contains variables with type `Never`, and we want to get
//! rid of those. We proceed in two steps. First, we remove the instructions
//! `drop(v)` where `v` has type `Never` (it can happen - this module does the
//! filtering). Then, we filter the unused variables ([crate::transform::remove_unused_locals]).

use crate::errors::register_error;
use crate::formatter::IntoFormatter;
use crate::llbc_ast::*;
use crate::name_matcher::NamePattern;
use crate::pretty::FmtWithCtx;
use crate::transform::TransformCtx;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use super::ctx::LlbcPass;

pub struct Transform;
impl Transform {
    fn update_block(
        ctx: &mut TransformCtx,
        block: &mut Block,
        discriminant_intrinsic: Option<FunDeclId>,
    ) {
        // Iterate through the statements.
        for i in 0..block.statements.len() {
            let suffix = &mut block.statements[i..];
            match suffix {
                [
                    Statement {
                        content: RawStatement::Assign(dest, Rvalue::Discriminant(p)),
                        ..
                    },
                    rest @ ..,
                ] => {
                    // The destination should be a variable
                    assert!(dest.is_local());
                    let TyKind::Adt(tdecl_ref) = p.ty().kind() else {
                        continue;
                    };
                    let TypeId::Adt(adt_id) = tdecl_ref.id else {
                        continue;
                    };

                    // Lookup the type of the scrutinee
                    let tkind = ctx.translated.type_decls.get(adt_id).map(|x| &x.kind);
                    let Some(TypeDeclKind::Enum(variants)) = tkind else {
                        match tkind {
                            // This can happen if the type was declared as invisible or opaque.
                            None | Some(TypeDeclKind::Opaque) => {
                                let name = ctx.translated.item_name(adt_id).unwrap();
                                register_error!(
                                    ctx,
                                    block.span,
                                    "reading the discriminant of an opaque enum. \
                                    Add `--include {}` to the `charon` arguments \
                                    to translate this enum.",
                                    name.with_ctx(&ctx.into_fmt())
                                );
                            }
                            // Don't double-error
                            Some(TypeDeclKind::Error(..)) => {}
                            Some(_) => {
                                register_error!(
                                    ctx,
                                    block.span,
                                    "reading the discriminant of a non-enum type"
                                );
                            }
                        }
                        block.statements[i].content = RawStatement::Error(
                            "error reading the discriminant of this type".to_owned(),
                        );
                        return;
                    };

                    // We look for a `SwitchInt` just after the discriminant read.
                    match rest {
                        [
                            Statement {
                                content:
                                    RawStatement::Switch(
                                        switch @ Switch::SwitchInt(Operand::Move(_), ..),
                                    ),
                                ..
                            },
                            ..,
                        ] => {
                            // Convert between discriminants and variant indices. Remark: the discriminant can
                            // be of any *signed* integer type (`isize`, `i8`, etc.).
                            let discr_to_id: HashMap<ScalarValue, VariantId> = variants
                                .iter_indexed_values()
                                .map(|(id, variant)| (variant.discriminant, id))
                                .collect();

                            take_mut::take(switch, |switch| {
                                let (Operand::Move(op_p), _, targets, otherwise) =
                                    switch.to_switch_int().unwrap()
                                else {
                                    unreachable!()
                                };
                                assert!(op_p.is_local() && op_p.local_id() == dest.local_id());

                                let mut covered_discriminants: HashSet<ScalarValue> =
                                    HashSet::default();
                                let targets = targets
                                    .into_iter()
                                    .map(|(v, e)| {
                                        let targets = v
                                            .into_iter()
                                            .filter_map(|discr| {
                                                covered_discriminants.insert(discr);
                                                discr_to_id.get(&discr).or_else(|| {
                                                    register_error!(
                                                        ctx,
                                                        block.span,
                                                        "Found incorrect discriminant \
                                                        {discr} for enum {adt_id}"
                                                    );
                                                    None
                                                })
                                            })
                                            .copied()
                                            .collect_vec();
                                        (targets, e)
                                    })
                                    .collect_vec();
                                // Filter the otherwise branch if it is not necessary.
                                let covers_all = covered_discriminants.len() == discr_to_id.len();
                                let otherwise = if covers_all { None } else { Some(otherwise) };

                                // Replace the old switch with a match.
                                Switch::Match(p.clone(), targets, otherwise)
                            });
                            // `Nop` the discriminant read.
                            block.statements[i].content = RawStatement::Nop;
                        }
                        _ => {
                            // The discriminant read is not followed by a `SwitchInt`. This can happen
                            // in optimized MIR.
                            continue;
                        }
                    }
                }
                // Replace calls of `core::intrinsics::discriminant_value` on a known enum with the
                // appropriate MIR.
                [
                    Statement {
                        content: RawStatement::Call(call),
                        ..
                    },
                    ..,
                ] if let Some(discriminant_intrinsic) = discriminant_intrinsic
                        // Detect a call to the intrinsic...
                        && let FnOperand::Regular(fn_ptr) = &call.func
                        && let FunIdOrTraitMethodRef::Fun(FunId::Regular(fun_id)) = fn_ptr.func.as_ref()
                        && *fun_id == discriminant_intrinsic
                        // on a known enum...
                        && let ty = &fn_ptr.generics.types[0]
                        && let TyKind::Adt(ty_ref) = ty.kind()
                        && let TypeId::Adt(type_id) = ty_ref.id
                        && let Some(TypeDeclKind::Enum(_)) =
                            ctx.translated.type_decls.get(type_id).map(|x| &x.kind)
                        // passing it a reference.
                        && let Operand::Move(p) = &call.args[0]
                        && let TyKind::Ref(_, sub_ty, _) = p.ty().kind() =>
                {
                    let p = p.clone().project(ProjectionElem::Deref, sub_ty.clone());
                    block.statements[i].content =
                        RawStatement::Assign(call.dest.clone(), Rvalue::Discriminant(p.clone()))
                }
                _ => {}
            }
        }
    }
}

const DISCRIMINANT_INTRINSIC: &str = "core::intrinsics::discriminant_value";

impl LlbcPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let pat = NamePattern::parse(DISCRIMINANT_INTRINSIC).unwrap();
        let discriminant_intrinsic: Option<FunDeclId> = ctx
            .translated
            .item_names
            .iter()
            .find(|(_, name)| pat.matches(&ctx.translated, name))
            .and_then(|(id, _)| id.as_fun())
            .copied();

        ctx.for_each_fun_decl(|ctx, decl| {
            if let Ok(body) = &mut decl.body {
                let body = body.as_structured_mut().unwrap();
                self.log_before_body(ctx, &decl.item_meta.name, Ok(&*body));
                body.body.visit_blocks_bwd(|block: &mut Block| {
                    Transform::update_block(ctx, block, discriminant_intrinsic);
                });
            };
        });
    }
}
