//! Copies of the relevant `MIR` types. MIR represents a rust (function) body as a CFG. It's a
//! semantically rich representation that contains no high-level control-flow operations like loops
//! or patterns; instead the control flow is entirely described by gotos and switches on integer
//! values.
use crate::prelude::*;
use rustc_middle::{mir, ty};

sinto_reexport!(rustc_abi::FieldIdx);

impl<S> SInto<S, u64> for rustc_middle::mir::interpret::AllocId {
    fn sinto(&self, _: &S) -> u64 {
        self.0.get()
    }
}

pub fn name_of_local(
    local: rustc_middle::mir::Local,
    var_debug_info: &Vec<mir::VarDebugInfo>,
) -> Option<String> {
    var_debug_info
        .iter()
        .find(|info| {
            if let mir::VarDebugInfoContents::Place(place) = info.value {
                place.projection.is_empty() && place.local == local
            } else {
                false
            }
        })
        .map(|dbg| dbg.name.to_string())
}

/// Enumerates the kinds of Mir bodies. TODO: use const generics
/// instead of an open list of types.
pub mod mir_kinds {
    #[derive(Clone, Copy, Debug)]
    pub struct Optimized;

    #[derive(Clone, Copy, Debug)]
    pub struct CTFE;

    /// MIR of unknown origin. `body()` returns `None`; this is used to get the bodies provided via
    /// `from_mir` but not attempt to get MIR for functions etc.
    #[derive(Clone, Copy, Debug)]
    pub struct Unknown;

    pub use rustc::*;
    mod rustc {
        use super::*;
        use rustc_middle::mir::Body;
        use rustc_middle::ty::TyCtxt;
        use rustc_span::def_id::DefId;

        pub trait IsMirKind: Clone + std::fmt::Debug {
            // CPS to deal with stealable bodies cleanly.
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T>;
        }

        impl IsMirKind for Optimized {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                tcx.is_mir_available(id).then(|| f(tcx.optimized_mir(id)))
            }
        }

        impl IsMirKind for CTFE {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                (!tcx.is_trivial_const(id)).then(|| f(tcx.mir_for_ctfe(id)))
            }
        }

        impl IsMirKind for Unknown {
            fn get_mir<'tcx, T>(
                _tcx: TyCtxt<'tcx>,
                _id: DefId,
                _f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                None
            }
        }
    }
}

pub use mir_kinds::IsMirKind;

/// The contents of `Operand::Const`.

#[derive(Clone, Debug)]
pub struct ConstOperand {
    pub span: Span,
    pub ty: Ty,
    pub kind: ConstOperandKind,
}

#[derive(Clone, Debug)]
pub enum ConstOperandKind {
    /// An evaluated constant represented as an expression.
    Value(ConstantExpr),
    /// Part of a MIR body that was promoted to be a constant. May not be evaluatable because of
    /// generics.
    /// It's a reference to the `DefId` of the constant. Note that rustc does not give a `DefId` to
    /// promoted constants, but we do in hax.
    Promoted(ItemRef),
}

impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, ConstOperand> for mir::ConstOperand<'tcx> {
    fn sinto(&self, s: &S) -> ConstOperand {
        let kind = translate_mir_const(s, self.span, self.const_);
        ConstOperand {
            span: self.span.sinto(s),
            ty: self.const_.ty().sinto(s),
            kind,
        }
    }
}

/// Retrieve the MIR for a promoted body.
pub fn get_promoted_mir<'tcx>(
    tcx: ty::TyCtxt<'tcx>,
    def_id: RDefId,
    promoted_id: mir::Promoted,
) -> mir::Body<'tcx> {
    if let Some(local_def_id) = def_id.as_local() {
        let (_, promoteds) = tcx.mir_promoted(local_def_id);
        if !promoteds.is_stolen() {
            promoteds.borrow()[promoted_id].clone()
        } else {
            tcx.promoted_mir(def_id)[promoted_id].clone()
        }
    } else {
        tcx.promoted_mir(def_id)[promoted_id].clone()
    }
}

/// Translate a MIR constant.
fn translate_mir_const<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    span: rustc_span::Span,
    konst: mir::Const<'tcx>,
) -> ConstOperandKind {
    use ConstOperandKind::{Promoted, Value};
    use rustc_middle::mir::Const;
    let tcx = s.base().tcx;
    match konst {
        Const::Val(const_value, ty) => {
            let evaluated = const_value_to_constant_expr(s, ty, const_value, span);
            match evaluated.report_err() {
                Ok(val) => Value(val),
                Err(err) => {
                    warning!(
                        s[span], "Couldn't convert constant back to an expression";
                        {const_value, ty, err}
                    );
                    Value(
                        ConstantExprKind::Todo("ConstEvalVal".into())
                            .decorate(ty.sinto(s), span.sinto(s)),
                    )
                }
            }
        }
        Const::Ty(_ty, c) => Value(c.sinto(s)),
        Const::Unevaluated(ucv, _) => {
            match ucv.promoted {
                Some(promoted) => {
                    let def_id = ucv.def.sinto(s).make_promoted_child(s, promoted);
                    // The def_id is not the real one: we don't want trait resolution to happen.
                    let item = ItemRef::translate_maybe_resolve_impl(s, false, def_id, ucv.args);
                    assert!(item.in_trait.is_none());
                    Promoted(item)
                }
                None => {
                    let ucv = ucv.shrink();
                    // We go through a `ty::Const`. This loses info that `ValTree`s don't capture
                    // such as data in padding bytes.
                    let val = ty::Const::new(tcx, ty::ConstKind::Unevaluated(ucv)).sinto(s);
                    Value(val)
                }
            }
        }
    }
}
