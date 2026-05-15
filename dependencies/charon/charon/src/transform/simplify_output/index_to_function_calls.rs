//! Desugar array/slice index operations to function calls.

use crate::llbc_ast::*;
use crate::transform::TransformCtx;
use crate::transform::ctx::{BodyTransformCtx, LlbcStatementTransformCtx};
use derive_generic_visitor::*;

use crate::transform::ctx::LlbcPass;

/// We replace some place constructors with function calls. To do that, we explore all the places
/// in a body and deconstruct a given place access into intermediate assignments.
///
/// We accumulate the new assignments as statements in the visitor, and at the end we insert these
/// statements before the one that was just explored.
#[derive(Visitor)]
struct IndexVisitor<'a, 'b> {
    ctx: &'b mut LlbcStatementTransformCtx<'a>,
    // When we visit a place, we need to know if it is being accessed mutably or not. Whenever we
    // visit something that contains a place we push the relevant mutability on this stack.
    // Unfortunately this requires us to be very careful to catch all the cases where we see
    // places.
    place_mutability_stack: Vec<bool>,
}

impl<'a, 'b> IndexVisitor<'a, 'b> {
    /// transform `place: subplace[i]` into indexing function calls for `subplace` and `i`
    fn transform_place(&mut self, mut_access: bool, place: &mut Place) {
        use ProjectionElem::*;
        // This function is naturally called recusively, so `subplace` cannot be another `Index` or `Subslice`.
        // Hence, `subplace`, if still projecting, must be either a `Deref` or a `Field`.
        let Some((subplace, pe @ (Index { .. } | Subslice { .. }))) = place.as_projection() else {
            return;
        };

        let (ty, len) = match subplace.ty.kind() {
            TyKind::Array(ty, len) => (ty.clone(), Some(len.clone())),
            TyKind::Slice(ty) => (ty.clone(), None),
            _ => unreachable!("Indexing can only be done on arrays or slices"),
        };

        // The built-in function to call.
        let indexing_function = {
            let builtin_fun = BuiltinFunId::Index(BuiltinIndexOp {
                is_array: subplace.ty.kind().is_array(),
                mutability: RefKind::mutable(mut_access),
                is_range: pe.is_subslice(),
            });
            // Same generics as the array/slice type, except for the extra lifetime.
            let generics = GenericArgs {
                types: [ty.clone()].into(),
                const_generics: len.map(|l| [*l].into()).unwrap_or_default(),
                regions: [Region::Erased].into(),
                trait_refs: [].into(),
            };
            FnOperand::Regular(FnPtr::new(FnPtrKind::mk_builtin(builtin_fun), generics))
        };

        let output_inner_ty = if matches!(pe, Index { .. }) {
            ty
        } else {
            TyKind::Slice(ty).into_ty()
        };
        let output_ty = {
            TyKind::Ref(
                Region::Erased,
                output_inner_ty.clone(),
                RefKind::mutable(mut_access),
            )
            .into_ty()
        };

        // Push the statements:
        // `storage_live(tmp0)`
        // `tmp0 = &{mut}p`
        let input_var =
            self.ctx
                .borrow_to_new_var(subplace.clone(), BorrowKind::mutable(mut_access), None);

        // Construct the arguments to pass to the indexing function.
        let mut args = vec![Operand::Move(input_var)];
        if let Subslice { from, .. } = &pe {
            args.push(from.as_ref().clone());
        }
        let (last_arg, from_end) = match &pe {
            Index {
                offset: x,
                from_end,
                ..
            }
            | Subslice {
                to: x, from_end, ..
            } => (x.as_ref().clone(), *from_end),
            _ => unreachable!(),
        };
        let to_idx = self
            .ctx
            .compute_subslice_end_idx(subplace, last_arg, from_end);
        args.push(to_idx);

        // Call the indexing function:
        // `storage_live(tmp1)`
        // `tmp1 = {Array,Slice}{Mut,Shared}{Index,SubSlice}(move tmp0, <other args>)`
        let output_var = {
            let output_var = self.ctx.fresh_var(None, output_ty);
            let index_call = Call {
                func: indexing_function,
                args,
                dest: output_var.clone(),
            };
            let kind = StatementKind::Call(index_call);
            self.ctx
                .statements
                .push(Statement::new(self.ctx.span, kind));
            output_var
        };

        // Update the place.
        *place = output_var.project(ProjectionElem::Deref, output_inner_ty);
    }

    /// Calls `self.visit_inner()` with `mutability` pushed on the stack.
    fn visit_inner_with_mutability<T>(
        &mut self,
        x: &mut T,
        mutability: bool,
    ) -> ControlFlow<Infallible>
    where
        T: for<'s> DriveMut<'s, BodyVisitableWrapper<Self>> + BodyVisitable,
    {
        self.place_mutability_stack.push(mutability);
        self.visit_inner(x)?;
        self.place_mutability_stack.pop();
        Continue(())
    }
}

/// The visitor methods.
impl VisitBodyMut for IndexVisitor<'_, '_> {
    /// We explore places from the inside-out --- recursion naturally happens here.
    fn exit_place(&mut self, place: &mut Place) {
        // We have intercepted every traversal that would reach a place and pushed the correct
        // mutability on the stack.
        let mut_access = *self.place_mutability_stack.last().unwrap();
        self.transform_place(mut_access, place);
    }

    fn visit_operand(&mut self, x: &mut Operand) -> ControlFlow<Infallible> {
        match x {
            Operand::Move(_) => self.visit_inner_with_mutability(x, true),
            Operand::Copy(_) => self.visit_inner_with_mutability(x, false),
            Operand::Const(..) => self.visit_inner(x),
        }
    }

    fn visit_call(&mut self, x: &mut Call) -> ControlFlow<Infallible> {
        self.visit_inner_with_mutability(x, true)
    }

    fn visit_fn_operand(&mut self, x: &mut FnOperand) -> ControlFlow<Infallible> {
        match x {
            FnOperand::Regular(_) => self.visit_inner(x),
            FnOperand::Dynamic(_) => self.visit_inner_with_mutability(x, true),
        }
    }

    fn visit_rvalue(&mut self, x: &mut Rvalue) -> ControlFlow<Infallible> {
        use Rvalue::*;
        match x {
            // `UniqueImmutable` de facto gives mutable access and only shows up if there is nested
            // mutable access.
            RawPtr {
                kind: RefKind::Mut, ..
            }
            | Ref {
                kind: BorrowKind::Mut | BorrowKind::TwoPhaseMut | BorrowKind::UniqueImmutable,
                ..
            } => self.visit_inner_with_mutability(x, true),
            RawPtr {
                kind: RefKind::Shared,
                ..
            }
            | Ref {
                kind: BorrowKind::Shared | BorrowKind::Shallow,
                ..
            }
            | Discriminant(..)
            | Len(..) => self.visit_inner_with_mutability(x, false),

            Use(_) | NullaryOp(..) | UnaryOp(..) | BinaryOp(..) | Aggregate(..) | Repeat(..)
            | ShallowInitBox(..) => self.visit_inner(x),
        }
    }

    fn visit_llbc_block(&mut self, _: &mut llbc_ast::Block) -> ControlFlow<Infallible> {
        ControlFlow::Continue(())
    }
}

/// We do the following.
///
/// If `p` is a projection (for instance: `var`, `*var`, `var.f`, etc.), we
/// detect:
/// - whether it operates on a slice or an array (we keep track of the types)
/// - whether the access might mutate the value or not (it is
///   the case if it is in a `move`, `&mut` or at the lhs of an assignment),
///   and do the following transformations
///
/// ```text
///   // If array and mutable access:
///   ... p[i] ...
///      ~~>
///   tmp0 = &mut p
///   tmp1 = ArrayIndexMut(move p, i)
///   ... *tmp1 ...
///
///   // If array and non-mutable access:
///   ... p[i] ...
///      ~~>
///   tmp0 := & p
///   tmp1 := ArrayIndexShared(move tmp0, i)
///   ... *tmp1 ...
///
///   // Omitting the slice cases, which are similar
/// ```
///
/// For instance, it leads to the following transformations:
/// ```text
///   // x : [u32; N]
///   y : u32 = copy x[i]
///      ~~>
///   tmp0 : & [u32; N] := &x
///   tmp1 : &u32 = ArrayIndexShared(move tmp0, i)
///   y : u32 = copy (*tmp1)
///
///   // x : &[T; N]
///   y : &T = & (*x)[i]
///      ~~>
///   tmp0 : & [T; N] := & (*x)
///   tmp1 : &T = ArrayIndexShared(move tmp0, i)
///   y : &T = & (*tmp1)
///
///   // x : [u32; N]
///   y = &mut x[i]
///      ~~>
///   tmp0 : &mut [u32; N] := &mut x
///   tmp1 : &mut u32 := ArrayIndexMut(move tmp0, i)
///   y = &mut (*tmp)
///
///   // When using an index on the lhs:
///   // y : [T; N]
///   y[i] = x
///      ~~>
///   tmp0 : &mut [T; N] := &mut y;
///   tmp1 : &mut T = ArrayIndexMut(move y, i)
///   *tmp1 = x
/// ```
pub struct Transform;
impl LlbcPass for Transform {
    fn should_run(&self, options: &crate::options::TranslateOptions) -> bool {
        options.index_to_function_calls
    }

    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        decl.transform_llbc_statements(ctx, |ctx, st: &mut Statement| {
            let mut visitor = IndexVisitor {
                ctx,
                place_mutability_stack: Vec::new(),
            };
            use StatementKind::*;
            match &mut st.kind {
                Assign(..) | SetDiscriminant(..) | CopyNonOverlapping(_) | Drop(..) | Call(..) => {
                    let _ = visitor.visit_inner_with_mutability(st, true);
                }
                Switch(..) | PlaceMention(..) => {
                    let _ = visitor.visit_inner_with_mutability(st, false);
                }
                Nop
                | Error(..)
                | Assert { .. }
                | Abort(..)
                | StorageDead(..)
                | StorageLive(..)
                | Return
                | Break(..)
                | Continue(..)
                | Loop(..) => {
                    let _ = st.drive_body_mut(&mut visitor);
                }
            }
        })
    }
}
