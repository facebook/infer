//! Desugar array/slice index operations to function calls.
use crate::llbc_ast::*;
use crate::transform::TransformCtx;
use derive_generic_visitor::*;

use super::ctx::LlbcPass;

/// We replace some place constructors with function calls. To do that, we explore all the places
/// in a body and deconstruct a given place access into intermediate assignments.
///
/// We accumulate the new assignments as statements in the visitor, and at the end we insert these
/// statements before the one that was just explored.
#[derive(Visitor)]
struct IndexVisitor<'a> {
    locals: &'a mut Locals,
    /// Statements to prepend to the statement currently being explored.
    statements: Vec<Statement>,
    // When we visit a place, we need to know if it is being accessed mutably or not. Whenever we
    // visit something that contains a place we push the relevant mutability on this stack.
    // Unfortunately this requires us to be very careful to catch all the cases where we see
    // places.
    place_mutability_stack: Vec<bool>,
    // Span of the statement.
    span: Span,
}

impl<'a> IndexVisitor<'a> {
    fn fresh_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let var = self.locals.new_var(name, ty);
        let live_kind = RawStatement::StorageLive(var.local_id().unwrap());
        self.statements.push(Statement::new(self.span, live_kind));
        var
    }

    fn transform_place(&mut self, mut_access: bool, place: &mut Place) {
        use ProjectionElem::*;
        let Some((subplace, pe @ (Index { .. } | Subslice { .. }))) = place.as_projection() else {
            return;
        };
        let tref = subplace.ty.as_adt().unwrap();
        let builtin_ty = tref.id.as_builtin().unwrap();

        // The built-in function to call.
        let indexing_function = {
            let builtin_fun = BuiltinFunId::Index(BuiltinIndexOp {
                is_array: matches!(builtin_ty, BuiltinTy::Array),
                mutability: RefKind::mutable(mut_access),
                is_range: matches!(pe, Subslice { .. }),
            });
            // Same generics as the array/slice type, except for the extra lifetime.
            let mut generics = tref.generics.clone();
            generics.regions = [Region::Erased].into();
            FnOperand::Regular(FnPtr {
                func: Box::new(FunIdOrTraitMethodRef::mk_builtin(builtin_fun)),
                generics,
            })
        };

        let input_ty = TyKind::Ref(
            Region::Erased,
            subplace.ty().clone(),
            RefKind::mutable(mut_access),
        )
        .into_ty();

        let elem_ty = tref.generics.types[0].clone();
        let output_inner_ty = if matches!(pe, Index { .. }) {
            elem_ty
        } else {
            TyKind::Adt(TypeDeclRef {
                id: TypeId::Builtin(BuiltinTy::Slice),
                generics: Box::new(GenericArgs::new_for_builtin(vec![elem_ty].into())),
            })
            .into_ty()
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
        let input_var = {
            let input_var = self.fresh_var(None, input_ty);
            let kind = RawStatement::Assign(
                input_var.clone(),
                Rvalue::Ref(subplace.clone(), BorrowKind::mutable(mut_access)),
            );
            self.statements.push(Statement::new(self.span, kind));
            input_var
        };

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
        if from_end {
            // `storage_live(len_var)`
            // `len_var = len(p)`
            let usize_ty = TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)).into_ty();
            let len_var = self.fresh_var(None, usize_ty.clone());
            let kind = RawStatement::Assign(
                len_var.clone(),
                Rvalue::Len(
                    subplace.clone(),
                    subplace.ty().clone(),
                    tref.generics.const_generics.get(0.into()).cloned(),
                ),
            );
            self.statements.push(Statement::new(self.span, kind));

            // `storage_live(index_var)`
            // `index_var = len_var - last_arg`
            // `storage_dead(len_var)`
            let index_var = self.fresh_var(None, usize_ty);
            let kind = RawStatement::Assign(
                index_var.clone(),
                Rvalue::BinaryOp(
                    BinOp::Sub(OverflowMode::UB),
                    Operand::Copy(len_var.clone()),
                    last_arg,
                ),
            );
            self.statements.push(Statement::new(self.span, kind));
            let dead_kind = RawStatement::StorageDead(len_var.local_id().unwrap());
            self.statements.push(Statement::new(self.span, dead_kind));
            args.push(Operand::Copy(index_var));
        } else {
            args.push(last_arg);
        }

        // Call the indexing function:
        // `storage_live(tmp1)`
        // `tmp1 = {Array,Slice}{Mut,Shared}{Index,SubSlice}(move tmp0, <other args>)`
        let output_var = {
            let output_var = self.fresh_var(None, output_ty);
            let index_call = Call {
                func: indexing_function,
                args,
                dest: output_var.clone(),
            };
            let kind = RawStatement::Call(index_call);
            self.statements.push(Statement::new(self.span, kind));
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
impl VisitBodyMut for IndexVisitor<'_> {
    /// We explore places from the inside-out.
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
            FnOperand::Move(_) => self.visit_inner_with_mutability(x, true),
        }
    }

    fn visit_rvalue(&mut self, x: &mut Rvalue) -> ControlFlow<Infallible> {
        use Rvalue::*;
        match x {
            // `UniqueImmutable` de facto gives mutable access and only shows up if there is nested
            // mutable access.
            RawPtr(_, RefKind::Mut)
            | Ref(_, BorrowKind::Mut | BorrowKind::TwoPhaseMut | BorrowKind::UniqueImmutable) => {
                self.visit_inner_with_mutability(x, true)
            }
            RawPtr(_, RefKind::Shared)
            | Ref(_, BorrowKind::Shared | BorrowKind::Shallow)
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

pub struct Transform;

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
impl LlbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, b: &mut ExprBody) {
        b.body.transform(|st: &mut Statement| {
            let mut visitor = IndexVisitor {
                locals: &mut b.locals,
                statements: Vec::new(),
                place_mutability_stack: Vec::new(),
                span: st.span,
            };
            use RawStatement::*;
            match &mut st.content {
                Assign(..)
                | SetDiscriminant(..)
                | CopyNonOverlapping(_)
                | Drop(..)
                | Deinit(..)
                | Call(..) => {
                    let _ = visitor.visit_inner_with_mutability(st, true);
                }
                Switch(..) => {
                    let _ = visitor.visit_inner_with_mutability(st, false);
                }
                Nop | Error(..) | Assert(..) | Abort(..) | StorageDead(..) | StorageLive(..)
                | Return | Break(..) | Continue(..) | Loop(..) => {
                    let _ = st.drive_body_mut(&mut visitor);
                }
            }
            visitor.statements
        });
    }
}
