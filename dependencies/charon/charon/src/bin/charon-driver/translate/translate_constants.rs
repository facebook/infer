//! Functions to translate constants to LLBC.
use rustc_middle::ty;

use crate::translate::translate_bodies::translate_variant_id;

use super::translate_ctx::*;
use charon_lib::ast::*;

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    fn translate_constant_literal_to_constant_expr_kind(
        &mut self,
        _span: Span,
        v: &hax::ConstantLiteral,
    ) -> Result<ConstantExprKind, Error> {
        let lit = match v {
            hax::ConstantLiteral::ByteStr(bs) => Literal::ByteStr(bs.clone()),
            hax::ConstantLiteral::Str(str) => {
                // We should only get here if we actually want to translate the data
                // backing the string, when we represent strings as unsized [u8]s
                assert!(self.t_ctx.options.unsized_strings);

                let str_bytes = str.as_bytes();
                return Ok(ConstantExprKind::RawMemory(
                    str_bytes.iter().map(|b| Byte::Value(*b)).collect(),
                ));
            }
            hax::ConstantLiteral::Char(c) => Literal::Char(*c),
            hax::ConstantLiteral::Bool(b) => Literal::Bool(*b),
            hax::ConstantLiteral::Int(i) => {
                use hax::ConstantInt;
                let scalar = match i {
                    ConstantInt::Int(v, int_type) => {
                        let ty = Self::translate_hax_int_ty(int_type);
                        ScalarValue::Signed(ty, *v)
                    }
                    ConstantInt::Uint(v, uint_type) => {
                        let ty = Self::translate_hax_uint_ty(uint_type);
                        ScalarValue::Unsigned(ty, *v)
                    }
                };
                Literal::Scalar(scalar)
            }
            hax::ConstantLiteral::Float(value, float_type) => {
                let value = value.clone();
                let ty = match float_type {
                    hax::FloatTy::F16 => FloatTy::F16,
                    hax::FloatTy::F32 => FloatTy::F32,
                    hax::FloatTy::F64 => FloatTy::F64,
                    hax::FloatTy::F128 => FloatTy::F128,
                };
                Literal::Float(FloatValue { value, ty })
            }
            hax::ConstantLiteral::PtrNoProvenance(v) => {
                return Ok(ConstantExprKind::PtrNoProvenance(*v));
            }
        };
        Ok(ConstantExprKind::Literal(lit))
    }

    /// Remark: [hax::ConstantExpr] contains span information, but it is often
    /// the default span (i.e., it is useless), hence the additional span argument.
    /// TODO: the user_ty might be None because hax doesn't extract it (because
    /// we are translating a [ConstantExpr] instead of a Constant. We need to
    /// update hax.
    pub(crate) fn translate_constant_expr(
        &mut self,
        span: Span,
        v: &hax::ConstantExpr,
    ) -> Result<ConstantExpr, Error> {
        let ty = self.translate_ty(span, &v.ty)?;
        let kind = match v.contents.as_ref() {
            hax::ConstantExprKind::Literal(lit) => {
                self.translate_constant_literal_to_constant_expr_kind(span, lit)?
            }
            hax::ConstantExprKind::Adt { kind, fields } => {
                let fields: Vec<ConstantExpr> = fields
                    .iter()
                    .map(|f| self.translate_constant_expr(span, &f.value))
                    .try_collect()?;
                use hax::VariantKind;
                let vid = if let VariantKind::Enum { index, .. } = *kind {
                    Some(translate_variant_id(index))
                } else {
                    None
                };
                ConstantExprKind::Adt(vid, fields)
            }
            hax::ConstantExprKind::Array { fields } => {
                let fields: Vec<ConstantExpr> = fields
                    .iter()
                    .map(|x| self.translate_constant_expr(span, x))
                    .try_collect()?;

                ConstantExprKind::Array(fields)
            }
            hax::ConstantExprKind::Tuple { fields } => {
                let fields: Vec<ConstantExpr> = fields
                    .iter()
                    // TODO: the user_ty is not always None
                    .map(|f| self.translate_constant_expr(span, f))
                    .try_collect()?;
                ConstantExprKind::Adt(None, fields)
            }
            hax::ConstantExprKind::NamedGlobal(item) => match &item.in_trait {
                Some(impl_expr) => {
                    let trait_ref = self.translate_trait_impl_expr(span, impl_expr)?;
                    // Trait consts can't have their own generics.
                    assert!(item.generic_args.is_empty());
                    let name = self.translate_trait_item_name(&item.def_id)?;
                    ConstantExprKind::TraitConst(trait_ref, name)
                }
                None => {
                    let global_ref = self.translate_global_decl_ref(span, item)?;
                    ConstantExprKind::Global(global_ref)
                }
            },
            hax::ConstantExprKind::Borrow(v)
                if let hax::ConstantExprKind::Literal(hax::ConstantLiteral::Str(s)) =
                    v.contents.as_ref()
                    && !self.t_ctx.options.unsized_strings =>
            {
                ConstantExprKind::Literal(Literal::Str(s.clone()))
            }

            hax::ConstantExprKind::Borrow(v) => {
                let mut val = self.translate_constant_expr(span, v)?;
                let metadata = match (v.contents.as_ref(), val.ty.kind()) {
                    (hax::ConstantExprKind::Array { fields }, TyKind::Slice(subty)) => {
                        let len = ConstantExpr::mk_usize(ScalarValue::Unsigned(
                            UIntTy::Usize,
                            fields.len() as u128,
                        ));
                        // the sub-constant is an array, that has it's reference unsized
                        val.ty = Ty::mk_array(subty.clone(), len.clone());
                        Some(UnsizingMetadata::Length(Box::new(len)))
                    }

                    (hax::ConstantExprKind::Literal(hax::ConstantLiteral::Str(s)), _) => {
                        let len = ConstantExpr::mk_usize(ScalarValue::Unsigned(
                            UIntTy::Usize,
                            s.len() as u128,
                        ));
                        // the sub-constant is an array, that has it's reference unsized
                        let subty = TyKind::Literal(LiteralTy::UInt(UIntTy::U8)).into();
                        val.ty = Ty::mk_array(subty, len.clone());
                        Some(UnsizingMetadata::Length(Box::new(len)))
                    }

                    _ => None,
                };
                ConstantExprKind::Ref(Box::new(val), metadata)
            }
            hax::ConstantExprKind::Cast { .. } => {
                register_error!(
                    self,
                    span,
                    "Unsupported constant: `ConstantExprKind::Cast {{..}}`",
                );
                ConstantExprKind::Opaque("`ConstantExprKind::Cast {{..}}`".into())
            }
            hax::ConstantExprKind::RawBorrow { mutability, arg } => {
                let arg = self.translate_constant_expr(span, arg)?;
                let rk = RefKind::mutable(mutability.is_mut());
                ConstantExprKind::Ptr(rk, Box::new(arg), None)
            }
            hax::ConstantExprKind::ConstRef { id } => {
                match self.lookup_const_generic_var(span, id) {
                    Ok(var) => ConstantExprKind::Var(var),
                    Err(err) => ConstantExprKind::Opaque(err.msg),
                }
            }
            hax::ConstantExprKind::FnDef(item) => {
                let fn_ptr = self.translate_fn_ptr(span, item, TransItemSourceKind::Fun)?;
                ConstantExprKind::FnDef(fn_ptr)
            }
            hax::ConstantExprKind::FnPtr(item) => {
                let fn_ptr = self.translate_fn_ptr(span, item, TransItemSourceKind::Fun)?;
                ConstantExprKind::FnPtr(fn_ptr)
            }
            hax::ConstantExprKind::Memory(bytes) => {
                ConstantExprKind::RawMemory(bytes.iter().map(|b| Byte::Value(*b)).collect())
            }
            hax::ConstantExprKind::Todo(msg) => {
                register_error!(self, span, "Unsupported constant: {:?}", msg);
                ConstantExprKind::Opaque(msg.into())
            }
        };

        Ok(ConstantExpr { kind, ty })
    }

    pub(crate) fn translate_ty_constant_expr(
        &mut self,
        span: Span,
        c: &ty::Const<'tcx>,
    ) -> Result<ConstantExpr, Error> {
        let c = self.catch_sinto(span, c)?;
        self.translate_constant_expr(span, &c)
    }
}
