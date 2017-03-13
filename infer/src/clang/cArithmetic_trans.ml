(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module for translating unary and binary operations and compound assignments *)

(* Returns the translation of assignment when ARC mode is enabled in Obj-C *)
(* For __weak and __unsafe_unretained the translation is the same as non-ARC *)
(* (this is because, in these cases, there is no change in the reference counter *)
(* of the pointee).*)
(* The difference is when the lvalue is a __strong or __autoreleasing. In those*)
(* case we need to add proper retain/release.*)
(* See document: "Objective-C Automatic Reference Counting" describing the semantics *)
let assignment_arc_mode e1 typ e2 loc rhs_owning_method is_e1_decl =
  let assign = Sil.Store (e1, typ, e2, loc) in
  let retain_pname = BuiltinDecl.__objc_retain in
  let release_pname = BuiltinDecl.__objc_release in
  let autorelease_pname = BuiltinDecl.__set_autorelease_attribute in
  let mk_call procname e t =
    let bi_retain = Exp.Const (Const.Cfun procname) in
    Sil.Call (None, bi_retain, [(e, t)], loc, CallFlags.default) in
  match typ with
  | Typ.Tptr (_, Typ.Pk_pointer) when not rhs_owning_method && not is_e1_decl ->
      (* for __strong e1 = e2 the semantics is*)
      (* retain(e2); tmp=e1; e1=e2; release(tmp); *)
      let retain = mk_call retain_pname e2 typ in
      let id = Ident.create_fresh Ident.knormal in
      let tmp_assign = Sil.Load (id, e1, typ, loc) in
      let release = mk_call release_pname (Exp.Var id) typ in
      (e1,[retain; tmp_assign; assign; release])
  | Typ.Tptr (_, Typ.Pk_pointer) when not rhs_owning_method && is_e1_decl ->
      (* for A __strong *e1 = e2 the semantics is*)
      (* retain(e2); e1=e2; *)
      let retain = mk_call retain_pname e2 typ in
      (e1,[retain; assign])
  | Typ.Tptr (_, Typ.Pk_objc_weak)
  | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained) ->
      (e1, [assign])
  | Typ.Tptr (_, Typ.Pk_objc_autoreleasing) ->
      (* for __autoreleasing e1 = e2 the semantics is*)
      (* retain(e2); autorelease(e2); e1=e2; *)
      let retain = mk_call retain_pname e2 typ in
      let autorelease = mk_call autorelease_pname e2 typ in
      (e1, [retain; autorelease; assign])
  | _ -> (e1, [assign])

(* Returns a pair ([binary_expression], instructions) for binary operator representing a *)
(* CompoundAssignment. "binary_expression" is returned when we are calculating an expression*)
(* "instructions" is not empty when the binary operator is actually a statement like an *)
(* assignment. *)
let compound_assignment_binary_operation_instruction boi e1 typ e2 loc =
  let id = Ident.create_fresh Ident.knormal in
  let instr1 = Sil.Load (id, e1, typ, loc) in
  let e_res, instr_op = match boi.Clang_ast_t.boi_kind with
    | `AddAssign ->
        let e1_plus_e2 = Exp.BinOp(Binop.PlusA, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_plus_e2, loc)])
    | `SubAssign ->
        let e1_sub_e2 = Exp.BinOp(Binop.MinusA, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_sub_e2, loc)])
    | `MulAssign ->
        let e1_mul_e2 = Exp.BinOp(Binop.Mult, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_mul_e2, loc)])
    | `DivAssign ->
        let e1_div_e2 = Exp.BinOp(Binop.Div, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_div_e2, loc)])
    | `ShlAssign ->
        let e1_shl_e2 = Exp.BinOp(Binop.Shiftlt, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_shl_e2, loc)])
    | `ShrAssign ->
        let e1_shr_e2 = Exp.BinOp(Binop.Shiftrt, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_shr_e2, loc)])
    | `RemAssign ->
        let e1_mod_e2 = Exp.BinOp(Binop.Mod, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_mod_e2, loc)])
    | `AndAssign ->
        let e1_and_e2 = Exp.BinOp(Binop.BAnd, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_and_e2, loc)])
    | `OrAssign ->
        let e1_or_e2 = Exp.BinOp(Binop.BOr, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_or_e2, loc)])
    | `XorAssign ->
        let e1_xor_e2 = Exp.BinOp(Binop.BXor, Exp.Var id, e2) in
        (e1, [Sil.Store (e1, typ, e1_xor_e2, loc)])
    | _ -> assert false in
  (e_res, instr1:: instr_op)

(* Returns a pair ([binary_expression], instructions). "binary_expression" *)
(* is returned when we are calculating an expression "instructions" is not *)
(* empty when the binary operator is actually a statement like an          *)
(* assignment.                                                             *)
let binary_operation_instruction boi e1 typ e2 loc rhs_owning_method =
  let binop_exp op = Exp.BinOp(op, e1, e2) in
  match boi.Clang_ast_t.boi_kind with
  | `Add -> (binop_exp (Binop.PlusA), [])
  | `Mul -> (binop_exp (Binop.Mult), [])
  | `Div -> (binop_exp (Binop.Div), [])
  | `Rem -> (binop_exp (Binop.Mod), [])
  | `Sub -> (binop_exp (Binop.MinusA), [])
  | `Shl -> (binop_exp (Binop.Shiftlt), [])
  | `Shr -> (binop_exp(Binop.Shiftrt), [])
  | `Or -> (binop_exp (Binop.BOr), [])
  | `And -> (binop_exp (Binop.BAnd), [])
  | `Xor -> (binop_exp (Binop.BXor), [])
  | `LT -> (binop_exp (Binop.Lt), [])
  | `GT -> (binop_exp (Binop.Gt), [])
  | `LE -> (binop_exp (Binop.Le), [])
  | `GE -> (binop_exp (Binop.Ge), [])
  | `NE -> (binop_exp (Binop.Ne), [])
  | `EQ -> (binop_exp (Binop.Eq), [])
  | `LAnd -> (binop_exp (Binop.LAnd), [])
  | `LOr -> (binop_exp (Binop.LOr), [])
  | `Assign ->
      if !Config.arc_mode && ObjcInterface_decl.is_pointer_to_objc_class typ then
        assignment_arc_mode e1 typ e2 loc rhs_owning_method false
      else
        (e1, [Sil.Store (e1, typ, e2, loc)])
  | `Comma -> (e2, []) (* C99 6.5.17-2 *)
  | `MulAssign | `DivAssign | `RemAssign | `AddAssign | `SubAssign
  | `ShlAssign | `ShrAssign | `AndAssign | `XorAssign | `OrAssign ->
      compound_assignment_binary_operation_instruction boi e1 typ e2 loc
  (* We should not get here.  *)
  (* These should be treated by compound_assignment_binary_operation_instruction*)
  | bok ->
      Logging.out
        "\nWARNING: Missing translation for Binary Operator Kind %s. Construct ignored...\n"
        (Clang_ast_j.string_of_binary_operator_kind bok);
      (Exp.minus_one, [])

let unary_operation_instruction translation_unit_context uoi e typ loc =
  let un_exp op =
    Exp.UnOp(op, e, Some typ) in
  match uoi.Clang_ast_t.uoi_kind with
  | `PostInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load (id, e, typ, loc) in
      let e_plus_1 = Exp.BinOp(Binop.PlusA, Exp.Var id, Exp.Const(Const.Cint (IntLit.one))) in
      (Exp.Var id, instr1::[Sil.Store (e, typ, e_plus_1, loc)])
  | `PreInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load (id, e, typ, loc) in
      let e_plus_1 = Exp.BinOp(Binop.PlusA, Exp.Var id, Exp.Const(Const.Cint (IntLit.one))) in
      let exp = if CGeneral_utils.is_cpp_translation translation_unit_context then
          e
        else
          e_plus_1 in
      (exp, instr1::[Sil.Store (e, typ, e_plus_1, loc)])
  | `PostDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load (id, e, typ, loc) in
      let e_minus_1 = Exp.BinOp(Binop.MinusA, Exp.Var id, Exp.Const(Const.Cint (IntLit.one))) in
      (Exp.Var id, instr1::[Sil.Store (e, typ, e_minus_1, loc)])
  | `PreDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load (id, e, typ, loc) in
      let e_minus_1 = Exp.BinOp(Binop.MinusA, Exp.Var id, Exp.Const(Const.Cint (IntLit.one))) in
      let exp = if CGeneral_utils.is_cpp_translation translation_unit_context then
          e
        else
          e_minus_1 in
      (exp, instr1::[Sil.Store (e, typ, e_minus_1, loc)])
  | `Not -> (un_exp (Unop.BNot), [])
  | `Minus -> (un_exp (Unop.Neg), [])
  | `Plus -> (e, [])
  | `LNot -> (un_exp (Unop.LNot), [])
  | `Deref ->
      (* Actual dereferencing is handled by implicit cast from rvalue to lvalue *)
      (e, [])
  | `AddrOf -> (e, [])
  | `Real | `Imag | `Extension | `Coawait ->
      let uok = Clang_ast_j.string_of_unary_operator_kind (uoi.Clang_ast_t.uoi_kind) in
      Logging.out
        "\nWARNING: Missing translation for Unary Operator Kind %s. The construct has been ignored...\n" uok;
      (e, [])

let bin_op_to_string boi =
  match boi.Clang_ast_t.boi_kind with
  | `PtrMemD -> "PtrMemD"
  | `PtrMemI -> "PtrMemI"
  | `Mul -> "Mul"
  | `Div -> "Div"
  | `Rem -> "Rem"
  | `Add -> "Add"
  | `Sub -> "Sub"
  | `Shl -> "Shl"
  | `Shr -> "Shr"
  | `LT -> "LT"
  | `GT -> "GT"
  | `LE -> "LE"
  | `GE -> "GE"
  | `EQ -> "EQ"
  | `NE -> "NE"
  | `And -> "And"
  | `Xor -> "Xor"
  | `Or -> "Or"
  | `LAnd -> "LAnd"
  | `LOr -> "LOr"
  | `Assign -> "Assign"
  | `MulAssign -> "MulAssign"
  | `DivAssign -> "DivAssign"
  | `RemAssign -> "RemAssing"
  | `AddAssign -> "AddAssign"
  | `SubAssign -> "SubAssign"
  | `ShlAssign -> "ShlAssign"
  | `ShrAssign -> "ShrAssign"
  | `AndAssign -> "AndAssign"
  | `XorAssign -> "XorAssign"
  | `OrAssign -> "OrAssign"
  | `Comma -> "Comma"

let sil_const_plus_one const =
  match const with
  | Exp.Const (Const.Cint n) ->
      Exp.Const (Const.Cint (IntLit.add n IntLit.one))
  | _ -> Exp.BinOp (Binop.PlusA, const, Exp.Const (Const.Cint (IntLit.one)))
