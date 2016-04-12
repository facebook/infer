(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Utility module for translating unary and binary operations and compound assignments *)

open CFrontend_utils

(* Returns the translation of assignment when ARC mode is enabled in Obj-C *)
(* For __weak and __unsafe_unretained the translation is the same as non-ARC *)
(* (this is because, in these cases, there is no change in the reference counter *)
(* of the pointee).*)
(* The difference is when the lvalue is a __strong or __autoreleasing. In those*)
(* case we need to add proper retain/release.*)
(* See document: "Objective-C Automatic Reference Counting" describing the semantics *)
let assignment_arc_mode e1 typ e2 loc rhs_owning_method is_e1_decl =
  let assign = Sil.Set (e1, typ, e2, loc) in
  let retain_pname = ModelBuiltins.__objc_retain in
  let release_pname = ModelBuiltins.__objc_release in
  let autorelease_pname = ModelBuiltins.__set_autorelease_attribute in
  let mk_call procname e t =
    let bi_retain = Sil.Const (Sil.Cfun procname) in
    Sil.Call([], bi_retain, [(e, t)], loc, Sil.cf_default) in
  match typ with
  | Sil.Tptr (_, Sil.Pk_pointer) when not rhs_owning_method && not is_e1_decl ->
      (* for __strong e1 = e2 the semantics is*)
      (* retain(e2); tmp=e1; e1=e2; release(tmp); *)
      let retain = mk_call retain_pname e2 typ in
      let id = Ident.create_fresh Ident.knormal in
      let tmp_assign = Sil.Letderef(id, e1, typ, loc) in
      let release = mk_call release_pname (Sil.Var id) typ in
      (e1,[retain; tmp_assign; assign; release ], [id])
  | Sil.Tptr (_, Sil.Pk_pointer) when not rhs_owning_method && is_e1_decl ->
      (* for A __strong *e1 = e2 the semantics is*)
      (* retain(e2); e1=e2; *)
      let retain = mk_call retain_pname e2 typ in
      (e1,[retain; assign ], [])
  | Sil.Tptr (_, Sil.Pk_objc_weak)
  | Sil.Tptr (_, Sil.Pk_objc_unsafe_unretained) ->
      (e1, [assign],[])
  | Sil.Tptr (_, Sil.Pk_objc_autoreleasing) ->
      (* for __autoreleasing e1 = e2 the semantics is*)
      (* retain(e2); autorelease(e2); e1=e2; *)
      let retain = mk_call retain_pname e2 typ in
      let autorelease = mk_call autorelease_pname e2 typ in
      (e1, [retain; autorelease; assign], [])
  | _ -> (e1, [assign], [])

(* Returns a pair ([binary_expression], instructions) for binary operator representing a *)
(* CompoundAssignment. "binary_expression" is returned when we are calculating an expression*)
(* "instructions" is not empty when the binary operator is actually a statement like an *)
(* assignment. *)
let compound_assignment_binary_operation_instruction boi e1 typ e2 loc =
  let id = Ident.create_fresh Ident.knormal in
  let instr1 = Sil.Letderef (id, e1, typ, loc) in
  let e_res, instr_op = match boi.Clang_ast_t.boi_kind with
    | `AddAssign ->
        let e1_plus_e2 = Sil.BinOp(Sil.PlusA, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_plus_e2, loc)])
    | `SubAssign ->
        let e1_sub_e2 = Sil.BinOp(Sil.MinusA, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_sub_e2, loc)])
    | `MulAssign ->
        let e1_mul_e2 = Sil.BinOp(Sil.Mult, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_mul_e2, loc)])
    | `DivAssign ->
        let e1_div_e2 = Sil.BinOp(Sil.Div, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_div_e2, loc)])
    | `ShlAssign ->
        let e1_shl_e2 = Sil.BinOp(Sil.Shiftlt, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_shl_e2, loc)])
    | `ShrAssign ->
        let e1_shr_e2 = Sil.BinOp(Sil.Shiftrt, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_shr_e2, loc)])
    | `RemAssign ->
        let e1_mod_e2 = Sil.BinOp(Sil.Mod, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_mod_e2, loc)])
    | `AndAssign ->
        let e1_and_e2 = Sil.BinOp(Sil.BAnd, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_and_e2, loc)])
    | `OrAssign ->
        let e1_or_e2 = Sil.BinOp(Sil.BOr, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_or_e2, loc)])
    | `XorAssign ->
        let e1_xor_e2 = Sil.BinOp(Sil.BXor, Sil.Var id, e2) in
        (e1, [Sil.Set (e1, typ, e1_xor_e2, loc)])
    | _ -> assert false in
  (e_res, instr1:: instr_op, [id])

(* Returns a pair ([binary_expression], instructions). "binary_expression" *)
(* is returned when we are calculating an expression "instructions" is not *)
(* empty when the binary operator is actually a statement like an          *)
(* assignment.                                                             *)
let binary_operation_instruction context boi e1 typ e2 loc rhs_owning_method =
  let binop_exp op = Sil.BinOp(op, e1, e2) in
  match boi.Clang_ast_t.boi_kind with
  | `Add -> (binop_exp (Sil.PlusA), [], [])
  | `Mul -> (binop_exp (Sil.Mult), [], [])
  | `Div -> (binop_exp (Sil.Div), [], [])
  | `Rem -> (binop_exp (Sil.Mod), [], [])
  | `Sub -> (binop_exp (Sil.MinusA), [], [])
  | `Shl -> (binop_exp (Sil.Shiftlt), [], [])
  | `Shr -> (binop_exp(Sil.Shiftrt), [], [])
  | `Or -> (binop_exp (Sil.BOr), [], [])
  | `And -> (binop_exp (Sil.BAnd), [], [])
  | `Xor -> (binop_exp (Sil.BXor), [], [])
  | `LT -> (binop_exp (Sil.Lt), [], [])
  | `GT -> (binop_exp (Sil.Gt), [], [])
  | `LE -> (binop_exp (Sil.Le), [], [])
  | `GE -> (binop_exp (Sil.Ge), [], [])
  | `NE -> (binop_exp (Sil.Ne), [], [])
  | `EQ -> (binop_exp (Sil.Eq), [], [])
  | `LAnd -> (binop_exp (Sil.LAnd), [], [])
  | `LOr -> (binop_exp (Sil.LOr), [], [])
  | `Assign ->
      if !Config.arc_mode && ObjcInterface_decl.is_pointer_to_objc_class context.CContext.tenv typ then
        assignment_arc_mode e1 typ e2 loc rhs_owning_method false
      else
        (e1, [Sil.Set (e1, typ, e2, loc)], [])
  | `Comma -> (e2, [], []) (* C99 6.5.17-2 *)
  | `MulAssign | `DivAssign | `RemAssign | `AddAssign | `SubAssign
  | `ShlAssign | `ShrAssign | `AndAssign | `XorAssign | `OrAssign ->
      compound_assignment_binary_operation_instruction boi e1 typ e2 loc
  (* We should not get here.  *)
  (* These should be treated by compound_assignment_binary_operation_instruction*)
  | bok ->
      Printing.log_stats
        "\nWARNING: Missing translation for Binary Operator Kind %s. Construct ignored...\n"
        (Clang_ast_j.string_of_binary_operator_kind bok);
      (Sil.exp_minus_one, [], [])

let unary_operation_instruction uoi e typ loc =
  let uok = Clang_ast_j.string_of_unary_operator_kind (uoi.Clang_ast_t.uoi_kind) in
  let un_exp op =
    Sil.UnOp(op, e, Some typ) in
  match uoi.Clang_ast_t.uoi_kind with
  | `PostInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Letderef (id, e, typ, loc) in
      let e_plus_1 = Sil.BinOp(Sil.PlusA, Sil.Var id, Sil.Const(Sil.Cint (Sil.Int.one))) in
      ([id], Sil.Var id, instr1::[Sil.Set (e, typ, e_plus_1, loc)])
  | `PreInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Letderef (id, e, typ, loc) in
      let e_plus_1 = Sil.BinOp(Sil.PlusA, Sil.Var id, Sil.Const(Sil.Cint (Sil.Int.one))) in
      let exp = if General_utils.is_cpp_translation !CFrontend_config.language then
          e
        else
          e_plus_1 in
      ([id], exp, instr1::[Sil.Set (e, typ, e_plus_1, loc)])
  | `PostDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Letderef (id, e, typ, loc) in
      let e_minus_1 = Sil.BinOp(Sil.MinusA, Sil.Var id, Sil.Const(Sil.Cint (Sil.Int.one))) in
      ([id], Sil.Var id, instr1::[Sil.Set (e, typ, e_minus_1, loc)])
  | `PreDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Letderef (id, e, typ, loc) in
      let e_minus_1 = Sil.BinOp(Sil.MinusA, Sil.Var id, Sil.Const(Sil.Cint (Sil.Int.one))) in
      let exp = if General_utils.is_cpp_translation !CFrontend_config.language then
          e
        else
          e_minus_1 in
      ([id], exp, instr1::[Sil.Set (e, typ, e_minus_1, loc)])
  | `Not -> ([], un_exp (Sil.BNot), [])
  | `Minus -> ([], un_exp (Sil.Neg), [])
  | `Plus -> ([], e, [])
  | `LNot -> ([], un_exp (Sil.LNot), [])
  | `Deref ->
      (* Actual dereferencing is handled by implicit cast from rvalue to lvalue *)
      ([], e, [])
  | `AddrOf -> ([], e, [])
  | `Real | `Imag | `Extension | `Coawait ->
      Printing.log_stats
        "\nWARNING: Missing translation for Unary Operator Kind %s. The construct has been ignored...\n" uok;
      ([], e, [])

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
  | Sil.Const (Sil.Cint n) ->
      Sil.Const (Sil.Cint (Sil.Int.add n Sil.Int.one))
  | _ -> Sil.BinOp (Sil.PlusA, const, Sil.Const (Sil.Cint (Sil.Int.one)))
