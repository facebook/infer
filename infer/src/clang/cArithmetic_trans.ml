(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let div_of_operand_types t1 t2 : Binop.t =
  (* integer division semantics iff both operands are integers *)
  if Typ.is_int t1 && Typ.is_int t2 then DivI else DivF


let compound_assignment_binary_operation_instruction boi_kind (e1, t1) typ (e2, t2) loc =
  let instrs =
    let bop =
      match boi_kind with
      | `AddAssign ->
          if Typ.is_pointer t1 then Binop.PlusPI else Binop.PlusA (Typ.get_ikind_opt typ)
      | `SubAssign ->
          if Typ.is_pointer t1 then Binop.MinusPI else Binop.MinusA (Typ.get_ikind_opt typ)
      | `MulAssign ->
          Binop.Mult (Typ.get_ikind_opt typ)
      | `DivAssign ->
          div_of_operand_types t1 t2
      | `ShlAssign ->
          Binop.Shiftlt
      | `ShrAssign ->
          Binop.Shiftrt
      | `RemAssign ->
          Binop.Mod
      | `AndAssign ->
          Binop.BAnd
      | `OrAssign ->
          Binop.BOr
      | `XorAssign ->
          Binop.BXor
    in
    let id = Ident.create_fresh Ident.knormal in
    [Sil.Load {id; e= e1; typ; loc}; Sil.Store {e1; typ; e2= Exp.BinOp (bop, Exp.Var id, e2); loc}]
  in
  (e1, instrs)


(** Returns a pair ([binary_expression], instructions). "binary_expression" is returned when we are
    calculating an expression "instructions" is not empty when the binary operator is actually a
    statement like an assignment. *)
let binary_operation_instruction source_range boi ((e1, t1) as e1_with_typ) typ
    ((e2, t2) as e2_with_typ) loc =
  let binop_exp ?(change_order = false) op =
    if change_order then Exp.BinOp (op, e2, e1) else Exp.BinOp (op, e1, e2)
  in
  match boi.Clang_ast_t.boi_kind with
  (* Note: Pointers to members that are not statically known are not
     expressible in Sil. The translation of the PtrMem ops treats the field as
     an integer offset, which is itself semantically ok though too low-level,
     but the translation of the argument expressions does not compute such
     offsets and instead passes the member pointer at type 'void'. *)
  | `PtrMemD | `PtrMemI ->
      CFrontend_errors.unimplemented __POS__ source_range
        "Pointer-to-member constructs are unsupported. Got '%a'."
        (Pp.of_string ~f:Clang_ast_j.string_of_binary_operator_info)
        boi
  | `Add ->
      if Typ.is_pointer t1 then (binop_exp Binop.PlusPI, [])
      else if Typ.is_pointer t2 then (binop_exp ~change_order:true Binop.PlusPI, [])
      else (binop_exp (Binop.PlusA (Typ.get_ikind_opt typ)), [])
  | `Mul ->
      (binop_exp (Binop.Mult (Typ.get_ikind_opt typ)), [])
  | `Div ->
      (binop_exp (div_of_operand_types t1 t2), [])
  | `Rem ->
      (binop_exp Binop.Mod, [])
  | `Sub ->
      if Typ.is_pointer t1 then
        if Typ.is_pointer t2 then (binop_exp Binop.MinusPP, []) else (binop_exp Binop.MinusPI, [])
      else (binop_exp (Binop.MinusA (Typ.get_ikind_opt typ)), [])
  | `Shl ->
      (binop_exp Binop.Shiftlt, [])
  | `Shr ->
      (binop_exp Binop.Shiftrt, [])
  | `Or ->
      (binop_exp Binop.BOr, [])
  | `And ->
      (binop_exp Binop.BAnd, [])
  | `Xor ->
      (binop_exp Binop.BXor, [])
  | `LT ->
      (binop_exp Binop.Lt, [])
  | `GT ->
      (binop_exp Binop.Gt, [])
  | `LE ->
      (binop_exp Binop.Le, [])
  | `GE ->
      (binop_exp Binop.Ge, [])
  | `NE ->
      (binop_exp Binop.Ne, [])
  | `EQ ->
      (binop_exp Binop.Eq, [])
  | `LAnd ->
      (binop_exp Binop.LAnd, [])
  | `LOr ->
      (binop_exp Binop.LOr, [])
  | `Assign ->
      (e1, [Sil.Store {e1; typ; e2; loc}])
  | `Cmp ->
      CFrontend_errors.unimplemented __POS__ source_range "C++20 spaceship operator <=>"
      (* C++20 spaceship operator <=>, TODO *)
  | `Comma ->
      (e2, []) (* C99 6.5.17-2 *)
  | ( `MulAssign
    | `DivAssign
    | `RemAssign
    | `AddAssign
    | `SubAssign
    | `ShlAssign
    | `ShrAssign
    | `AndAssign
    | `XorAssign
    | `OrAssign ) as boi_kind ->
      compound_assignment_binary_operation_instruction boi_kind e1_with_typ typ e2_with_typ loc


let unary_operation_instruction translation_unit_context uoi e typ loc =
  let un_exp op = Exp.UnOp (op, e, Some typ) in
  match uoi.Clang_ast_t.uoi_kind with
  | `PostInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load {id; e; typ; loc} in
      let bop = if Typ.is_pointer typ then Binop.PlusPI else Binop.PlusA (Typ.get_ikind_opt typ) in
      let e_plus_1 = Exp.BinOp (bop, Exp.Var id, Exp.Const (Const.Cint IntLit.one)) in
      (Exp.Var id, [instr1; Sil.Store {e1= e; typ; e2= e_plus_1; loc}])
  | `PreInc ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load {id; e; typ; loc} in
      let bop = if Typ.is_pointer typ then Binop.PlusPI else Binop.PlusA (Typ.get_ikind_opt typ) in
      let e_plus_1 = Exp.BinOp (bop, Exp.Var id, Exp.Const (Const.Cint IntLit.one)) in
      let exp =
        if CGeneral_utils.is_cpp_translation translation_unit_context then e else e_plus_1
      in
      (exp, [instr1; Sil.Store {e1= e; typ; e2= e_plus_1; loc}])
  | `PostDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load {id; e; typ; loc} in
      let bop =
        if Typ.is_pointer typ then Binop.MinusPI else Binop.MinusA (Typ.get_ikind_opt typ)
      in
      let e_minus_1 = Exp.BinOp (bop, Exp.Var id, Exp.Const (Const.Cint IntLit.one)) in
      (Exp.Var id, [instr1; Sil.Store {e1= e; typ; e2= e_minus_1; loc}])
  | `PreDec ->
      let id = Ident.create_fresh Ident.knormal in
      let instr1 = Sil.Load {id; e; typ; loc} in
      let bop =
        if Typ.is_pointer typ then Binop.MinusPI else Binop.MinusA (Typ.get_ikind_opt typ)
      in
      let e_minus_1 = Exp.BinOp (bop, Exp.Var id, Exp.Const (Const.Cint IntLit.one)) in
      let exp =
        if CGeneral_utils.is_cpp_translation translation_unit_context then e else e_minus_1
      in
      (exp, [instr1; Sil.Store {e1= e; typ; e2= e_minus_1; loc}])
  | `Not ->
      (un_exp Unop.BNot, [])
  | `Minus ->
      (un_exp Unop.Neg, [])
  | `Plus ->
      (e, [])
  | `LNot ->
      (un_exp Unop.LNot, [])
  | `Deref ->
      (* Actual dereferencing is handled by implicit cast from rvalue to lvalue *)
      (e, [])
  | `AddrOf ->
      (e, [])
  | `Real | `Imag | `Extension | `Coawait ->
      let uok = Clang_ast_j.string_of_unary_operator_kind uoi.Clang_ast_t.uoi_kind in
      L.(debug Capture Medium)
        "@\n\
         WARNING: Missing translation for Unary Operator Kind %s. The construct has been ignored...@\n"
        uok ;
      (e, [])


let atomic_operation_instruction aei e1 e2 typ loc =
  let atomic_binop bop ~fetch_first =
    let id = Ident.create_fresh Ident.knormal in
    let instr1 = Sil.Load {id; e= e1; typ; loc} in
    let e1_op_e2 = Exp.BinOp (bop, Exp.Var id, e2) in
    if fetch_first then (Exp.Var id, [instr1; Sil.Store {e1; typ; e2= e1_op_e2; loc}])
    else
      let fetch_id = Ident.create_fresh Ident.knormal in
      let fetch_instr = Sil.Load {id= fetch_id; e= e1; typ; loc} in
      (Exp.Var fetch_id, [instr1; Sil.Store {e1; typ; e2= e1_op_e2; loc}; fetch_instr])
  in
  match aei.Clang_ast_t.aei_kind with
  (* GNU builtins do arithmetic operations even with pointers *)
  | `AO__atomic_fetch_add ->
      atomic_binop (Binop.PlusA (Typ.get_ikind_opt typ)) ~fetch_first:true
  | `AO__c11_atomic_fetch_add | `AO__opencl_atomic_fetch_add ->
      atomic_binop
        (if Typ.is_pointer typ then Binop.PlusPI else Binop.PlusA (Typ.get_ikind_opt typ))
        ~fetch_first:true
  | `AO__atomic_add_fetch ->
      atomic_binop (Binop.PlusA (Typ.get_ikind_opt typ)) ~fetch_first:false
  | `AO__atomic_fetch_sub ->
      atomic_binop (Binop.MinusA (Typ.get_ikind_opt typ)) ~fetch_first:true
  | `AO__c11_atomic_fetch_sub | `AO__opencl_atomic_fetch_sub ->
      atomic_binop
        (if Typ.is_pointer typ then Binop.MinusPI else Binop.MinusA (Typ.get_ikind_opt typ))
        ~fetch_first:true
  | `AO__atomic_sub_fetch ->
      atomic_binop (Binop.MinusA (Typ.get_ikind_opt typ)) ~fetch_first:false
  | `AO__atomic_fetch_or | `AO__c11_atomic_fetch_or | `AO__opencl_atomic_fetch_or ->
      atomic_binop Binop.BOr ~fetch_first:true
  | `AO__atomic_or_fetch ->
      atomic_binop Binop.BOr ~fetch_first:false
  | `AO__atomic_fetch_xor | `AO__c11_atomic_fetch_xor | `AO__opencl_atomic_fetch_xor ->
      atomic_binop Binop.BXor ~fetch_first:true
  | `AO__atomic_xor_fetch ->
      atomic_binop Binop.BXor ~fetch_first:false
  | `AO__atomic_fetch_and | `AO__c11_atomic_fetch_and | `AO__opencl_atomic_fetch_and ->
      atomic_binop Binop.BAnd ~fetch_first:true
  | `AO__atomic_and_fetch ->
      atomic_binop Binop.BAnd ~fetch_first:false
  | _ ->
      (* We only call this function if it's been implemented. *)
      assert false


let bin_op_to_string boi =
  match boi.Clang_ast_t.boi_kind with
  | `PtrMemD ->
      "PtrMemD"
  | `PtrMemI ->
      "PtrMemI"
  | `Mul ->
      "Mul"
  | `Div ->
      "Div"
  | `Rem ->
      "Rem"
  | `Add ->
      "Add"
  | `Sub ->
      "Sub"
  | `Shl ->
      "Shl"
  | `Shr ->
      "Shr"
  | `LT ->
      "LT"
  | `GT ->
      "GT"
  | `LE ->
      "LE"
  | `GE ->
      "GE"
  | `EQ ->
      "EQ"
  | `NE ->
      "NE"
  | `And ->
      "And"
  | `Xor ->
      "Xor"
  | `Or ->
      "Or"
  | `LAnd ->
      "LAnd"
  | `LOr ->
      "LOr"
  | `Assign ->
      "Assign"
  | `MulAssign ->
      "MulAssign"
  | `DivAssign ->
      "DivAssign"
  | `RemAssign ->
      "RemAssing"
  | `AddAssign ->
      "AddAssign"
  | `SubAssign ->
      "SubAssign"
  | `ShlAssign ->
      "ShlAssign"
  | `ShrAssign ->
      "ShrAssign"
  | `AndAssign ->
      "AndAssign"
  | `XorAssign ->
      "XorAssign"
  | `OrAssign ->
      "OrAssign"
  | `Cmp ->
      "Cmp"
  | `Comma ->
      "Comma"


let sil_const_plus_one const =
  match const with
  | Exp.Const (Const.Cint n) ->
      Exp.Const (Const.Cint (IntLit.add n IntLit.one))
  | _ ->
      Exp.BinOp (Binop.PlusA None, const, Exp.Const (Const.Cint IntLit.one))
