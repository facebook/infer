(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

type call = Direct of Typ.Procname.t | Indirect of AccessPath.t [@@deriving compare]

let pp_call fmt = function
  | Direct pname
   -> Typ.Procname.pp fmt pname
  | Indirect access_path
   -> F.fprintf fmt "*%a" AccessPath.pp access_path

type t =
  | Assign of AccessPath.t * HilExp.t * Location.t
  | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
  | Call of AccessPath.base option * call * HilExp.t list * CallFlags.t * Location.t
  [@@deriving compare]

let pp fmt = function
  | Assign (access_path, exp, loc)
   -> F.fprintf fmt "%a := %a [%a]" AccessPath.pp access_path HilExp.pp exp Location.pp loc
  | Assume (exp, _, _, loc)
   -> F.fprintf fmt "assume %a [%a]" HilExp.pp exp Location.pp loc
  | Call (ret_opt, call, actuals, _, loc)
   -> let pp_ret fmt = Option.iter ~f:(F.fprintf fmt "%a := " AccessPath.pp_base) in
      let pp_actuals fmt = PrettyPrintable.pp_collection ~pp_item:HilExp.pp fmt in
      F.fprintf fmt "%a%a(%a) [%a]" pp_ret ret_opt pp_call call pp_actuals actuals Location.pp loc

type translation = Instr of t | Bind of Var.t * AccessPath.t | Unbind of Var.t list | Ignore

(* convert an SIL instruction into an HIL instruction. the [f_resolve_id] function should map an SSA
   temporary variable to the access path it represents. evaluating the HIL instruction should
   produce the same result as evaluating the SIL instruction and replacing the temporary variables
   using [f_resolve_id]. *)
let of_sil ~include_array_indexes ~f_resolve_id (instr: Sil.instr) =
  let exp_of_sil = HilExp.of_sil ~include_array_indexes ~f_resolve_id in
  let analyze_id_assignment lhs_id rhs_exp rhs_typ loc =
    let rhs_hil_exp = exp_of_sil rhs_exp rhs_typ in
    match HilExp.get_access_paths rhs_hil_exp with
    | [rhs_access_path]
     -> Bind (lhs_id, rhs_access_path)
    | _
     -> Instr (Assign (((lhs_id, rhs_typ), []), rhs_hil_exp, loc))
  in
  match instr with
  | Load (lhs_id, rhs_exp, rhs_typ, loc)
   -> analyze_id_assignment (Var.of_id lhs_id) rhs_exp rhs_typ loc
  | Store (Lvar lhs_pvar, lhs_typ, rhs_exp, loc) when Pvar.is_ssa_frontend_tmp lhs_pvar
   -> analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ loc
  | Call
      ( Some (ret_id, _)
      , Const Cfun callee_pname
      , (target_exp, _) :: (Sizeof {typ= cast_typ}, _) :: _
      , loc
      , _ )
    when Typ.Procname.equal callee_pname BuiltinDecl.__cast
   -> analyze_id_assignment (Var.of_id ret_id) target_exp cast_typ loc
  | Store (lhs_exp, typ, rhs_exp, loc)
   -> let lhs_access_path =
        match exp_of_sil lhs_exp typ with
        | AccessPath ap
         -> ap
        | BinaryOperator (_, exp0, exp1) -> (
          match
            (* pointer arithmetic. somewhere in one of the expressions, there should be at least
               one pointer type represented as an access path. just use that access path and forget
               about the arithmetic. if you need to model this more precisely, you should be using
               SIL instead *)
            HilExp.get_access_paths exp0
          with
          | ap :: _
           -> ap
          | [] ->
            match HilExp.get_access_paths exp1 with
            | ap :: _
             -> ap
            | []
             -> invalid_argf "Invalid pointer arithmetic expression %a used as LHS" Exp.pp lhs_exp
          )
        | _
         -> invalid_argf "Non-assignable LHS expression %a" Exp.pp lhs_exp
      in
      Instr (Assign (lhs_access_path, exp_of_sil rhs_exp typ, loc))
  | Call (ret_opt, call_exp, formals, loc, call_flags)
   -> let hil_ret = Option.map ~f:(fun (ret_id, ret_typ) -> (Var.of_id ret_id, ret_typ)) ret_opt in
      let hil_call =
        match exp_of_sil call_exp (Typ.mk Tvoid) with
        | Constant Cfun procname
         -> Direct procname
        | AccessPath access_path
         -> Indirect access_path
        | call_exp
         -> invalid_argf "Unexpected call expression %a" HilExp.pp call_exp
      in
      let formals = List.map ~f:(fun (exp, typ) -> exp_of_sil exp typ) formals in
      Instr (Call (hil_ret, hil_call, formals, call_flags, loc))
  | Prune (exp, loc, true_branch, if_kind)
   -> let hil_exp = exp_of_sil exp (Typ.mk (Tint IBool)) in
      let branch = if true_branch then `Then else `Else in
      Instr (Assume (hil_exp, branch, if_kind, loc))
  | Nullify (pvar, _)
   -> Unbind [Var.of_pvar pvar]
  | Remove_temps (ids, _)
   -> Unbind (List.map ~f:Var.of_id ids)
  (* ignoring for now; will translate as builtin function call if needed *)
  | Abstract _
  | Declare_locals _
   -> (* these don't seem useful for most analyses. can translate them later if we want to *)
      Ignore
