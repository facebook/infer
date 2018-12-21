(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type call = Direct of Typ.Procname.t | Indirect of HilExp.AccessExpression.t [@@deriving compare]

let pp_call fmt = function
  | Direct pname ->
      Typ.Procname.pp fmt pname
  | Indirect access_expr ->
      F.fprintf fmt "*%a" HilExp.AccessExpression.pp access_expr


type t =
  | Assign of HilExp.AccessExpression.t * HilExp.t * Location.t
  | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
  | Call of AccessPath.base * call * HilExp.t list * CallFlags.t * Location.t
  | ExitScope of Var.t list * Location.t
[@@deriving compare]

let pp fmt = function
  | Assign (access_expr, exp, loc) ->
      F.fprintf fmt "%a := %a [%a]" HilExp.AccessExpression.pp access_expr HilExp.pp exp
        Location.pp loc
  | Assume (exp, _, _, loc) ->
      F.fprintf fmt "assume %a [%a]" HilExp.pp exp Location.pp loc
  | Call (ret, call, actuals, _, loc) ->
      let pp_ret fmt = F.fprintf fmt "%a := " AccessPath.pp_base in
      let pp_actuals fmt = PrettyPrintable.pp_collection ~pp_item:HilExp.pp fmt in
      F.fprintf fmt "%a%a(%a) [%a]" pp_ret ret pp_call call pp_actuals actuals Location.pp loc
  | ExitScope (vars, loc) ->
      F.fprintf fmt "exit scope(%a) [%a]" (Pp.seq ~sep:"; " Var.pp) vars Location.pp loc


type translation = Instr of t | Bind of Var.t * HilExp.AccessExpression.t | Ignore

(** convert an SIL instruction into an HIL instruction. The [f_resolve_id] function should map an
   SSA temporary variable to the access path it represents. Evaluating the HIL instruction should
   produce the same result as evaluating the SIL instruction and replacing the temporary variables
   using [f_resolve_id]. *)
let of_sil ~include_array_indexes ~f_resolve_id (instr : Sil.instr) =
  let exp_of_sil ?(add_deref = false) =
    HilExp.of_sil ~include_array_indexes ~f_resolve_id ~add_deref
  in
  let analyze_id_assignment ?(add_deref = false) lhs_id rhs_exp rhs_typ loc =
    let rhs_hil_exp = exp_of_sil ~add_deref rhs_exp rhs_typ in
    match rhs_hil_exp with
    | HilExp.AccessExpression rhs_access_expr ->
        Bind (lhs_id, rhs_access_expr)
    | _ ->
        Instr (Assign (HilExp.AccessExpression.base (lhs_id, rhs_typ), rhs_hil_exp, loc))
  in
  match instr with
  | Load (lhs_id, rhs_exp, rhs_typ, loc) ->
      analyze_id_assignment ~add_deref:true (Var.of_id lhs_id) rhs_exp rhs_typ loc
  | Store (Lvar lhs_pvar, lhs_typ, rhs_exp, loc) when Pvar.is_ssa_frontend_tmp lhs_pvar ->
      (* do not need to add deref here as it is added implicitly in of_pvar by forgetting the & *)
      analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ loc
  | Call
      ( (ret_id, _)
      , Const (Cfun callee_pname)
      , (target_exp, _) :: (Sizeof {typ= cast_typ}, _) :: _
      , loc
      , _ )
    when Typ.Procname.equal callee_pname BuiltinDecl.__cast ->
      analyze_id_assignment (Var.of_id ret_id) target_exp cast_typ loc
  | Store (lhs_exp, typ, rhs_exp, loc) ->
      let lhs_access_expr =
        match HilExp.ignore_cast (exp_of_sil ~add_deref:true lhs_exp typ) with
        | HilExp.AccessExpression access_expr ->
            access_expr
        | BinaryOperator (_, exp0, exp1) -> (
          (* pointer arithmetic. somewhere in one of the expressions, there should be at least
               one pointer type represented as an access path. just use that access path and forget
               about the arithmetic. if you need to model this more precisely, you should be using
               SIL instead *)
          match HilExp.get_access_exprs exp0 with
          | ap :: _ ->
              ap
          | [] -> (
            match HilExp.get_access_exprs exp1 with
            | ap :: _ ->
                ap
            | [] ->
                L.(die InternalError)
                  "Invalid pointer arithmetic expression %a used as LHS at %a" Exp.pp lhs_exp
                  Location.pp_file_pos loc ) )
        | HilExp.Constant (Const.Cint i) ->
            (* this can happen in intentionally crashing code like *0xdeadbeef = 0 used for
               debugging. doesn't really matter what we do here, so just create a dummy var *)
            let dummy_base_var =
              Var.of_id (Ident.create_normal (Ident.string_to_name (IntLit.to_string i)) 0)
            in
            HilExp.AccessExpression.base (dummy_base_var, Typ.void_star)
        | _ ->
            L.(die InternalError)
              "Non-assignable LHS expression %a at %a" Exp.pp lhs_exp Location.pp_file_pos loc
      in
      Instr (Assign (lhs_access_expr, exp_of_sil rhs_exp typ, loc))
  | Call ((ret_id, ret_typ), call_exp, formals, loc, call_flags) ->
      let hil_ret = (Var.of_id ret_id, ret_typ) in
      let hil_call =
        match exp_of_sil call_exp (Typ.mk Tvoid) with
        | Constant (Cfun procname) | Closure (procname, _) ->
            Direct procname
        | HilExp.AccessExpression access_expr ->
            Indirect access_expr
        | call_exp ->
            L.(die InternalError) "Unexpected call expression %a" HilExp.pp call_exp
      in
      let formals = List.map ~f:(fun (exp, typ) -> exp_of_sil exp typ) formals in
      Instr (Call (hil_ret, hil_call, formals, call_flags, loc))
  | Prune (exp, loc, true_branch, if_kind) ->
      let hil_exp = exp_of_sil exp (Typ.mk (Tint IBool)) in
      let branch = if true_branch then `Then else `Else in
      Instr (Assume (hil_exp, branch, if_kind, loc))
  | ExitScope (vars, loc) ->
      Instr (ExitScope (vars, loc))
  | Abstract _ | Nullify _ ->
      (* these don't seem useful for most analyses. can translate them later if we want to *)
      Ignore
