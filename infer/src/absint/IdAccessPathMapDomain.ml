(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IdMap = Var.Map
module L = Logging

include (IdMap : module type of IdMap with type 'a t := 'a IdMap.t)

let bottom = empty

let is_bottom = is_empty

type t = HilExp.AccessExpression.t IdMap.t

type value = HilExp.AccessExpression.t

let pp fmt astate = IdMap.pp ~pp_value:HilExp.AccessExpression.pp fmt astate

let check_invariant ap1 ap2 = function
  | Var.ProgramVar pvar when Pvar.is_ssa_frontend_tmp pvar ->
      (* Sawja reuses temporary variables which sometimes breaks this invariant *)
      ()
  | id ->
      if not (HilExp.AccessExpression.equal ap1 ap2) then
        L.(die InternalError)
          "Id %a maps to both %a and %a" Var.pp id HilExp.AccessExpression.pp ap1
          HilExp.AccessExpression.pp ap2


let leq ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else
    IdMap.for_all
      (fun id lhs_ap ->
        let rhs_has = IdMap.mem id rhs in
        if rhs_has && Config.debug_exceptions then check_invariant lhs_ap (IdMap.find id rhs) id ;
        rhs_has )
      lhs


let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    IdMap.merge
      (fun var ap1_opt ap2_opt ->
        match (ap1_opt, ap2_opt) with
        | Some ap1, Some ap2 ->
            if Config.debug_exceptions then check_invariant ap1 ap2 var ;
            ap1_opt
        | Some _, None ->
            ap1_opt
        | None, Some _ ->
            ap2_opt
        | None, None ->
            None )
      astate1 astate2


let widen ~prev ~next ~num_iters:_ = join prev next
