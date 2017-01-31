(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module IdMap = Var.Map

type astate = AccessPath.Raw.t IdMap.t

include IdMap

let pp fmt astate =
  IdMap.pp ~pp_value:AccessPath.Raw.pp fmt astate

let check_invariant ap1 ap2 = function
  | Var.ProgramVar pvar when Pvar.is_frontend_tmp pvar ->
      (* Sawja reuses temporary variables which sometimes breaks this invariant *)
      (* TODO: fix (13370224) *)
      ()
  | id ->
      if not (AccessPath.Raw.equal ap1 ap2)
      then
        failwithf "Id %a maps to both %a and %a@."
          Var.pp id
          AccessPath.Raw.pp ap1
          AccessPath.Raw.pp ap2

let (<=) ~lhs ~rhs =
  if phys_equal lhs rhs
  then true
  else
    IdMap.for_all
      (fun id lhs_ap ->
         let rhs_has = IdMap.mem id rhs in
         if rhs_has && Config.debug_exceptions
         then check_invariant lhs_ap (IdMap.find id rhs) id;
         rhs_has)
      lhs

let join astate1 astate2 =
  if phys_equal astate1 astate2
  then astate1
  else
    IdMap.merge
      (fun var ap1_opt ap2_opt -> match ap1_opt, ap2_opt with
         | Some ap1, Some ap2 ->
             if Config.debug_exceptions then check_invariant ap1 ap2 var;
             ap1_opt
         | Some _, None -> ap1_opt
         | None, Some _ -> ap2_opt
         | None, None -> None)
      astate1
      astate2

let widen ~prev ~next ~num_iters:_ =
  join prev next
