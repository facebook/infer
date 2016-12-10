(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** mapping of ids to raw access paths. useful for id-normalizing access paths *)

module IdMap = Var.Map

type astate = AccessPath.raw IdMap.t

include IdMap

let pp fmt astate =
  IdMap.pp ~pp_value:AccessPath.pp_raw fmt astate

let initial = IdMap.empty

let (<=) ~lhs ~rhs =
  if phys_equal lhs rhs
  then true
  else
    try
      IdMap.for_all
        (fun id lhs_ap ->
           let rhs_ap = IdMap.find id rhs in
           let eq = AccessPath.equal_raw lhs_ap rhs_ap in
           assert eq;
           eq)
        lhs
    with Not_found -> false

let join astate1 astate2 =
  if phys_equal astate1 astate2
  then astate1
  else
    IdMap.merge
      (fun var ap1_opt ap2_opt -> match ap1_opt, ap2_opt with
         | Some ap1, Some ap2 ->
             (* in principle, could do a join here if the access paths have the same root. but
                they should always be equal if we are using the map correctly *)
             let check_invariant () = match var with
               | Var.ProgramVar pvar when Pvar.is_frontend_tmp pvar ->
                   (* Sawja reuses temporary variables which sometimes breaks this invariant *)
                   (* TODO: fix (13370224) *)
                   ()
               | _ ->
                   assert (AccessPath.equal_raw ap1 ap2) in
             check_invariant ();
             ap1_opt
         | Some _, None -> ap1_opt
         | None, Some _ -> ap2_opt
         | None, None -> None)
      astate1
      astate2

let widen ~prev ~next ~num_iters:_ =
  join prev next
