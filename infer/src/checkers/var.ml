(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Single abstraction for all the kinds of variables in SIL *)

type t =
  | ProgramVar of Pvar.t
  | LogicalVar of Ident.t

let of_id id =
  LogicalVar id

let of_pvar pvar =
  ProgramVar pvar

let to_exp = function
  | ProgramVar pvar -> Exp.Lvar pvar
  | LogicalVar id -> Exp.Var id

let compare v1 v2 = match v1, v2 with
  | ProgramVar pv1, ProgramVar pv2 -> Pvar.compare pv1 pv2
  | LogicalVar sv1, LogicalVar sv2 -> Ident.compare sv1 sv2
  | ProgramVar _, _ -> 1
  | LogicalVar _, _ -> -1

let equal v1 v2 =
  compare v1 v2 = 0

let pp fmt = function
  | ProgramVar pv -> (Pvar.pp pe_text) fmt pv
  | LogicalVar id -> (Ident.pp pe_text) fmt id

module Map = PrettyPrintable.MakePPMap(struct
    type nonrec t = t
    let compare = compare
    let pp_key = pp
  end)

module Set = PrettyPrintable.MakePPSet(struct
    type nonrec t = t
    let compare = compare
    let pp_element = pp
  end)
