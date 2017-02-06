(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Single abstraction for all the kinds of variables in SIL *)

type t =
  | LogicalVar of Ident.t
  | ProgramVar of Pvar.t
[@@deriving compare]

let equal = [%compare.equal : t]

let of_id id =
  LogicalVar id

let of_pvar pvar =
  ProgramVar pvar

let to_exp = function
  | ProgramVar pvar -> Exp.Lvar pvar
  | LogicalVar id -> Exp.Var id

let compare_alpha v1 v2 = match v1, v2 with
  | ProgramVar pv1, ProgramVar pv2 -> Pvar.compare_alpha pv1 pv2
  | _ -> compare v1 v2

let pp fmt = function
  | ProgramVar pv -> (Pvar.pp Pp.text) fmt pv
  | LogicalVar id -> (Ident.pp Pp.text) fmt id

module Map = PrettyPrintable.MakePPMap(struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end)

module Set = PrettyPrintable.MakePPCompareSet(struct
    type nonrec t = t
    let compare = compare
    let compare_pp = compare_alpha
    let pp = pp
  end)
