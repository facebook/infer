(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Single abstraction for all the kinds of variables in SIL *)

type t = LogicalVar of Ident.t | ProgramVar of Pvar.t [@@deriving compare]

let equal = [%compare.equal : t]

let compare_modulo_this x y =
  match (x, y) with
  | ProgramVar i, ProgramVar j ->
      Pvar.compare_modulo_this i j
  | _, _ ->
      compare x y


let of_id id = LogicalVar id

let of_pvar pvar = ProgramVar pvar

let of_formal_index formal_index = of_id (Ident.create_footprint Ident.name_spec formal_index)

let to_exp = function ProgramVar pvar -> Exp.Lvar pvar | LogicalVar id -> Exp.Var id

let is_global = function ProgramVar pvar -> Pvar.is_global pvar | LogicalVar _ -> false

let is_return = function ProgramVar pvar -> Pvar.is_return pvar | LogicalVar _ -> false

let is_footprint = function ProgramVar _ -> false | LogicalVar id -> Ident.is_footprint id

let appears_in_source_code = function
  | LogicalVar _ ->
      false
  | ProgramVar pvar ->
      not (Pvar.is_frontend_tmp pvar)


let pp fmt = function
  | ProgramVar pv ->
      F.fprintf fmt "%s" (Pvar.get_simplified_name pv)
  | LogicalVar id ->
      Ident.pp fmt id


let get_footprint_index t =
  match t with LogicalVar id when is_footprint t -> Some (Ident.get_stamp id) | _ -> None


module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t

  let compare = compare

  let pp = pp
end)
