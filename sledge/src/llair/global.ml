(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = {var: Var.t; init: Exp.t option; siz: int; typ: Typ.t; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

let pp fs {var} =
  let name = Var.name var in
  let pf pp =
    Format.pp_open_box fs 2 ;
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs pp
  in
  pf "@%s%a" name Var.pp_demangled var

let pp_defn fs {var; init; typ} =
  Format.fprintf fs "@[<2>%a %a%a@]" Typ.pp typ Var.pp var
    (Option.pp " =@ @[%a@]" Exp.pp)
    init

let invariant g =
  Invariant.invariant [%here] g [%sexp_of: t]
  @@ fun () ->
  let {typ} = g in
  assert (Typ.is_sized typ)

let mk ?init var siz typ loc = {var; init; siz; typ; loc} |> check invariant
