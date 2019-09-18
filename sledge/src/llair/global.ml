(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = {var: Var.t; init: (Exp.t * int) option; typ: Typ.t; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

let pp fs {var} =
  let name = Var.name var in
  let pf pp =
    Format.pp_open_box fs 2 ;
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs pp
  in
  pf "@%s%a" name Var.pp_demangled var

let pp_defn fs {var; init; typ; loc} =
  Format.fprintf fs "@[<2>%a %a%a%a@]" Typ.pp typ Var.pp var Loc.pp loc
    (Option.pp "@ = @[%a@]" (fun fs (init, _) -> Exp.pp fs init))
    init

let invariant g =
  Invariant.invariant [%here] g [%sexp_of: t]
  @@ fun () ->
  let {var; typ} = g in
  assert (Typ.is_sized typ) ;
  assert (Var.global var)

let mk ?init var typ loc = {var; init; typ; loc} |> check invariant
