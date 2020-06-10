(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = {reg: Reg.t; init: (Exp.t * int) option; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

let pp fs {reg} =
  let name = Reg.name reg in
  let pf pp =
    Format.pp_open_box fs 2 ;
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs pp
  in
  pf "@%s%a" name Reg.pp_demangled reg

let pp_defn fs {reg; init; loc} =
  Format.fprintf fs "@[<2>%a %a%a%a@]" Typ.pp (Reg.typ reg) Reg.pp reg
    Loc.pp loc
    (Option.pp "@ = @[%a@]" Exp.pp)
    (Option.map ~f:fst init)

let invariant g =
  let@ () = Invariant.invariant [%here] g [%sexp_of: t] in
  let {reg} = g in
  assert (Typ.is_sized (Reg.typ reg)) ;
  assert (Var.is_global (Reg.var reg))

let mk ?init reg loc = {reg; init; loc} |> check invariant
