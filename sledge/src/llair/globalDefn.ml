(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

type t = {name: Global.t; init: (Exp.t * int) option; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

let pp ppf {name; init; loc} =
  Format.fprintf ppf "@[<2>%a %a%a %a@]" Typ.pp (Global.typ name) Global.pp
    name
    (Option.pp "@ = @[%a@]" Exp.pp)
    (Option.map ~f:fst init) Loc.pp loc

let invariant g =
  let@ () = Invariant.invariant [%here] g [%sexp_of: t] in
  let {name} = g in
  assert (Typ.is_sized (Global.typ name))

let mk ?init name loc = {name; init; loc} |> check invariant
