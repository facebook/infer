(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global variables *)

open! NS

type t = {name: Global.t; init: LlairExp.t option; loc: LairLoc.t} [@@deriving compare, equal, sexp]

let pp ppf {name; init; loc} =
  Format.fprintf ppf "@[<2>%a %a%a %a@]" LlairTyp.pp (Global.typ name) Global.pp name
    (Option.pp "@ = @[%a@]" LlairExp.pp)
    init LairLoc.pp loc


let invariant g =
  let@ () = Invariant.invariant [%here] g [%sexp_of: t] in
  match Global.typ g.name with
  | Pointer _ ->
      (* pre-llvm17 check: assert (Option.is_none g.init || LlairTyp.is_sized elt) *)
      ()
  | _ ->
      assert false


let mk ?init name loc = {name; init; loc} |> check invariant
