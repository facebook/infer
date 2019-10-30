(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = EqualTo of Const.t [@@deriving compare]

let pp fmt = function EqualTo c -> F.fprintf fmt "=%a" (Const.pp Pp.text) c

(** booleans with \top *)
module TBool = struct
  type t = True | False | Top
end

let flip_abduced (tbool, c1, c2) = (tbool, c2, c1)

let rec abduce_eq a1 a2 =
  match (a1, a2) with
  | Some (EqualTo c1), Some (EqualTo c2) when Const.equal c1 c2 ->
      (TBool.True, None, None)
  | Some (EqualTo _c1), Some (EqualTo _c2) (* c1≠c2 *) ->
      (TBool.False, None, None)
  | None, Some _ ->
      abduce_eq a2 a1 |> flip_abduced
  | Some (EqualTo _c), None ->
      (TBool.True, None, a1)
  | None, None ->
      (TBool.Top, None, None)


let abduce_ne a1 a2 =
  match (a1, a2) with
  | Some (EqualTo c1), Some (EqualTo c2) when Const.equal c1 c2 ->
      (TBool.False, None, None)
  | Some (EqualTo _c1), Some (EqualTo _c2) (* c1≠c2 *) ->
      (TBool.True, None, None)
  | None, Some _ | Some _, None ->
      (* cannot express ≠c so go to Top *)
      (TBool.Top, None, None)
  | None, None ->
      (TBool.Top, None, None)


let abduce_binop_constraints ~negated (bop : Binop.t) a1 a2 =
  let open Binop in
  match (bop, negated) with
  | Eq, false | Ne, true ->
      abduce_eq a1 a2
  | Eq, true | Ne, false ->
      abduce_ne a1 a2
  | _ ->
      (TBool.Top, None, None)


let abduce_binop_is_true_aux ~negated bop a1_opt a2_opt =
  Logging.d_printfln "abduce_binop_is_true ~negated:%b %s (%a) (%a)" negated
    (Binop.str Pp.text bop) (Pp.option pp) a1_opt (Pp.option pp) a2_opt ;
  abduce_binop_constraints ~negated bop a1_opt a2_opt


let abduce_binop_is_true ~negated bop v1 v2 =
  let result, abduced1, abduced2 = abduce_binop_is_true_aux ~negated bop v1 v2 in
  let can_go_through = match result with Top | True -> true | False -> false in
  (can_go_through, abduced1, abduced2)
