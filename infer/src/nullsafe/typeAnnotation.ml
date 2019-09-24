(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to represent annotations on types. *)

type t = {is_nullable: bool; origin: TypeOrigin.t} [@@deriving compare]

let equal = [%compare.equal: t]

let is_nullable ta = ta.is_nullable

let set_nullable b ta = if Bool.equal (is_nullable ta) b then ta else {ta with is_nullable= b}

let descr_origin ta =
  let descr_opt = TypeOrigin.get_description ta.origin in
  match descr_opt with
  | None ->
      ("", None, None)
  | Some (str, loc_opt, sig_opt) ->
      ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)


let to_string ta = if is_nullable ta then " @Nullable" else ""

let join ta1 ta2 =
  let nul1, nul2 = (is_nullable ta1, is_nullable ta2) in
  let choose_left = match (nul1, nul2) with false, true -> false | _ -> true in
  let ta_chosen, ta_other = if choose_left then (ta1, ta2) else (ta2, ta1) in
  let origin =
    if Bool.equal nul1 nul2 then TypeOrigin.join ta_chosen.origin ta_other.origin
    else ta_chosen.origin
  in
  let ta' = {ta_chosen with origin} in
  if equal ta' ta1 then None else Some ta'


let get_origin ta = ta.origin

let origin_is_fun_library ta =
  match get_origin ta with
  | TypeOrigin.Proc proc_origin ->
      proc_origin.TypeOrigin.is_library
  | _ ->
      false


let const_nullable is_nullable origin = {origin; is_nullable}

let with_origin ta o = {ta with origin= o}

let from_nullsafe_type NullsafeType.{nullability} origin =
  match nullability with
  | Nullable _ ->
      {origin; is_nullable= true}
  | Nonnull _ ->
      {origin; is_nullable= false}
