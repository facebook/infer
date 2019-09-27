(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = {is_nullable: bool; origin: TypeOrigin.t} [@@deriving compare]

let equal = [%compare.equal: t]

let is_nullable t = t.is_nullable

let is_nonnull t = not t.is_nullable

let set_nonnull t = {t with is_nullable= false}

let descr_origin t =
  let descr_opt = TypeOrigin.get_description t.origin in
  match descr_opt with
  | None ->
      ("", None, None)
  | Some (str, loc_opt, sig_opt) ->
      ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)


let to_string t = if is_nullable t then " @Nullable" else ""

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


let get_origin t = t.origin

let origin_is_fun_library t =
  match get_origin t with
  | TypeOrigin.Proc proc_origin ->
      proc_origin.TypeOrigin.is_library
  | _ ->
      false


let create_nullable origin = {origin; is_nullable= true}

let create_nonnull origin = {origin; is_nullable= false}

let with_origin t o = {t with origin= o}

let of_annotated_nullability annotated_nullability origin =
  match annotated_nullability with
  | AnnotatedNullability.Nullable _ ->
      {origin; is_nullable= true}
  | AnnotatedNullability.Nonnull _ ->
      {origin; is_nullable= false}
