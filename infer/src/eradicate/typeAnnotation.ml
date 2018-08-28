(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to represent annotations on types. *)

module AnnotationsMap = Caml.Map.Make (struct
  type t = AnnotatedSignature.annotation [@@deriving compare]
end)

type t = {map: bool AnnotationsMap.t; origin: TypeOrigin.t} [@@deriving compare]

let equal = [%compare.equal: t]

let get_value ann ta = try AnnotationsMap.find ann ta.map with Caml.Not_found -> false

let set_value ann b ta =
  if Bool.equal (get_value ann ta) b then ta else {ta with map= AnnotationsMap.add ann b ta.map}


let get_nullable = get_value AnnotatedSignature.Nullable

let get_present = get_value Present

let set_nullable b = set_value Nullable b

let set_present b = set_value Present b

let descr_origin ta =
  let descr_opt = TypeOrigin.get_description ta.origin in
  match descr_opt with
  | None ->
      ("", None, None)
  | Some (str, loc_opt, sig_opt) ->
      ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)


let to_string ta =
  let nullable_s = if get_nullable ta then " @Nullable" else "" in
  let present_s = if get_present ta then " @Present" else "" in
  nullable_s ^ present_s


let join ta1 ta2 =
  let nul1, nul2 = (get_nullable ta1, get_nullable ta2) in
  let choose_left = match (nul1, nul2) with false, true -> false | _ -> true in
  let ta_chosen, ta_other = if choose_left then (ta1, ta2) else (ta2, ta1) in
  let present = get_present ta1 && get_present ta2 in
  let origin =
    if Bool.equal nul1 nul2 then TypeOrigin.join ta_chosen.origin ta_other.origin
    else ta_chosen.origin
  in
  let ta' = set_present present {ta_chosen with origin} in
  if equal ta' ta1 then None else Some ta'


let get_origin ta = ta.origin

let origin_is_fun_library ta =
  match get_origin ta with
  | TypeOrigin.Proc proc_origin ->
      proc_origin.TypeOrigin.is_library
  | _ ->
      false


let const annotation b origin =
  let nullable, present =
    match annotation with
    | AnnotatedSignature.Nullable ->
        (b, false)
    | AnnotatedSignature.Present ->
        (false, b)
  in
  let ta = {origin; map= AnnotationsMap.empty} in
  set_present present (set_nullable nullable ta)


let with_origin ta o = {ta with origin= o}

let from_item_annotation ia origin =
  let ta = const Nullable (Annotations.ia_is_nullable ia) origin in
  set_value Present (Annotations.ia_is_present ia) ta
