(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format
module P = Printf

(** Module to represent annotations on types. *)

module AnnotationsMap = Caml.Map.Make (
  struct
    open Annotations
    type t = annotation [@@deriving compare]
  end)

type t = {
  map : bool AnnotationsMap.t;
  origin : TypeOrigin.t;
} [@@deriving compare]

let equal ta1 ta2 = 0 = compare ta1 ta2

let get_value ann ta =
  try
    AnnotationsMap.find ann ta.map
  with Not_found -> false

let set_value ann b ta =
  if get_value ann ta = b then ta
  else
    { ta with
      map = AnnotationsMap.add ann b ta.map; }

let get_nullable =
  get_value Annotations.Nullable

let get_present =
  get_value Annotations.Present

let set_nullable b =
  set_value Annotations.Nullable b

let set_present b =
  set_value Annotations.Present b

let to_string ta =
  let nullable_s = if get_nullable ta then " @Nullable" else "" in
  let present_s = if get_present ta then " @Present" else "" in
  nullable_s ^ present_s

let join ta1 ta2 =
  let choose_left = match get_nullable ta1, get_nullable ta2 with
    | false, true ->
        false
    | _ ->
        true in
  let ta_chosen, ta_other =
    if choose_left then ta1, ta2 else ta2, ta1 in
  let present = get_present ta1 && get_present ta2 in
  let origin = TypeOrigin.join ta_chosen.origin ta_other.origin in
  let ta' =
    set_present present
      { ta_chosen with
        origin; } in
  if ta' = ta1 then None else Some ta'

let get_origin ta = ta.origin

let origin_is_fun_library ta = match get_origin ta with
  | TypeOrigin.Proc proc_origin ->
      proc_origin.TypeOrigin.is_library
  | _ -> false

let descr_origin tenv ta : TypeErr.origin_descr =
  let descr_opt = TypeOrigin.get_description tenv ta.origin in
  match descr_opt with
  | None -> ("", None, None)
  | Some (str, loc_opt, sig_opt) -> ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)

let const annotation b origin =
  let nullable, present = match annotation with
    | Annotations.Nullable -> b, false
    | Annotations.Present -> false, b in
  let ta =
    { origin;
      map = AnnotationsMap.empty;
    } in
  set_present present (set_nullable nullable ta)

let with_origin ta o =
  { ta with origin = o }

let from_item_annotation ia origin =
  let ta = const Annotations.Nullable (Annotations.ia_is_nullable ia) origin in
  set_value Annotations.Present (Annotations.ia_is_present ia) ta
