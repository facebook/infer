(*
* Copyright (c) 2014 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

module L = Logging
module F = Format
module P = Printf
open Utils

(** Module to represent annotations on types. *)

type t = {
  nullable : bool;
  present : bool;
  origin : TypeOrigin.t;
}

let equal ta1 ta2 =
  bool_equal ta1.nullable ta2.nullable &&
  bool_equal ta1.present ta2.present &&
  TypeOrigin.equal ta1.origin ta2.origin

let to_string ta =
  let nullable_s = if ta.nullable then " @Nullable" else "" in
  let present_s = if ta.present then " @Present" else "" in
  nullable_s ^ present_s

let join ta1 ta2 =
  let present = ta1.present && ta2.present in
  let ta' = match ta1.nullable, ta2.nullable with
    | false, true ->
        { ta2 with
          present;
          origin = TypeOrigin.join ta2.origin ta1.origin;
        }
    | true, false ->
        { ta1 with
          present;
          origin = TypeOrigin.join ta1.origin ta2.origin;
        }
    | _ ->
        { ta1 with
          present;
          origin = TypeOrigin.join ta1.origin ta2.origin;
        } in
  if ta' = ta1 then None else Some ta'

let get_value annotation ta = match annotation with
  | Annotations.Nullable -> ta.nullable
  | Annotations.Present -> ta.present

let set_value annotation ta b = match annotation with
  | Annotations.Nullable -> { ta with nullable = b }
  | Annotations.Present -> { ta with present = b }

let get_origin ta = ta.origin

let origin_is_fun_library ta = match get_origin ta with
  | TypeOrigin.Proc (pname, _, _, is_library) ->
      is_library
  | _ -> false

let descr_origin ta : TypeErr.origin_descr =
  let descr_opt = TypeOrigin.get_description ta.origin in
  match descr_opt with
  | None -> ("", None, None)
  | Some (str, loc_opt, sig_opt) -> ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)

let const annotation b origin =
  let nullable, present = match annotation with
    | Annotations.Nullable -> b, false
    | Annotations.Present -> false, b in
  { nullable; present; origin; }

let with_origin ta o =
  { ta with origin = o }

let from_item_annotation ia origin =
  let ann = const Annotations.Nullable (Annotations.ia_is_nullable ia) origin in
  set_value Annotations.Present ann (Annotations.ia_is_present ia)
