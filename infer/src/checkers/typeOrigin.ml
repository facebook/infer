(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module P = Printf
open Utils


(** Describe the origin of values propagated by the checker. *)


type proc_origin =
  Procname.t * Sil.location * Annotations.annotated_signature * bool (* is_library *)

type t =
  | Const of Sil.location
  | Field of Ident.fieldname * Sil.location
  | Formal of string
  | Proc of proc_origin
  | New
  | ONone
  | Undef

let equal o1 o2 = match o1, o2 with
  | Const loc1, Const loc2 ->
      Sil.loc_equal loc1 loc2
  | Const _, _
  | _, Const _ -> false
  | Field (fn1, loc1), Field (fn2, loc2) ->
      Ident.fieldname_equal fn1 fn2 &&
      Sil.loc_equal loc1 loc2
  | Field _, _
  | _, Field _ -> false
  | Formal s1, Formal s2 ->
      string_equal s1 s2
  | Formal _, _
  | _, Formal _ -> false
  | Proc (pn1, loc1, as1, b1), Proc (pn2, loc2, as2, b2) ->
      Procname.equal pn1 pn2 &&
      Sil.loc_equal loc1 loc2 &&
      Annotations.equal as1 as2 &&
      bool_equal b1 b2
  | Proc _, _
  | _, Proc _ -> false
  | New, New -> true
  | New, _
  | _, New -> false
  | ONone, ONone -> true
  | ONone, _
  | _, ONone -> false
  | Undef, Undef -> true

let to_string = function
  | Const loc -> "Const"
  | Field (fn, loc) -> "Field " ^ Ident.fieldname_to_simplified_string fn
  | Formal s -> "Formal " ^ s
  | Proc (pname, _, _, _) ->
      Printf.sprintf
        "Fun %s"
        (Procname.to_simplified_string pname)
  | New -> "New"
  | ONone -> "ONone"
  | Undef -> "Undef"

let get_description origin =
  let atline loc =
    " at line " ^ (string_of_int loc.Sil.line) in
  match origin with
  | Const loc ->
      Some ("null constant" ^ atline loc, Some loc, None)
  | Field (fn, loc) ->
      Some ("field " ^ Ident.fieldname_to_simplified_string fn ^ atline loc, Some loc, None)
  | Formal s ->
      Some ("method parameter " ^ s, None, None)
  | Proc (pname, loc, signature, is_library) ->
      let strict = match TypeErr.Strict.signature_get_strict signature with
        | Some ann ->
            let str = "@Strict" in
            (match ann.Sil.parameters with
             | par1 :: _ -> Printf.sprintf "%s(%s) " str par1
             | [] -> Printf.sprintf "%s " str)
        | None -> "" in
      let description = Printf.sprintf
          "call to %s%s %s"
          strict
          (Procname.to_simplified_string pname)
          (atline loc) in
      Some (description, Some loc, Some signature)
  | New
  | ONone
  | Undef -> None


let join o1 o2 = match o1, o2 with (* left priority *)
  | Undef, _
  | _, Undef -> Undef
  | _ -> o1
