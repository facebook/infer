(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Describe the origin of values propagated by the checker. *)

type proc_origin =
  { pname: Typ.Procname.t
  ; loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_library: bool }
[@@deriving compare]

type t =
  | Const of Location.t
  | Field of t * Typ.Fieldname.t * Location.t
  | Formal of Mangled.t
  | Proc of proc_origin
  | New
  | ONone
  | Undef
[@@deriving compare]

let equal = [%compare.equal: t]

let rec to_string = function
  | Const _ ->
      "Const"
  | Field (o, fn, _) ->
      "Field " ^ Typ.Fieldname.to_simplified_string fn ^ " (inner: " ^ to_string o ^ ")"
  | Formal s ->
      "Formal " ^ Mangled.to_string s
  | Proc po ->
      Printf.sprintf "Fun %s" (Typ.Procname.to_simplified_string po.pname)
  | New ->
      "New"
  | ONone ->
      "ONone"
  | Undef ->
      "Undef"


let get_description origin =
  let atline loc = " at line " ^ string_of_int loc.Location.line in
  match origin with
  | Const loc ->
      Some ("null constant" ^ atline loc, Some loc, None)
  | Field (_, fn, loc) ->
      Some ("field " ^ Typ.Fieldname.to_simplified_string fn ^ atline loc, Some loc, None)
  | Formal s ->
      Some ("method parameter " ^ Mangled.to_string s, None, None)
  | Proc po ->
      let modelled_in =
        if Models.is_modelled_nullable po.pname then " modelled in " ^ ModelTables.this_file
        else ""
      in
      let description =
        Printf.sprintf "call to %s%s%s"
          (Typ.Procname.to_simplified_string po.pname)
          modelled_in (atline po.loc)
      in
      Some (description, Some po.loc, Some po.annotated_signature)
  | New | ONone | Undef ->
      None


let join o1 o2 =
  match (o1, o2) with
  (* left priority *)
  | Undef, _ | _, Undef ->
      Undef
  | Field _, (Const _ | Formal _ | Proc _ | New) ->
      (* low priority to Field, to support field initialization patterns *)
      o2
  | _ ->
      o1
