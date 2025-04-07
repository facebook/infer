(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Subtypes *)

open! IStd
module F = Format

let list_to_string list =
  if List.is_empty list then "( sub )"
  else "- {" ^ String.concat ~sep:", " (List.map ~f:Typ.Name.name list) ^ "}"


type t' = Exact  (** denotes the current type only *) | Subtypes of Typ.Name.t list
[@@deriving compare, equal, hash, normalize]

(** denotes the current type and a list of types that are not their subtypes *)
type kind = CAST | INSTOF | NORMAL [@@deriving compare, equal, hash, normalize]

type t = t' * kind [@@deriving compare, equal, hash, normalize]

type result = No | Unknown | Yes [@@deriving compare, equal]

let max_result res1 res2 = if compare_result res1 res2 <= 0 then res2 else res1

let is_root_class class_name =
  match class_name with
  | Typ.JavaClass _ ->
      Typ.Name.equal class_name StdTyp.Name.Java.java_lang_object
  | Typ.CSharpClass _ ->
      Typ.Name.equal class_name StdTyp.Name.CSharp.system_object
  | _ ->
      false


(** check if c1 is a subclass of c2 *)
let check_subclass_tenv tenv c1 c2 : result =
  let rec loop best_result classnames : result =
    (* Check if the name c2 is found in the list of super types and
       keep the best results according to Yes > Unknown > No *)
    if equal_result best_result Yes then Yes
    else
      match classnames with
      | [] ->
          best_result
      | cn :: cns ->
          loop (max_result best_result (check cn)) cns
  and check cn : result =
    if Typ.Name.equal cn c2 then Yes
    else
      match Tenv.lookup tenv cn with
      | None when is_root_class cn ->
          No
      | None ->
          Unknown
      | Some {supers} ->
          loop No supers
  in
  if is_root_class c2 then Yes else check c1


module SubtypesMap = Stdlib.Map.Make (struct
  (* pair of subtypes *)
  type t = Typ.Name.t * Typ.Name.t [@@deriving compare]
end)

let check_subtype =
  let subtMap = ref SubtypesMap.empty in
  fun tenv c1 c2 : result ->
    try SubtypesMap.find (c1, c2) !subtMap
    with Stdlib.Not_found ->
      let is_subt = check_subclass_tenv tenv c1 c2 in
      subtMap := SubtypesMap.add (c1, c2) is_subt !subtMap ;
      is_subt


let is_known_subtype tenv c1 c2 : bool = equal_result (check_subtype tenv c1 c2) Yes

let flag_to_string flag = match flag with CAST -> "(cast)" | INSTOF -> "(instof)" | NORMAL -> ""

let pp f (t, flag) =
  if Config.print_types then
    match t with
    | Exact ->
        F.pp_print_string f (flag_to_string flag)
    | Subtypes list ->
        F.fprintf f "%s%s" (list_to_string list) (flag_to_string flag)


let exact = (Exact, NORMAL)

let all_subtypes = Subtypes []

let subtypes_cast = (all_subtypes, CAST)

let subtypes_instof = (all_subtypes, INSTOF)

let is_instof t = equal_kind (snd t) INSTOF
