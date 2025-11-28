(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Var = PulseFormulaVar

(** Domain for tracking dynamic type of variables via positive and negative instanceof constraints
*)

(* *Intended* invariant is that these all be normalised wrt alias expansion *)
type dynamic_type_data = {typ: Typ.t; source_file: (SourceFile.t[@yojson.opaque]) option}
[@@deriving compare, equal, yojson_of]

type instance_fact =
  | Known of dynamic_type_data
  | Unknown of {below: Typ.t list; notbelow: Typ.t list}
[@@deriving compare, equal, yojson_of]

let pp_instance_fact fmt inf =
  match inf with
  | Known {typ} when Language.curr_language_is Python ->
      F.fprintf fmt "%a" (Typ.pp_full Pp.text) typ
  | Known {typ; source_file} ->
      F.fprintf fmt "%a, SourceFile %a" (Typ.pp_full Pp.text) typ (Pp.option SourceFile.pp)
        source_file
  | Unknown {below; notbelow} ->
      let tlist_pp = Pp.seq ~sep:"," (Typ.pp_full Pp.text) in
      F.fprintf fmt "{%a \\ %a}" tlist_pp below tlist_pp notbelow


type t = instance_fact Var.Map.t [@@deriving compare, equal, yojson_of]

let pp_with_pp_var pp_var fmt m =
  Pp.collection ~sep:"∧"
    ~fold:(IContainer.fold_of_pervasives_map_fold Var.Map.fold)
    (fun fmt (var, inf) -> F.fprintf fmt "%a:%a" pp_var var pp_instance_fact inf)
    fmt m


let is_subtype t1 t2 =
  (* Don't really like doing dereference on every call *)
  let tenv = PulseContext.tenv_exn () in
  match (Typ.name t1, Typ.name t2) with
  | Some n1, Some n2 ->
      PatternMatch.is_subtype tenv n1 n2
  | _, _ ->
      Typ.equal t1 t2


let is_final t =
  let tenv = PulseContext.tenv_exn () in
  match Typ.name t with
  | None ->
      false
  | Some (ErlangType _) ->
      true
      (* TODO: Actually wrong for Any, if we ever get that, and maybe there are subtypes of bool?? *)
  | Some tn -> (
    match Tenv.lookup tenv tn with None -> false | Some {annots} -> Annot.Item.is_final annots )


(* The following are a bit conservative, as they currently only return true for Hack types
     TODO: check soundness of reasoning for other languages and extend the logic
  *)
let is_concrete_or_abstract t =
  let tenv = PulseContext.tenv_exn () in
  match Typ.name t with
  | None ->
      false
  | Some (ErlangType _) ->
      true
  | Some (JavaClass _ as tn) -> (
    match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_not_java_interface str )
  | Some (HackClass _ as tn) -> (
    match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_hack_class str )
  | _ ->
      false


let is_abstract t =
  let tenv = PulseContext.tenv_exn () in
  match Typ.name t with
  | None ->
      false
  | Some tn -> (
    match Tenv.lookup tenv tn with None -> false | Some str -> Struct.is_hack_abstract_class str )
