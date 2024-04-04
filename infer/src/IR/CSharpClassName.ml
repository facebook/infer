(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [namespace = Some str] then [not (String.equal str "")]. [classname] appears first
    so that the comparator fails earlier *)
type t = {classname: string; namespace: string option}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let make ~namespace ~classname =
  match namespace with Some "" -> {namespace= None; classname} | _ -> {namespace; classname}


let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {classname= str; namespace= None}
  | Some ("", _) ->
      L.die InternalError "Empty namespace path in CSharp qualified classname.@."
  | Some (pkg, classname) ->
      {classname; namespace= Some pkg}


let to_string = function
  | {classname; namespace= None} ->
      classname
  | {classname; namespace= Some pkg} ->
      String.concat ~sep:"." [pkg; classname]


let pp fmt = function
  | {classname; namespace= None} ->
      F.pp_print_string fmt classname
  | {classname; namespace= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg classname


let classname {classname} = classname

let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (classname t)
