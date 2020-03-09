(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")] *)
type t = {classname: string; package: string option} [@@deriving compare]

let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {classname= str; package= None}
  | Some ("", _) ->
      L.die InternalError "Empty package path in Java qualified classname.@."
  | Some (pkg, classname) ->
      {classname; package= Some pkg}


let to_string = function
  | {classname; package= None} ->
      classname
  | {classname; package= Some pkg} ->
      String.concat ~sep:"." [pkg; classname]


let pp fmt = function
  | {classname; package= None} ->
      F.pp_print_string fmt classname
  | {classname; package= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg classname


let package {package} = package

let classname {classname} = classname

let is_external_via_config t =
  let package = package t in
  Option.exists ~f:Config.java_package_is_external package


let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (classname t)
