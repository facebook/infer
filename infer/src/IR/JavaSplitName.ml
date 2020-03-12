(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {package: string option; type_name: string} [@@deriving compare, equal]

let make ?package type_name = {type_name; package}

(** Given a package.class_name string, it looks for the latest dot and split the string in two
    (package, class_name) *)
let of_string package_classname =
  match String.rsplit2 package_classname ~on:'.' with
  | Some (package, type_name) ->
      {type_name; package= Some package}
  | None ->
      {type_name= package_classname; package= None}


let package {package} = package

let type_name {type_name} = type_name

let java_lang_object = make ~package:"java.lang" "Object"

let java_lang_object_array = make ~package:"java.lang" "Object[]"

let java_lang_string = make ~package:"java.lang" "String"

let void = make "void"

let pp_type_verbosity ~verbose fmt = function
  | {package= Some package; type_name} when verbose ->
      F.fprintf fmt "%s.%s" package type_name
  | {type_name} ->
      F.pp_print_string fmt type_name
