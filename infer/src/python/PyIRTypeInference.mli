(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type field_type =
  | Class of {class_name: string; supers: string list}
  | Fundef of {qual_name: string}
  | Import of {module_name: string}
  | ImportFrom of {module_name: string; attr_name: string}

type field_decl = {name: string; typ: field_type}

type struct_kind = Global | ClassCompanion of {supers: string list}

type struct_type = {name: string; kind: struct_kind; fields: field_decl list}

type t = struct_type list

val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]

val gen_module_default_type : PyIR.Module.t -> t option

val gen_module_default_type_debug : PyIR.Module.t -> unit [@@warning "-unused-value-declaration"]
