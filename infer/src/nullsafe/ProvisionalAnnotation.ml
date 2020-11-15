(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t =
  | Field of {field_name: Fieldname.t}
  | Method of Procname.Java.t
  | Param of {method_info: Procname.Java.t; num: int}
[@@deriving compare]

let pp fmt = function
  | Field {field_name} ->
      Format.fprintf fmt "Field(%a)" Fieldname.pp field_name
  | Method proc_name ->
      Format.fprintf fmt "Method(%a)" Procname.pp (Procname.Java proc_name)
  | Param {method_info; num} ->
      Format.fprintf fmt "Param(%d, %a)" num Procname.pp (Procname.Java method_info)
