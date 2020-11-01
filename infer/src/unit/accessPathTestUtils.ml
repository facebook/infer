(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let make_var var_str = Pvar.mk (Mangled.from_string var_str) Procname.empty_block

let make_base ?(typ = StdTyp.void) base_str = AccessPath.base_of_pvar (make_var base_str) typ

let make_fieldname field_name =
  assert (not (String.contains field_name '.')) ;
  Fieldname.make (Typ.Name.Java.from_string "SomeClass") field_name


let make_field_access access_str = AccessPath.FieldAccess (make_fieldname access_str)

let make_array_access typ = AccessPath.ArrayAccess (typ, [])

let make_access_path base_str access_strs =
  (make_base base_str, List.map ~f:make_field_access access_strs)
