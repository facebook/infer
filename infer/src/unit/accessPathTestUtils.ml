(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

let make_base base_str =
  Var.of_pvar (Pvar.mk (Mangled.from_string base_str) Procname.empty_block), Typ.Tvoid

let make_field_access access_str =
  AccessPath.FieldAccess (Ident.create_fieldname (Mangled.from_string access_str) 0, Typ.Tvoid)

let make_array_access () =
  AccessPath.ArrayAccess Typ.Tvoid

let make_access_path base_str access_strs =
  make_base base_str, IList.map make_field_access access_strs
