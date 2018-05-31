(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val make_var : string -> Pvar.t

val make_fieldname : string -> Typ.Fieldname.t

val make_base : ?typ:Typ.t -> string -> AccessPath.base

val make_field_access : string -> AccessPath.access

val make_array_access : Typ.t -> AccessPath.access

val make_access_path : string -> string list -> AccessPath.t
