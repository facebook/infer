(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* OCaml needs to know not only that this has the same interface as [Q] but also that the types it
   defines are, in fact, the same as [Q] *)
include module type of struct
  include Q
end

val yojson_of_t : [%yojson_of: t]

val is_zero : t -> bool

val is_not_zero : t -> bool

val is_one : t -> bool

val is_minus_one : t -> bool

val not_equal : t -> t -> bool

(* the functions below shadow definitions in [Q] to give them safer types *)
[@@@warning "-unused-value-declaration"]

val to_int : t -> int option

val to_int32 : t -> int32 option

val to_int64 : t -> int64 option

val to_bigint : t -> Z.t option

val to_nativeint : t -> nativeint option

val is_rational : t -> bool
