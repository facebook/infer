(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t =
  | Base of AccessPath.base
  | Offset of t * AccessPath.access
  (* field/array access *)
  | AddressOf of t
  (* & *)
  | Dereference of t
  (* * *)
  [@@deriving compare]

val to_access_path : t -> AccessPath.t

val of_access_path : AccessPath.t -> t
