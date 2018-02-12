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
  (* address of operator & *)
  | Dereference of t
  (* dereference operator * *)
  [@@deriving compare]

(** convert to an AccessPath.t, ignoring AddressOf and Dereference for now *)
let rec to_access_path t =
  match t with
  | Base base ->
      (base, [])
  | Offset (ae, acc) ->
      AccessPath.append (to_access_path ae) [acc]
  | AddressOf ae ->
      to_access_path ae
  | Dereference ae ->
      to_access_path ae


let of_access_path (base, accesses) =
  let rec add_access accesses ae =
    match accesses with [] -> ae | access :: rest -> add_access rest (Offset (ae, access))
  in
  add_access accesses (Base base)
