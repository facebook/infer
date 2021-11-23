(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO: Add other types as they are needed by translation (otherwise it's dead code). *)
type t = Any | Atom | Cons | Nil | Tuple of int | Map [@@deriving compare, yojson_of]

let pp f = function
  | Any ->
      Format.fprintf f "ErlangAny"
  | Atom ->
      Format.fprintf f "ErlangAtom"
  | Nil ->
      Format.fprintf f "ErlangNil"
  | Cons ->
      Format.fprintf f "ErlangCons"
  | Tuple arity ->
      Format.fprintf f "ErlangTuple%d" arity
  | Map ->
      Format.fprintf f "ErlangMap"


let to_string name = Format.asprintf "%a" pp name

let atom_value = "value"

let atom_hash = "hash"

let cons_head = "head"

let cons_tail = "tail"

let tuple_elem i = Printf.sprintf "elem%d" i

(* Tuple element indexing is one based *)
let tuple_field_names size = List.init size ~f:(fun i -> tuple_elem (i + 1))

let erlang_namespace = "erlang"
