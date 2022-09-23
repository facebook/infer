(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO: Add other types as they are needed by translation (otherwise it's dead code). *)
type t = Any | Atom | Integer | Cons | Nil | Tuple of int | Map
[@@deriving compare, equal, yojson_of, sexp, hash]

let pp f = function
  | Any ->
      Format.fprintf f "ErlangAny"
  | Atom ->
      Format.fprintf f "ErlangAtom"
  | Integer ->
      Format.fprintf f "ErlangInteger"
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

let atom_true = "true"

let atom_false = "false"

let calculate_hash atom = String.hash atom lsl 16

let integer_value = "value"

let cons_head = "head"

let cons_tail = "tail"

let tuple_elem i = Printf.sprintf "elem%d" i

(* Tuple element indexing is one based *)
let tuple_field_names size = List.init size ~f:(fun i -> tuple_elem (i + 1))

let erlang_namespace = "erlang"

let unsupported = "__unsupported"

let infer_erlang_namespace = "__infer__erlang"
