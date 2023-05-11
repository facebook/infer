(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO: Add other types as they are needed by translation (otherwise it's dead code). *)
type t =
  | Any
  | Atom
  | Integer
  | Cons
  | Nil
  | Tuple of int
  | Map
  | GenServerPid of {module_name: string option}
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
  | GenServerPid {module_name} ->
      Format.fprintf f "ErlangGenServerPid_%s" (Option.value module_name ~default:"")


let to_string name = Format.asprintf "%a" pp name

let from_string s =
  let constr_opt format constr =
    try Scanf.sscanf s format (fun d -> Some (constr d)) with Scanf.Scan_failure _ -> None
  in
  match s with
  | "ErlangAny" | "Any" ->
      Some Any
  | "ErlangAtom" | "Atom" ->
      Some Atom
  | "ErlangCons" | "Cons" ->
      Some Cons
  | "ErlangInteger" | "Integer" ->
      Some Integer
  | "ErlangMap" | "Map" ->
      Some Map
  | "ErlangNil" | "Nil" ->
      Some Nil
  | _ ->
      let mk_tuple i = Tuple i in
      let mk_genserverpid m =
        match m with
        | "" ->
            GenServerPid {module_name= None}
        | _ ->
            GenServerPid {module_name= Some m}
      in
      let find_some li = List.find_map ~f:Fn.id li in
      find_some
        [ constr_opt "ErlangTuple%d%!" mk_tuple
        ; constr_opt "Tuple%d%!" mk_tuple
        ; constr_opt "ErlangGenServerPid_%s%!" mk_genserverpid
        ; constr_opt "GenServerPid_%s%!" mk_genserverpid ]


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

module Normalizer = struct
  let tuple_cache_size = 256

  let tuple = Array.init tuple_cache_size ~f:(fun size -> Tuple size)

  type nonrec t = t

  let normalize x = match x with Tuple size when size < tuple_cache_size -> tuple.(size) | x -> x
end
