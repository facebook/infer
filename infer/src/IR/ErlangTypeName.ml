(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO: Add other types as they are needed by translation (otherwise it's dead code). *)
type t = Any | Cons | Nil [@@deriving compare, yojson_of]

let pp f = function
  | Any ->
      Format.fprintf f "ErlangAny"
  | Nil ->
      Format.fprintf f "ErlangNil"
  | Cons ->
      Format.fprintf f "ErlangCons"


let to_string name = Format.asprintf "%a" pp name
