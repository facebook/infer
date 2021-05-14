(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Clang | CIL | Erlang | Java [@@deriving compare, enumerate]

let equal = [%compare.equal: t]

let language_to_string =
  [(Clang, "C/C++/ObjC"); (Erlang, "Erlang"); (Java, "Java"); (CIL, "C#/.Net")]


let to_string lang = List.Assoc.find_exn language_to_string ~equal lang

let of_string s = List.Assoc.find (List.Assoc.inverse language_to_string) ~equal:String.equal s

(** Current language *)
let curr_language = ref Clang

let curr_language_is lang = equal !curr_language lang
