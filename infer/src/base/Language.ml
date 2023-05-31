(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = Clang | CIL | Erlang | Hack | Java | Python [@@deriving compare, enumerate, equal]

let language_to_string =
  [ (Clang, "C/C++/ObjC")
  ; (Erlang, "Erlang")
  ; (Hack, "Hack")
  ; (Java, "Java")
  ; (CIL, "C#/.Net")
  ; (Python, "Python") ]


let to_string lang = List.Assoc.find_exn language_to_string ~equal lang

let curr_language = ref Clang

let curr_language_is lang = equal !curr_language lang
