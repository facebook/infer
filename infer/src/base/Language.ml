(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

type t = Clang | Java | Python [@@deriving compare]

let equal = [%compare.equal : t]

let language_to_string = [(Clang, "C/C++/ObjC"); (Java, "Java"); (Python, "python")]

let to_string lang = List.Assoc.find_exn language_to_string ~equal lang

let to_explicit_string = function Clang -> "Clang" | Java -> "Java" | Python -> "Python"

let of_string s = List.Assoc.find (List.Assoc.inverse language_to_string) ~equal:String.equal s

(** Current language *)
let curr_language = ref Clang

let curr_language_is lang = equal !curr_language lang
