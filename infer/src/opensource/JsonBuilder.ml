(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = (string * Yojson.Basic.json) list

let empty = []

let add_int t ~key ~data = (key, `Int data) :: t

let _add_float t ~key ~data = (key, `Float data) :: t

let add_string t ~key ~data = (key, `String data) :: t

let add_string_opt t ~key ~data =
  match data with Some data -> add_string t ~key ~data | None -> t


let to_json t = Yojson.Basic.to_string (`Assoc t)
