(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = {integers: int String.Map.t; floats: float String.Map.t; strings: string String.Map.t}

let empty = {integers= String.Map.empty; floats= String.Map.empty; strings= String.Map.empty}

let add_int ({integers} as t) ~key ~data = {t with integers= String.Map.set integers ~key ~data}

let add_float ({floats} as t) ~key ~data = {t with floats= String.Map.set floats ~key ~data}

let add_string ({strings} as t) ~key ~data = {t with strings= String.Map.set strings ~key ~data}

let add_string_opt t ~key ~data =
  match data with Some data -> add_string t ~key ~data | None -> t


let yojson_of_integers integers =
  let f ~key ~data acc = (key, `Int data) :: acc in
  `Assoc (String.Map.fold integers ~init:[] ~f)


let yojson_of_floats floats =
  let f ~key ~data acc = (key, `Float data) :: acc in
  `Assoc (String.Map.fold floats ~init:[] ~f)


let yojson_of_strings strings =
  let f ~key ~data acc = (key, `String data) :: acc in
  `Assoc (String.Map.fold strings ~init:[] ~f)


let to_json {integers; floats; strings} =
  `Assoc
    [ ("int", yojson_of_integers integers)
    ; ("double", yojson_of_floats floats)
    ; ("normal", yojson_of_strings strings) ]
  |> Yojson.Basic.to_string
