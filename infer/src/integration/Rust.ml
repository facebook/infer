(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let json_file =
    match List.hd args with
    | Some json_file ->
        json_file
    | None ->
        L.die UserError "No arguments provided, missing argument json_file"
  in
  let json = Yojson.Basic.from_file json_file in
  match Charon.UllbcOfJson.crate_of_json json with
  | Ok _crate ->
      Printf.printf "success"
  | Error err ->
      L.die UserError "%s: %s" err (Yojson.Basic.to_string json)
