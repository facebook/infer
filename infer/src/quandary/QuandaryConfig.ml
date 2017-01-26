(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(** utilities for importing JSON specifications of sources/sinks into Quandary*)

module Source = struct
  type t = { procedure : string; kind : string; }

  let of_json = function
    | `List sources ->
        let parse_source json =
          let open Yojson.Basic.Util in
          let procedure = json |> member "procedure" |> to_string in
          let kind = json |> member "kind" |> to_string in
          { procedure; kind; } in
        IList.map parse_source sources
    | _ ->
        []

  let pp fmt { procedure; kind; } =
    F.fprintf fmt "Procedure: %s Kind: %s" procedure kind
end
