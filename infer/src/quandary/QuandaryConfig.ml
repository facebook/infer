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

(** utilities for importing JSON specifications of sources/sinks into Quandary *)

module Source = struct
  type t = { procedure : Str.regexp; kind : string; }

  let of_json = function
    | `List sources ->
        let parse_source json =
          let open Yojson.Basic.Util in
          let procedure = json |> member "procedure" |> to_string |> Str.regexp in
          let kind = json |> member "kind" |> to_string in
          { procedure; kind; } in
        List.map ~f:parse_source sources
    | _ ->
        []
end

module Sink = struct
  type t = { procedure : Str.regexp; kind : string; index : string}

  let of_json = function
    | `List sinks ->
        let parse_sink json =
          let open Yojson.Basic.Util in
          let procedure = json |> member "procedure" |> to_string |> Str.regexp in
          let kind = json |> member "kind" |> to_string in
          let index = json |> member "index" |> to_string in
          { procedure; kind; index; } in
        List.map ~f:parse_sink sinks
    | _ ->
        []
end
