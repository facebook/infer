(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** utilities for importing JSON specifications of sources/sinks into Quandary *)

let get_kinds json =
  let open Yojson.Basic in
  match (Util.member "kinds" json, Util.member "kind" json) with
  | `Null, kind ->
      [Util.to_string kind]
  | kinds, kind ->
      (Util.to_string_option kind |> Option.to_list) @ Util.convert_each Util.to_string kinds


module Source = struct
  type t = {procedure: string; kinds: string list; index: string}

  let of_json = function
    | `List sources ->
        let parse_source json =
          let open Yojson.Basic in
          let procedure = Util.member "procedure" json |> Util.to_string in
          let kinds = get_kinds json in
          let index =
            Util.member "index" json |> Util.to_string_option |> Option.value ~default:"return"
          in
          {procedure; kinds; index}
        in
        List.map ~f:parse_source sources
    | _ ->
        []
end

module Sink = struct
  type t = {procedure: string; kinds: string list; index: string}

  let of_json = function
    | `List sinks ->
        let parse_sink json =
          let open Yojson.Basic in
          let procedure = Util.member "procedure" json |> Util.to_string in
          let kinds = get_kinds json in
          let index =
            Util.member "index" json |> Util.to_string_option |> Option.value ~default:"all"
          in
          {procedure; kinds; index}
        in
        List.map ~f:parse_sink sinks
    | _ ->
        []
end

module Sanitizer = struct
  type t = {procedure: string; kind: string}

  let of_json = function
    | `List sinks ->
        let parse_sanitizer json =
          let open Yojson.Basic in
          let procedure = Util.member "procedure" json |> Util.to_string in
          let kind =
            Util.member "kind" json |> Util.to_string_option |> Option.value ~default:"All"
          in
          {procedure; kind}
        in
        List.map ~f:parse_sanitizer sinks
    | _ ->
        []
end

module Endpoints = struct
  let of_json = function
    | `List endpoints ->
        let parse_endpoint = Yojson.Basic.Util.to_string in
        List.map ~f:parse_endpoint endpoints
    | _ ->
        []
end

let is_endpoint =
  let endpoints = lazy (String.Set.of_list (Endpoints.of_json Config.quandary_endpoints)) in
  fun name -> String.Set.mem (Lazy.force endpoints) name
