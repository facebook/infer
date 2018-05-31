(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** utilities for importing JSON specifications of sources/sinks into Quandary *)

module Source = struct
  type t = {procedure: string; kind: string; index: string}

  let of_json = function
    | `List sources ->
        let parse_source json =
          let open Yojson.Basic in
          let procedure = Util.member "procedure" json |> Util.to_string in
          let kind = Util.member "kind" json |> Util.to_string in
          let index =
            Util.member "index" json |> Util.to_string_option |> Option.value ~default:"return"
          in
          {procedure; kind; index}
        in
        List.map ~f:parse_source sources
    | _ ->
        []
end

module Sink = struct
  type t = {procedure: string; kind: string; index: string}

  let of_json = function
    | `List sinks ->
        let parse_sink json =
          let open Yojson.Basic in
          let procedure = Util.member "procedure" json |> Util.to_string in
          let kind = Util.member "kind" json |> Util.to_string in
          let index =
            Util.member "index" json |> Util.to_string_option |> Option.value ~default:"all"
          in
          {procedure; kind; index}
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

module Endpoint = struct
  type t = string

  let of_json = function
    | `List endpoints ->
        let parse_endpoint = Yojson.Basic.Util.to_string in
        List.map ~f:parse_endpoint endpoints
    | _ ->
        []
end
