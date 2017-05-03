(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

module SourceKind = struct
  type t =
    | EnvironmentVariable (** source that was read from an environment variable *)
    | Other (** for testing or uncategorized sources *)
    | Unknown
  [@@deriving compare]

  let unknown = Unknown

  let of_string = function
    | "EnvironmentVariable" -> EnvironmentVariable
    | _ -> Other

  let external_sources =
    List.map
      ~f:(fun { QuandaryConfig.Source.procedure; kind; } ->
          QualifiedCppName.Match.of_fuzzy_qual_names [procedure], kind)
      (QuandaryConfig.Source.of_json Config.quandary_sources)

  (* return Some(source kind) if [procedure_name] is in the list of externally specified sources *)
  let get_external_source pname =
    let qualified_pname = Typ.Procname.get_qualifiers pname in
    List.find_map
      ~f:(fun (qualifiers, kind) ->
          if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname
          then Some (of_string kind)
          else None)
      external_sources

  let get pname _ = match pname with
    | Typ.Procname.ObjC_Cpp _ ->
        get_external_source pname
    | Typ.Procname.C _ ->
        begin
          match Typ.Procname.to_string pname with
          | "getenv" -> Some EnvironmentVariable
          | _ -> get_external_source pname
        end
    | Typ.Procname.Block _ ->
        None
    | pname when BuiltinDecl.is_declared pname ->
        None
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Typ.Procname.pp pname

  let get_tainted_formals pdesc _ =
    Source.all_formals_untainted pdesc

  let pp fmt = function
    | EnvironmentVariable -> F.fprintf fmt "EnvironmentVariable"
    | Other -> F.fprintf fmt "Other"
    | Unknown -> F.fprintf fmt "Unknown"
end

module CppSource = Source.Make(SourceKind)

module SinkKind = struct

  type t =
    | ShellExec (** shell exec function *)
    | Other (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let of_string = function
    | "ShellExec" -> ShellExec
    | _ -> Other

  let external_sinks =
    List.map
      ~f:(fun { QuandaryConfig.Sink.procedure; kind; index; } ->
          QualifiedCppName.Match.of_fuzzy_qual_names [procedure], kind, index)
      (QuandaryConfig.Sink.of_json Config.quandary_sinks)

  (* taint the nth parameter (0-indexed) *)
  let taint_nth n kind ~report_reachable =
    [kind, n, report_reachable]

  let taint_all actuals kind ~report_reachable =
    List.mapi
      ~f:(fun actual_num _ -> kind, actual_num, report_reachable)
      actuals

  (* return Some(sink kind) if [procedure_name] is in the list of externally specified sinks *)
  let get_external_sink pname actuals =
    let qualified_pname = Typ.Procname.get_qualifiers pname in
    List.find_map
      ~f:(fun (qualifiers, kind, index) ->
          if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname
          then
            let kind = of_string kind in
            try
              let n = int_of_string index in
              Some (taint_nth n kind ~report_reachable:true)
            with Failure _ ->
              (* couldn't parse the index, just taint everything *)
              Some (taint_all actuals kind ~report_reachable:true)
          else
            None)
      external_sinks

  let get pname actuals _ =
    match pname with
    | Typ.Procname.ObjC_Cpp _ ->
        Option.value (get_external_sink pname actuals) ~default:[]
    | Typ.Procname.C _ ->
        begin
          match Typ.Procname.to_string pname with
          | "execl" | "execlp" | "execle" | "execv" | "execvp" ->
              taint_all actuals ShellExec ~report_reachable:false
          | _ ->
              Option.value (get_external_sink pname actuals) ~default:[]
        end
    | Typ.Procname.Block _ ->
        []
    | pname when BuiltinDecl.is_declared pname ->
        []
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Typ.Procname.pp pname

  let pp fmt = function
    | ShellExec -> F.fprintf fmt "ShellExec"
    | Other -> F.fprintf fmt "Other"
end

module CppSink = Sink.Make(SinkKind)

include
  Trace.Make(struct
    module Source = CppSource
    module Sink = CppSink

    let should_report source sink =
      match Source.kind source, Sink.kind sink with
      | EnvironmentVariable, ShellExec ->
          true
      | Other, Other ->
          true
      | _ ->
          false
  end)
