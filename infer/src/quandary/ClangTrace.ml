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
    | File (** source that was read from a file *)
    | Other (** for testing or uncategorized sources *)
    | Unknown
  [@@deriving compare]

  let unknown = Unknown

  let of_string = function
    | "EnvironmentVariable" -> EnvironmentVariable
    | "File" -> File
    | _ -> Other

  let external_sources =
    List.map
      ~f:(fun { QuandaryConfig.Source.procedure; kind; index; } ->
          QualifiedCppName.Match.of_fuzzy_qual_names [procedure], kind, index)
      (QuandaryConfig.Source.of_json Config.quandary_sources)

  (* return Some(source kind) if [procedure_name] is in the list of externally specified sources *)
  let get_external_source qualified_pname =
    let return = None in
    List.find_map
      ~f:(fun (qualifiers, kind, index) ->
          if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname
          then
            let source_index =
              try Some (int_of_string index)
              with Failure _ -> return in
            Some (of_string kind, source_index)
          else None)
      external_sources

  let get pname _ =
    let return = None in
    match pname with
    | Typ.Procname.ObjC_Cpp cpp_name ->
        let qualified_pname = Typ.Procname.get_qualifiers pname in
        begin
          match
            (QualifiedCppName.to_list
               (Typ.Name.unqualified_name (Typ.Procname.objc_cpp_get_class_type_name cpp_name))),
            Typ.Procname.get_method pname with
          | ["std"; ("basic_istream" | "basic_iostream")],
            ("getline" | "read" | "readsome" | "operator>>") ->
              Some (File, Some 1)
          | _ ->
              get_external_source qualified_pname
        end
    | Typ.Procname.C _ ->
        begin
          match Typ.Procname.to_string pname with
          | "getenv" ->
              Some (EnvironmentVariable, return)
          | _ ->
              get_external_source (Typ.Procname.get_qualifiers pname)
        end
    | Typ.Procname.Block _ ->
        None
    | pname when BuiltinDecl.is_declared pname ->
        None
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Typ.Procname.pp pname

  let get_tainted_formals pdesc _ =
    Source.all_formals_untainted pdesc

  let pp fmt kind =
    F.fprintf fmt
      (match kind with
       | EnvironmentVariable -> "EnvironmentVariable"
       | File -> "File"
       | Other -> "Other"
       | Unknown -> "Unknown")
end

module CppSource = Source.Make(SourceKind)

module SinkKind = struct

  type t =
    | Allocation (** memory allocation *)
    | ShellExec (** shell exec function *)
    | Other (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let of_string = function
    | "Allocation" -> Allocation
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
          | "execl" | "execlp" | "execle" | "execv" | "execve" | "execvp" | "system" ->
              taint_all actuals ShellExec ~report_reachable:false
          | "brk" | "calloc" | "malloc" | "realloc" | "sbrk" ->
              taint_all actuals Allocation ~report_reachable:false
          | _ ->
              Option.value (get_external_sink pname actuals) ~default:[]
        end
    | Typ.Procname.Block _ ->
        []
    | pname when BuiltinDecl.is_declared pname ->
        []
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Typ.Procname.pp pname

  let pp fmt kind =
    F.fprintf fmt
      (match kind with
       | Allocation -> "Allocation"
       | ShellExec -> "ShellExec"
       | Other -> "Other")
end

module CppSink = Sink.Make(SinkKind)

include
  Trace.Make(struct
    module Source = CppSource
    module Sink = CppSink

    let should_report source sink =
      match Source.kind source, Sink.kind sink with
      | (EnvironmentVariable | File), ShellExec ->
          (* untrusted data flowing to exec *)
          true
      | (EnvironmentVariable | File), Allocation ->
          (* untrusted data flowing to memory allocation *)
          true
      | Other, _
      | _, Other ->
          (* Other matches everything *)
          true
      | _ ->
          false
  end)
