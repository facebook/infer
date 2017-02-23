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


module Kind = struct
  type t =
    | EnvironmentVariable (** source that was read from an environment variable *)
    | Other (** for testing or uncategorized sources *)
    | Unknown
  [@@deriving compare]

  let unknown = Unknown

  let get pname _ = match pname with
    | (Procname.ObjC_Cpp cpp_pname) as pname ->
        begin
          match Procname.objc_cpp_get_class_name cpp_pname, Procname.get_method pname with
          | "InferTaint", "source" -> Some Other
          | _ -> None
        end
    | (Procname.C _) as pname ->
        begin
          match Procname.to_string pname with
          | "getenv" -> Some EnvironmentVariable
          | "__infer_taint_source" -> Some Other
          | _ -> None
        end
    | Procname.Block _ ->
        None
    | pname when BuiltinDecl.is_declared pname ->
        None
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Procname.pp pname

  let get_tainted_formals pdesc _ =
    Source.all_formals_untainted pdesc

  let pp fmt = function
    | EnvironmentVariable -> F.fprintf fmt "EnvironmentVariable"
    | Other -> F.fprintf fmt "Other"
    | Unknown -> F.fprintf fmt "Unknown"
end

module CppSource = Source.Make(Kind)

module SinkKind = struct

  type t =
    | ShellExec (** shell exec function *)
    | Other (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let get pname actuals _ =
    let taint_all actuals kind ~report_reachable =
      List.mapi
        ~f:(fun actual_num _ -> kind, actual_num, report_reachable)
        actuals in
    match pname with
    | (Procname.ObjC_Cpp cpp_pname) as pname ->
        begin
          match Procname.objc_cpp_get_class_name cpp_pname, Procname.get_method pname with
          | "InferTaint", "sink:" -> taint_all actuals Other ~report_reachable:true
          | _ -> []
        end
    | Procname.C _ ->
        begin
          match Procname.to_string pname with
          | "execl" | "execlp" | "execle" | "execv" | "execvp" ->
              taint_all actuals ShellExec ~report_reachable:false
          | "__infer_taint_sink" ->
              [Other, 0, false]
          | _ ->
              []
        end
    | Procname.Block _ ->
        []
    | pname when BuiltinDecl.is_declared pname ->
        []
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Procname.pp pname

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
