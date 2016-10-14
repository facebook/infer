(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module CppSource = struct

  module SourceKind = struct
    type t =
      | Footprint of AccessPath.t (** source that was read from the environment. *)
      | EnvironmentVariable (** source that was read from an environment variable *)
      | Other (** for testing or uncategorized sources *)

    let compare sk1 sk2 = match sk1, sk2 with
      | Footprint ap1, Footprint ap2 -> AccessPath.compare ap1 ap2
      | _ -> tags_compare sk1 sk2
  end

  type kind = SourceKind.t

  type t =
    {
      kind : kind;
      site : CallSite.t;
    }

  let is_footprint t = match t.kind with
    | SourceKind.Footprint _ -> true
    | _ -> false

  let get_footprint_access_path t = match t.kind with
    | SourceKind.Footprint access_path -> Some access_path
    | _ -> None

  let call_site t =
    t.site

  let kind t =
    t.kind

  let make kind site =
    { site; kind; }

  let make_footprint ap site =
    { kind = SourceKind.Footprint ap; site; }

  let get site = match CallSite.pname site with
    | (Procname.ObjC_Cpp cpp_pname) as pname ->
        begin
          match Procname.objc_cpp_get_class_name cpp_pname, Procname.get_method pname with
          (* placeholder for real sources *)
          | "Namespace here", "method name here" -> None
          | _ -> None
        end
    | (C _) as pname ->
        begin
          match Procname.to_string pname with
          | "getenv" -> Some (make EnvironmentVariable site)
          | "__infer_taint_source" -> Some (make Other site)
          | _ -> None
        end
    | pname when Builtin.is_registered pname ->
        None
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Procname.pp pname

  let compare src1 src2 =
    SourceKind.compare src1.kind src2.kind
    |> next CallSite.compare src1.site src2.site

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp_kind fmt (kind : kind) = match kind with
    | Footprint ap -> F.fprintf fmt "Footprint[%a]" AccessPath.pp ap
    | EnvironmentVariable -> F.fprintf fmt "EnvironmentVariable"
    | Other -> F.fprintf fmt "Other"

  let pp fmt s =
    F.fprintf fmt "%a(%a)" pp_kind s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)
end

module CppSink = struct

  module SinkKind = struct
    type t =
      | ShellExec (** shell exec function *)
      | Other (** for testing or uncategorized sinks *)

    let compare snk1 snk2 = tags_compare snk1 snk2
  end

  type kind = SinkKind.t

  type t =
    {
      kind : kind;
      site : CallSite.t;
    }

  let kind t =
    t.kind

  let call_site t =
    t.site

  let make kind site =
    { kind; site; }

  let get site actuals =
    let taint_all actuals sink ~report_reachable =
      IList.mapi
        (fun actual_num _ -> Sink.make_sink_param sink actual_num ~report_reachable)
        actuals in
    match CallSite.pname site with
    | (Procname.ObjC_Cpp cpp_pname) as pname ->
        begin
          match Procname.objc_cpp_get_class_name cpp_pname, Procname.get_method pname with
          (* placeholder for real sinks *)
          | "Namespace here", "method name here" -> []
          | _ -> []
        end
    | (C _ as pname) ->
        begin
          match Procname.to_string pname with
          | "execl" | "execlp" | "execle" | "execv" | "execvp" ->
              taint_all actuals (make ShellExec site) ~report_reachable:false
          | "__infer_taint_sink" ->
              [Sink.make_sink_param (make Other site) 0 ~report_reachable:false]
          | _ ->
              []
        end
    | pname when Builtin.is_registered pname ->
        []
    | pname ->
        failwithf "Non-C++ procname %a in C++ analysis@." Procname.pp pname

  let to_callee t callee_site =
    { t with site = callee_site; }

  let compare snk1 snk2 =
    SinkKind.compare snk1.kind snk2.kind
    |> next CallSite.compare snk1.site snk2.site

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp_kind fmt (kind : kind) = match kind with
    | ShellExec -> F.fprintf fmt "ShellExec"
    | Other -> F.fprintf fmt "Other"

  let pp fmt s =
    F.fprintf fmt "%a(%a)" pp_kind s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)
end

include
  Trace.Make(struct
    module Source = CppSource
    module Sink = CppSink

    let should_report source sink =
      let open Source in
      let open Sink in
      match Source.kind source, Sink.kind sink with
      | SourceKind.EnvironmentVariable, SinkKind.ShellExec ->
          true
      | SourceKind.Other, SinkKind.Other ->
          true
      | _ ->
          false

    let get_reportable_exn source sink passthroughs =
      let pp_error fmt () =
        F.fprintf
          fmt
          "Error: %a -> %a via %a"
          Source.pp source Sink.pp sink Passthrough.Set.pp passthroughs in
      let msg = "QUANDARY_TAINT_ERROR" in
      let description = pp_to_string pp_error () in
      Exceptions.Checkers (msg, Localise.verbatim_desc description)

  end)
