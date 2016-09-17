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

module JavaSource = struct

  module SourceKind = struct
    type t =
      | SharedPreferences (** private data read from SharedPreferences *)
      | Footprint of AccessPath.t (** source that was read from the environment. *)
      | Other (** for testing or uncategorized sources *)

    let compare sk1 sk2 = match sk1, sk2 with
      | SharedPreferences, SharedPreferences -> 0
      | SharedPreferences, _ -> (-1)
      | _, SharedPreferences -> 1
      | Footprint ap1, Footprint ap2 -> AccessPath.compare ap1 ap2
      | Footprint _, _ -> (-1)
      | _, Footprint _ -> 1
      | Other, Other -> 0
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
    { kind = (SourceKind.Footprint ap); site; }

  let get site = match CallSite.pname site with
    | Procname.Java pname ->
        begin
          match Procname.java_get_class_name pname, Procname.java_get_method pname with
          | "android.content.SharedPreferences", "getString" ->
              [0, make SharedPreferences site]
          | "com.facebook.infer.models.InferTaint", "inferSecretSource" ->
              [0, make Other site]
          | _ ->
              []
        end
    | pname when Builtin.is_registered pname -> []
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  (** make a clone of [t] with a new call site *)
  let to_return t return_site =
    { t with site = return_site; }

  let compare src1 src2 =
    SourceKind.compare src1.kind src2.kind
    |> next CallSite.compare src1.site src2.site

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp_kind fmt (kind : kind) = match kind with
    | SharedPreferences -> F.fprintf fmt "SharedPreferences"
    | Footprint ap -> F.fprintf fmt "Footprint[%a]" AccessPath.pp ap
    | Other -> F.fprintf fmt "Other"

  let pp fmt s =
    F.fprintf fmt "%a(%a)" pp_kind s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)
end

module JavaSink = struct

  module SinkKind = struct
    type t =
      | Logging (** sink that logs one or more of its arguments *)
      | Other (** for testing or uncategorized sinks *)

    let compare snk1 snk2 = match snk1, snk2 with
      | Logging, Logging -> 0
      | Logging, _ -> (-1)
      | _, Logging -> 1
      | Other, Other -> 0
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

  let get site =
    (* taint all the inputs of [pname] *)
    let taint_all pname kind site ~report_reachable =
      IList.mapi
        (fun param_num _ -> Sink.make_sink_param (make kind site) param_num ~report_reachable)
        (Procname.java_get_parameters pname) in
    match CallSite.pname site with
    | Procname.Java pname ->
        begin
          match Procname.java_get_class_name pname, Procname.java_get_method pname with
          | "android.util.Log", ("d" | "e" | "i" | "println" | "v" | "w" | "wtf") ->
              taint_all pname Logging site ~report_reachable:true
          | "com.facebook.infer.models.InferTaint", "inferSensitiveSink" ->
              [Sink.make_sink_param (make Other site) 0 ~report_reachable:false]
          | _ ->
              []
        end
    | pname when Builtin.is_registered pname -> []
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  let to_callee t callee_site =
    { t with site = callee_site; }

  let compare snk1 snk2 =
    SinkKind.compare snk1.kind snk2.kind
    |> next CallSite.compare snk1.site snk2.site

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp_kind fmt (kind : kind) = match kind with
    | Logging -> F.fprintf fmt "Logging"
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
    module Source = JavaSource
    module Sink = JavaSink

    let should_report source sink =
      let open Source in
      let open Sink in
      match Source.kind source, Sink.kind sink with
      | SourceKind.Other, SinkKind.Other
      | SourceKind.SharedPreferences, SinkKind.Logging ->
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
