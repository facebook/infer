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

module JavaSource = struct

  module Kind = struct
    type t =
      | PrivateData (** private user or device-specific data *)
      | Footprint of AccessPath.t (** source that was read from the environment. *)
      | Intent
      | Other (** for testing or uncategorized sources *)
    [@@deriving compare]

    let pp fmt = function
      | Intent -> F.fprintf fmt "Intent"
      | PrivateData -> F.fprintf fmt "PrivateData"
      | Footprint ap -> F.fprintf fmt "Footprint[%a]" AccessPath.pp ap
      | Other -> F.fprintf fmt "Other"
  end

  type t =
    {
      kind : Kind.t;
      site : CallSite.t;
    } [@@deriving compare]

  let is_footprint t = match t.kind with
    | Kind.Footprint _ -> true
    | _ -> false

  let get_footprint_access_path t = match t.kind with
    | Kind.Footprint access_path -> Some access_path
    | _ -> None

  let call_site t =
    t.site

  let kind t =
    t.kind

  let make kind site =
    { site; kind; }

  let make_footprint ap site =
    { kind = Kind.Footprint ap; site; }

  let get site = match CallSite.pname site with
    | Procname.Java pname ->
        begin
          match Procname.java_get_class_name pname, Procname.java_get_method pname with
          | "android.content.Intent", ("parseUri" | "parseIntent") ->
              Some (make Intent site)
          | "android.content.SharedPreferences", "getString" ->
              Some (make PrivateData site)
          | "android.location.Location",
            ("getAltitude" | "getBearing" | "getLatitude" | "getLongitude" | "getSpeed") ->
              Some (make PrivateData site)
          | "android.telephony.TelephonyManager",
            ("getDeviceId" |
             "getLine1Number" |
             "getSimSerialNumber" |
             "getSubscriberId" |
             "getVoiceMailNumber") ->
              Some (make PrivateData site)
          | "com.facebook.infer.builtins.InferTaint", "inferSecretSource" ->
              Some (make Other site)
          | _ ->
              None
        end
    | pname when BuiltinDecl.is_declared pname -> None
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  let with_callsite t callee_site =
    { t with site = callee_site; }

  let pp fmt s =
    F.fprintf fmt "%a(%a)" Kind.pp s.kind CallSite.pp s.site

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)
end

module JavaSink = struct

  module Kind = struct
    type t =
      | Intent (** sink that trusts an Intent *)
      | Logging (** sink that logs one or more of its arguments *)
      | Other (** for testing or uncategorized sinks *)
    [@@deriving compare]

    let pp fmt = function
      | Intent -> F.fprintf fmt "Intent"
      | Logging -> F.fprintf fmt "Logging"
      | Other -> F.fprintf fmt "Other"
  end

  type t =
    {
      kind : Kind.t;
      site : CallSite.t;
    } [@@deriving compare]

  let kind t =
    t.kind

  let call_site t =
    t.site

  let make kind site =
    { kind; site; }

  let get site actuals =
    (* taint all the inputs of [pname]. for non-static procedures, taints the "this" parameter only
       if [taint_this] is true. *)
    let taint_all ?(taint_this=false) kind site ~report_reachable =
      let actuals_to_taint, offset =
        if Procname.java_is_static (CallSite.pname site) || taint_this
        then actuals, 0
        else IList.tl actuals, 1 in
      let sink = make kind site in
      IList.mapi
        (fun param_num _ -> Sink.make_sink_param sink (param_num + offset) ~report_reachable)
        actuals_to_taint in
    (* taint the nth non-"this" parameter (0-indexed) *)
    let taint_nth n kind site ~report_reachable =
      let first_index = if Procname.java_is_static (CallSite.pname site) then n else n + 1 in
      [Sink.make_sink_param (make kind site) first_index ~report_reachable] in
    match CallSite.pname site with
    | Procname.Java pname ->
        begin
          match Procname.java_get_class_name pname, Procname.java_get_method pname with
          | ("android.app.Activity" | "android.content.ContextWrapper" | "android.content.Context"),
            ("bindService" |
             "sendBroadcast" |
             "sendBroadcastAsUser" |
             "sendOrderedBroadcast" |
             "sendStickyBroadcast" |
             "sendStickyBroadcastAsUser" |
             "sendStickyOrderedBroadcast" |
             "sendStickyOrderedBroadcastAsUser" |
             "startActivities" |
             "startActivity" |
             "startActivityForResult" |
             "startActivityIfNeeded" |
             "startNextMatchingActivity" |
             "startService") ->
              taint_nth 0 Intent site ~report_reachable:true
          | "android.app.Activity", ("startActivityFromChild" | "startActivityFromFragment") ->
              taint_nth 1 Intent site ~report_reachable:true
          | "android.util.Log", ("e" | "println" | "w" | "wtf") ->
              taint_all Logging site ~report_reachable:true
          | "com.facebook.infer.builtins.InferTaint", "inferSensitiveSink" ->
              [Sink.make_sink_param (make Other site) 0 ~report_reachable:false]
          | _ ->
              []
        end
    | pname when BuiltinDecl.is_declared pname -> []
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  let with_callsite t callee_site =
    { t with site = callee_site; }

  let pp fmt s =
    F.fprintf fmt "%a(%a)" Kind.pp s.kind CallSite.pp s.site

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
      match Source.kind source, Sink.kind sink with
      | Source.Kind.Other, Sink.Kind.Other
      | Source.Kind.PrivateData, Sink.Kind.Logging ->
          true
      | Source.Kind.Intent, Sink.Kind.Intent ->
          true
      | _ ->
          false
  end)
