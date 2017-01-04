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
    | PrivateData (** private user or device-specific data *)
    | Intent
    | Other (** for testing or uncategorized sources *)
    | Unknown
  [@@deriving compare]

  let unknown = Unknown

  let get = function
    | Procname.Java pname ->
        begin
          match Procname.java_get_class_name pname, Procname.java_get_method pname with
          | "android.content.Intent", "getStringExtra" ->
              Some Intent
          | "android.content.SharedPreferences", "getString" ->
              Some PrivateData
          | "android.location.Location",
            ("getAltitude" | "getBearing" | "getLatitude" | "getLongitude" | "getSpeed") ->
              Some PrivateData
          | "android.telephony.TelephonyManager",
            ("getDeviceId" |
             "getLine1Number" |
             "getSimSerialNumber" |
             "getSubscriberId" |
             "getVoiceMailNumber") ->
              Some PrivateData
          | "com.facebook.infer.builtins.InferTaint", "inferSecretSource" ->
              Some Other
          | _ ->
              None
        end
    | pname when BuiltinDecl.is_declared pname -> None
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  let get_tainted_formals pdesc =
    let make_untainted (name, typ) =
      name, typ, None in
    let taint_formals_with_types type_strs kind formals =
      let taint_formal_with_types ((formal_name, formal_typ) as formal) =
        let matches_classname typ typ_str = match typ with
          | Typ.Tptr (Tstruct typename, _) -> Typename.name typename = typ_str
          | _ -> false in
        if IList.mem matches_classname formal_typ type_strs
        then
          formal_name, formal_typ, Some kind
        else
          make_untainted formal in
      IList.map taint_formal_with_types formals in

    let formals = Procdesc.get_formals pdesc in
    match Procdesc.get_proc_name pdesc with
    | Procname.Java java_pname ->
        begin
          match Procname.java_get_class_name java_pname, Procname.java_get_method java_pname with
          | "codetoanalyze.java.quandary.TaintedFormals", "taintedContextBad" ->
              taint_formals_with_types
                ["java.lang.Integer"; "java.lang.String"]
                Other
                formals
          | _ ->
              Source.all_formals_untainted pdesc
        end
    | procname ->
        failwithf
          "Non-Java procedure %a where only Java procedures are expected"
          Procname.pp procname

  let pp fmt = function
    | Intent -> F.fprintf fmt "Intent"
    | PrivateData -> F.fprintf fmt "PrivateData"
    | Other -> F.fprintf fmt "Other"
    | Unknown -> F.fprintf fmt "Unknown"
end

module JavaSource = Source.Make(SourceKind)

module SinkKind = struct
  type t =
    | Intent (** sink that trusts an Intent *)
    | Logging (** sink that logs one or more of its arguments *)
    | Other (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let get pname actuals =
    (* taint all the inputs of [pname]. for non-static procedures, taints the "this" parameter only
       if [taint_this] is true. *)
    let taint_all ?(taint_this=false) kind ~report_reachable =
      let actuals_to_taint, offset =
        if Procname.java_is_static pname || taint_this
        then actuals, 0
        else IList.tl actuals, 1 in
      IList.mapi
        (fun param_num _ -> kind, param_num + offset, report_reachable)
        actuals_to_taint in
    (* taint the nth non-"this" parameter (0-indexed) *)
    let taint_nth n kind ~report_reachable =
      let first_index = if Procname.java_is_static pname then n else n + 1 in
      [kind, first_index, report_reachable] in
    match pname with
    | Procname.Java java_pname ->
        begin
          match Procname.java_get_class_name java_pname, Procname.java_get_method java_pname with
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
              taint_nth 0 Intent ~report_reachable:true
          | "android.app.Activity", ("startActivityFromChild" | "startActivityFromFragment") ->
              taint_nth 1 Intent ~report_reachable:true
          | "android.content.Intent",
            ("fillIn" |
             "makeMainSelectorActivity" |
             "parseIntent" |
             "parseUri" |
             "replaceExtras" |
             "setAction" |
             "setClassName" |
             "setData" |
             "setDataAndNormalize" |
             "setDataAndType" |
             "setDataAndTypeAndNormalize" |
             "setPackage" |
             "setSelector" |
             "setType" |
             "setTypeAndNormalize") ->
              taint_all Intent ~report_reachable:true
          | "android.util.Log", ("e" | "println" | "w" | "wtf") ->
              taint_all Logging ~report_reachable:true
          | "com.facebook.infer.builtins.InferTaint", "inferSensitiveSink" ->
              [Other, 0, false]
          | _ ->
              []
        end
    | pname when BuiltinDecl.is_declared pname -> []
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname

  let pp fmt = function
    | Intent -> F.fprintf fmt "Intent"
    | Logging -> F.fprintf fmt "Logging"
    | Other -> F.fprintf fmt "Other"
end

module JavaSink = Sink.Make(SinkKind)

include
  Trace.Make(struct
    module Source = JavaSource
    module Sink = JavaSink

    let should_report source sink =
      match Source.kind source, Sink.kind sink with
      | PrivateData, Logging
      | Intent, Intent
      | Other, _ | _, Other ->
          true
      | _ ->
          false
  end)
