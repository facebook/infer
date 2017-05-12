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
    | Clipboard (** data read from the clipboard service *)
    | Intent (** external Intent or a value read from one *)
    | Other (** for testing or uncategorized sources *)
    | PrivateData (** private user or device-specific data *)
    | UserControlledURI (** resource locator controller by user *)
    | Unknown
  [@@deriving compare]

  let unknown = Unknown

  let of_string = function
    | "Clipboard" -> Clipboard
    | "Intent" -> Intent
    | "PrivateData" -> PrivateData
    | "UserControlledURI" -> UserControlledURI
    | _ -> Other

  let external_sources =
    List.map
      ~f:(fun { QuandaryConfig.Source.procedure; kind; } -> Str.regexp procedure, kind)
      (QuandaryConfig.Source.of_json Config.quandary_sources)

  let get pname tenv =
    let return = None in
    match pname with
    | Typ.Procname.Java pname ->
        begin
          match Typ.Procname.java_get_class_name pname, Typ.Procname.java_get_method pname with
          | "android.location.Location",
            ("getAltitude" | "getBearing" | "getLatitude" | "getLongitude" | "getSpeed") ->
              Some (PrivateData, return)
          | "android.telephony.TelephonyManager",
            ("getDeviceId" |
             "getLine1Number" |
             "getSimSerialNumber" |
             "getSubscriberId" |
             "getVoiceMailNumber") ->
              Some (PrivateData, return)
          | "com.facebook.infer.builtins.InferTaint", "inferSecretSource" ->
              Some (Other, return)
          | class_name, method_name ->
              let taint_matching_supertype typename _ =
                match Typ.Name.name typename, method_name with
                | "android.app.Activity", "getIntent" ->
                    Some (Intent, return)
                | "android.content.Intent", "getStringExtra" ->
                    Some (Intent, return)
                | "android.content.SharedPreferences", "getString" ->
                    Some (PrivateData, return)
                | ("android.content.ClipboardManager" | "android.text.ClipboardManager"),
                  ("getPrimaryClip" | "getText") ->
                    Some (Clipboard, return)
                | _ ->
                    None in
              let kind_opt =
                PatternMatch.supertype_find_map_opt
                  tenv
                  taint_matching_supertype
                  (Typ.Name.Java.from_string class_name) in
              begin
                match kind_opt with
                | Some _ -> kind_opt
                | None ->
                    (* check the list of externally specified sources *)
                    let procedure = class_name ^ "." ^ method_name in
                    List.find_map
                      ~f:(fun (procedure_regex, kind) ->
                          if Str.string_match procedure_regex procedure 0
                          then Some (of_string kind, return)
                          else None)
                      external_sources
              end
        end
    | pname when BuiltinDecl.is_declared pname -> None
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Typ.Procname.pp pname

  let get_tainted_formals pdesc tenv =
    let make_untainted (name, typ) =
      name, typ, None in
    let taint_formals_with_types type_strs kind formals =
      let taint_formal_with_types ((formal_name, formal_typ) as formal) =
        let matches_classname = match formal_typ.Typ.desc with
          | Tptr ({desc=Tstruct typename}, _) ->
              List.mem ~equal:String.equal type_strs (Typ.Name.name typename)
          | _ ->
              false in
        if matches_classname
        then
          formal_name, formal_typ, Some kind
        else
          make_untainted formal in
      List.map ~f:taint_formal_with_types formals in

    let formals = Procdesc.get_formals pdesc in
    match Procdesc.get_proc_name pdesc with
    | Typ.Procname.Java java_pname ->
        begin
          match Typ.Procname.java_get_class_name java_pname,
                Typ.Procname.java_get_method java_pname with
          | "codetoanalyze.java.quandary.TaintedFormals", "taintedContextBad" ->
              taint_formals_with_types ["java.lang.Integer"; "java.lang.String"] Other formals
          | class_name, method_name ->
              let taint_matching_supertype typename _ =
                match Typ.Name.name typename, method_name with
                | "android.app.Activity", ("onActivityResult" | "onNewIntent") ->
                    Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
                | "android.app.Service",
                  ("onBind" |
                   "onRebind" |
                   "onStart" |
                   "onStartCommand" |
                   "onTaskRemoved" |
                   "onUnbind") ->
                    Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
                | "android.content.BroadcastReceiver", "onReceive" ->
                    Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
                | "android.content.ContentProvider",
                  ("bulkInsert" |
                   "call" |
                   "delete" |
                   "insert" |
                   "getType" |
                   "openAssetFile" |
                   "openFile" |
                   "openPipeHelper" |
                   "openTypedAssetFile" |
                   "query" |
                   "refresh" |
                   "update") ->
                    Some
                      (taint_formals_with_types
                         ["android.net.Uri"; "java.lang.String"] UserControlledURI formals)
                | "android.webkit.WebViewClient",
                  ("onLoadResource" | "shouldInterceptRequest" | "shouldOverrideUrlLoading") ->
                    Some
                      (taint_formals_with_types
                         ["android.webkit.WebResourceRequest"; "java.lang.String"]
                         UserControlledURI
                         formals)
                | "android.webkit.WebChromeClient",
                  ("onJsAlert" | "onJsBeforeUnload" | "onJsConfirm" | "onJsPrompt") ->
                    Some (taint_formals_with_types ["java.lang.String"] UserControlledURI formals)
                | _ ->
                    None in
              begin
                match
                  PatternMatch.supertype_find_map_opt
                    tenv
                    taint_matching_supertype
                    (Typ.Name.Java.from_string class_name) with
                | Some tainted_formals -> tainted_formals
                | None -> Source.all_formals_untainted pdesc
              end
        end
    | procname ->
        failwithf
          "Non-Java procedure %a where only Java procedures are expected"
          Typ.Procname.pp procname

  let pp fmt kind =
    F.fprintf fmt
      (match kind with
       | Clipboard -> "Clipboard"
       | Intent -> "Intent"
       | UserControlledURI -> "UserControlledURI"
       | PrivateData -> "PrivateData"
       | Other -> "Other"
       | Unknown -> "Unknown")
end

module JavaSource = Source.Make(SourceKind)

module SinkKind = struct
  type t =
    | CreateFile (** sink that creates a file *)
    | CreateIntent (** sink that creates an Intent *)
    | JavaScript (** sink that passes its arguments to untrusted JS code *)
    | Logging (** sink that logs one or more of its arguments *)
    | StartComponent (** sink that launches an Activity, Service, etc. *)
    | Other (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let of_string = function
    | "CreateFile" -> CreateFile
    | "CreateIntent" -> CreateIntent
    | "JavaScript" -> JavaScript
    | "Logging" -> Logging
    | "StartComponent" -> StartComponent
    | _ -> Other

  let external_sinks =
    List.map
      ~f:(fun { QuandaryConfig.Sink.procedure; kind; index; } ->
          Str.regexp procedure, kind, index)
      (QuandaryConfig.Sink.of_json Config.quandary_sinks)

  let get pname actuals tenv =
    (* taint all the inputs of [pname]. for non-static procedures, taints the "this" parameter only
       if [taint_this] is true. *)
    let taint_all ?(taint_this=false) kind ~report_reachable =
      let actuals_to_taint, offset =
        if Typ.Procname.java_is_static pname || taint_this
        then actuals, 0
        else List.tl_exn actuals, 1 in
      List.mapi
        ~f:(fun param_num _ -> kind, param_num + offset, report_reachable)
        actuals_to_taint in
    (* taint the nth non-"this" parameter (0-indexed) *)
    let taint_nth n kind ~report_reachable =
      let first_index = if Typ.Procname.java_is_static pname then n else n + 1 in
      [kind, first_index, report_reachable] in
    match pname with
    | Typ.Procname.Java java_pname ->
        begin
          match Typ.Procname.java_get_class_name java_pname,
                Typ.Procname.java_get_method java_pname with
          | "android.util.Log", ("e" | "println" | "w" | "wtf") ->
              taint_all Logging ~report_reachable:true
          | "java.io.File", "<init>"
          | "java.nio.file.FileSystem", "getPath"
          | "java.nio.file.Paths", "get" ->
              taint_all CreateFile ~report_reachable:true
          | "com.facebook.infer.builtins.InferTaint", "inferSensitiveSink" ->
              [Other, 0, false]
          | class_name, method_name ->
              let taint_matching_supertype typename _ =
                match Typ.Name.name typename, method_name with
                | "android.app.Activity",
                  ("startActivityFromChild" | "startActivityFromFragment") ->
                    Some (taint_nth 1 StartComponent ~report_reachable:true)
                | "android.app.Activity", "startIntentSenderForResult"  ->
                    Some (taint_nth 2 StartComponent ~report_reachable:true)
                | "android.app.Activity", "startIntentSenderFromChild"  ->
                    Some (taint_nth 3 StartComponent ~report_reachable:true)
                | "android.content.Context",
                  ("bindService" |
                   "sendBroadcast" |
                   "sendBroadcastAsUser" |
                   "sendOrderedBroadcast" |
                   "sendOrderedBroadcastAsUser" |
                   "sendStickyBroadcast" |
                   "sendStickyBroadcastAsUser" |
                   "sendStickyOrderedBroadcast" |
                   "sendStickyOrderedBroadcastAsUser" |
                   "startActivities" |
                   "startActivity" |
                   "startActivityForResult" |
                   "startActivityIfNeeded" |
                   "startNextMatchingActivity" |
                   "startService" |
                   "stopService") ->
                    Some (taint_nth 0 StartComponent ~report_reachable:true)
                | "android.content.Context", "startIntentSender" ->
                    Some (taint_nth 1 StartComponent ~report_reachable:true)
                | "android.content.Intent",
                  ("parseUri" |
                   "getIntent" |
                   "getIntentOld" |
                   "setComponent" |
                   "setData" |
                   "setDataAndNormalize" |
                   "setDataAndType" |
                   "setDataAndTypeAndNormalize" |
                   "setPackage") ->
                    Some (taint_nth 0 CreateIntent ~report_reachable:true)
                | "android.content.Intent", "setClassName" ->
                    Some (taint_all CreateIntent ~report_reachable:true)
                | "android.webkit.WebView",
                  ("evaluateJavascript" |
                   "loadData" |
                   "loadDataWithBaseURL" |
                   "loadUrl" |
                   "postUrl" |
                   "postWebMessage") ->
                    Some (taint_all JavaScript ~report_reachable:true)
                | class_name, method_name ->
                    (* check the list of externally specified sinks *)
                    let procedure = class_name ^ "." ^ method_name in
                    List.find_map
                      ~f:(fun (procedure_regex, kind, index) ->
                          if Str.string_match procedure_regex procedure 0
                          then
                            let kind = of_string kind in
                            try
                              let n = int_of_string index in
                              Some (taint_nth n kind ~report_reachable:true)
                            with Failure _ ->
                              (* couldn't parse the index, just taint everything *)
                              Some (taint_all kind ~report_reachable:true)
                          else
                            None)
                      external_sinks in
              begin
                match
                  PatternMatch.supertype_find_map_opt
                    tenv
                    taint_matching_supertype
                    (Typ.Name.Java.from_string class_name) with
                | Some sinks -> sinks
                | None -> []
              end

        end
    | pname when BuiltinDecl.is_declared pname -> []
    | pname -> failwithf "Non-Java procname %a in Java analysis@." Typ.Procname.pp pname

  let pp fmt kind =
    F.fprintf fmt
      (match kind with
       | CreateFile -> "CreateFile"
       | CreateIntent -> "CreateIntent"
       | JavaScript -> "JavaScript"
       | Logging -> "Logging"
       | StartComponent -> "StartComponent"
       | Other -> "Other")
end

module JavaSink = Sink.Make(SinkKind)

include
  Trace.Make(struct
    module Source = JavaSource
    module Sink = JavaSink

    let should_report source sink =
      match Source.kind source, Sink.kind sink with
      | PrivateData, Logging (* logging private data issue *)
      | Intent, StartComponent (* intent reuse issue *)
      | Intent, CreateIntent (* intent configured with external values issue *)
      | Intent, JavaScript (* external data flows into JS: remote code execution risk *)
      | PrivateData, JavaScript (* leaking private data into JS *)
      | UserControlledURI, (CreateIntent | StartComponent)
      (* create intent/launch component from user-controlled URI *)
      | UserControlledURI, CreateFile
      (* create file from user-controller URI; potential path-traversal vulnerability *)
      | Clipboard, (StartComponent | CreateIntent | JavaScript | CreateFile) ->
          (* do something sensitive with user-controlled data from the clipboard *)
          true
      | Other, _ | _, Other -> (* for testing purposes, Other matches everything *)
          true
      | _ ->
          false
  end)
