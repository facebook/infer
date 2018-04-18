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
    | DrawableResource of Pvar.t  (** Drawable resource ID read from a global *)
    | Endpoint of (Mangled.t * Typ.desc)  (** source originating from formal of an endpoint *)
    | Intent  (** external Intent or a value read from one *)
    | IntentFromURI  (** Intent created from a URI *)
    | Other  (** for testing or uncategorized sources *)
    | PrivateData  (** private user or device-specific data *)
    | UserControlledString  (** data read from a text box or the clipboard service *)
    | UserControlledURI  (** resource locator originating from the browser bar *)
  [@@deriving compare]

  let matches ~caller ~callee = Int.equal 0 (compare caller callee)

  let of_string = function
    | "Endpoint" ->
        Endpoint (Mangled.from_string "NONE", Typ.Tvoid)
    | "Intent" ->
        Intent
    | "IntentFromURI" ->
        IntentFromURI
    | "PrivateData" ->
        PrivateData
    | "UserControlledURI" ->
        UserControlledURI
    | "UserControlledString" ->
        UserControlledString
    | _ ->
        Other


  let external_sources =
    List.map
      ~f:(fun {QuandaryConfig.Source.procedure; kind} -> (Str.regexp procedure, kind))
      (QuandaryConfig.Source.of_json Config.quandary_sources)


  let actual_has_type n type_string actuals tenv =
    let is_typ typename _ = String.equal (Typ.Name.name typename) type_string in
    match List.nth actuals n with
    | Some actual -> (
      match HilExp.get_typ tenv actual with
      | Some {desc= Tptr ({desc= Tstruct typename}, _)} ->
          PatternMatch.supertype_exists tenv is_typ typename
      | _ ->
          false )
    | None ->
        false


  let get pname actuals tenv =
    let return = None in
    let get_external_source class_name method_name =
      (* check the list of externally specified sources *)
      let procedure = class_name ^ "." ^ method_name in
      List.find_map
        ~f:(fun (procedure_regex, kind) ->
          if Str.string_match procedure_regex procedure 0 then Some (of_string kind, return)
          else None )
        external_sources
    in
    match pname with
    | Typ.Procname.Java pname -> (
      match (Typ.Procname.Java.get_class_name pname, Typ.Procname.Java.get_method pname) with
      | "android.content.Intent", "<init>" when actual_has_type 2 "android.net.Uri" actuals tenv ->
          (* taint the [this] parameter passed to the constructor *)
          Some (IntentFromURI, Some 0)
      | ( "android.content.Intent"
        , ( "parseUri"
          | "setData"
          | "setDataAndNormalize"
          | "setDataAndType"
          | "setDataAndTypeAndNormalize" ) ) ->
          Some (IntentFromURI, return)
      | ( "android.location.Location"
        , ("getAltitude" | "getBearing" | "getLatitude" | "getLongitude" | "getSpeed") ) ->
          Some (PrivateData, return)
      | ( "android.telephony.TelephonyManager"
        , ( "getDeviceId"
          | "getLine1Number"
          | "getSimSerialNumber"
          | "getSubscriberId"
          | "getVoiceMailNumber" ) ) ->
          Some (PrivateData, return)
      | "com.facebook.infer.builtins.InferTaint", "inferSecretSource" ->
          Some (Other, return)
      | class_name, method_name ->
          let taint_matching_supertype typename =
            match (Typ.Name.name typename, method_name) with
            | "android.app.Activity", "getIntent" ->
                Some (Intent, return)
            | "android.content.Intent", "getStringExtra" ->
                Some (Intent, return)
            | "android.content.SharedPreferences", "getString" ->
                Some (PrivateData, return)
            | ( ("android.content.ClipboardManager" | "android.text.ClipboardManager")
              , ("getPrimaryClip" | "getText") ) ->
                Some (UserControlledString, return)
            | "android.webkit.WebResourceRequest", "getUrl" ->
                Some (UserControlledURI, return)
            | "android.widget.EditText", "getText" ->
                Some (UserControlledString, return)
            | class_name, method_name ->
                get_external_source class_name method_name
          in
          PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
            (Typ.Name.Java.from_string class_name) )
    | Typ.Procname.C _ when Typ.Procname.equal pname BuiltinDecl.__global_access -> (
      match (* accessed global will be passed to us as the only parameter *)
            actuals with
      | [HilExp.AccessExpression access_expr] -> (
        match AccessExpression.to_access_path access_expr with
        | (Var.ProgramVar pvar, _), _ ->
            let pvar_string = Pvar.to_string pvar in
            (* checking substring instead of prefix because we expect field names like
               com.myapp.R$drawable.whatever *)
            if String.is_substring ~substring:AndroidFramework.drawable_prefix pvar_string then
              Some (DrawableResource pvar, None)
            else None
        | _ ->
            None )
      | _ ->
          None )
    | pname when BuiltinDecl.is_declared pname ->
        None
    | pname ->
        L.(die InternalError) "Non-Java procname %a in Java analysis" Typ.Procname.pp pname


  let get_tainted_formals pdesc tenv =
    let make_untainted (name, typ) = (name, typ, None) in
    let taint_formals_with_types type_strs kind formals =
      let taint_formal_with_types ((formal_name, formal_typ) as formal) =
        let matches_classname =
          match formal_typ.Typ.desc with
          | Tptr ({desc= Tstruct typename}, _) ->
              List.mem ~equal:String.equal type_strs (Typ.Name.name typename)
          | _ ->
              false
        in
        if matches_classname then (formal_name, formal_typ, Some kind) else make_untainted formal
      in
      List.map ~f:taint_formal_with_types formals
    in
    (* taint all formals except for [this] *)
    let taint_all_but_this ~make_source =
      List.map
        ~f:(fun (name, typ) ->
          let taint =
            match Mangled.to_string name with
            | "this" ->
                None
            | _ ->
                Some (make_source name typ.Typ.desc)
          in
          (name, typ, taint) )
        (Procdesc.get_formals pdesc)
    in
    let formals = Procdesc.get_formals pdesc in
    match Procdesc.get_proc_name pdesc with
    | Typ.Procname.Java java_pname -> (
      match
        (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
      with
      | "codetoanalyze.java.quandary.TaintedFormals", "taintedContextBad" ->
          taint_formals_with_types ["java.lang.Integer"; "java.lang.String"] Other formals
      | class_name, method_name ->
          let taint_matching_supertype typename =
            match (Typ.Name.name typename, method_name) with
            | "android.app.Activity", ("onActivityResult" | "onNewIntent") ->
                Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
            | ( "android.app.Service"
              , ( "onBind"
                | "onRebind"
                | "onStart"
                | "onStartCommand"
                | "onTaskRemoved"
                | "onUnbind" ) ) ->
                Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
            | "android.content.BroadcastReceiver", "onReceive" ->
                Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
            | ( "android.content.ContentProvider"
              , ( "bulkInsert"
                | "call"
                | "delete"
                | "insert"
                | "getType"
                | "openAssetFile"
                | "openFile"
                | "openPipeHelper"
                | "openTypedAssetFile"
                | "query"
                | "refresh"
                | "update" ) ) ->
                Some
                  (taint_formals_with_types ["android.net.Uri"; "java.lang.String"]
                     UserControlledURI formals)
            | ( "android.webkit.WebViewClient"
              , ("onLoadResource" | "shouldInterceptRequest" | "shouldOverrideUrlLoading") ) ->
                Some
                  (taint_formals_with_types
                     ["android.webkit.WebResourceRequest"; "java.lang.String"] UserControlledURI
                     formals)
            | ( "android.webkit.WebChromeClient"
              , ("onJsAlert" | "onJsBeforeUnload" | "onJsConfirm" | "onJsPrompt") ) ->
                Some (taint_formals_with_types ["java.lang.String"] UserControlledURI formals)
            | _ ->
              match Tenv.lookup tenv typename with
              | Some typ ->
                  if
                    Annotations.struct_typ_has_annot typ Annotations.ia_is_thrift_service
                    && PredSymb.equal_access (Procdesc.get_access pdesc) PredSymb.Public
                  then
                    (* assume every non-this formal of a Thrift service is tainted *)
                    (* TODO: may not want to taint numbers or Enum's *)
                    Some (taint_all_but_this ~make_source:(fun name desc -> Endpoint (name, desc)))
                  else None
              | _ ->
                  None
          in
          match
            PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
              (Typ.Name.Java.from_string class_name)
          with
          | Some tainted_formals ->
              tainted_formals
          | None ->
              Source.all_formals_untainted pdesc )
    | procname ->
        L.(die InternalError)
          "Non-Java procedure %a where only Java procedures are expected" Typ.Procname.pp procname


  let pp fmt kind =
    F.fprintf fmt "%s"
      ( match kind with
      | DrawableResource pvar ->
          Pvar.to_string pvar
      | Endpoint (formal_name, _) ->
          F.asprintf "Endpoint\\[%s\\]" (Mangled.to_string formal_name)
      | Intent ->
          "Intent"
      | IntentFromURI ->
          "IntentFromURI"
      | Other ->
          "Other"
      | PrivateData ->
          "PrivateData"
      | UserControlledString ->
          "UserControlledString"
      | UserControlledURI ->
          "UserControlledURI" )
end

module JavaSource = Source.Make (SourceKind)

module SinkKind = struct
  type t =
    | CreateFile  (** sink that creates a file *)
    | CreateIntent  (** sink that creates an Intent *)
    | OpenDrawableResource  (** sink that inflates a Drawable resource from an integer ID *)
    | Deserialization  (** sink that deserializes a Java object *)
    | HTML  (** sink that creates HTML *)
    | JavaScript  (** sink that passes its arguments to untrusted JS code *)
    | Logging  (** sink that logs one or more of its arguments *)
    | ShellExec  (** sink that runs a shell command *)
    | StartComponent  (** sink that launches an Activity, Service, etc. *)
    | Other  (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let matches ~caller ~callee = Int.equal 0 (compare caller callee)

  let of_string = function
    | "CreateFile" ->
        CreateFile
    | "CreateIntent" ->
        CreateIntent
    | "Deserialization" ->
        Deserialization
    | "HTML" ->
        HTML
    | "JavaScript" ->
        JavaScript
    | "Logging" ->
        Logging
    | "OpenDrawableResource" ->
        OpenDrawableResource
    | "ShellExec" ->
        ShellExec
    | "StartComponent" ->
        StartComponent
    | _ ->
        Other


  let external_sinks =
    List.map
      ~f:(fun {QuandaryConfig.Sink.procedure; kind; index} -> (Str.regexp procedure, kind, index))
      (QuandaryConfig.Sink.of_json Config.quandary_sinks)


  let get pname actuals _ tenv =
    match pname with
    | Typ.Procname.Java java_pname
      -> (
        (* taint all the inputs of [pname]. for non-static procedures, taints the "this" parameter
           only if [taint_this] is true. *)
        let taint_all ?(taint_this= false) kind =
          let actuals_to_taint, offset =
            if Typ.Procname.Java.is_static java_pname || taint_this then (actuals, 0)
            else (List.tl_exn actuals, 1)
          in
          let indexes =
            IntSet.of_list (List.mapi ~f:(fun param_num _ -> param_num + offset) actuals_to_taint)
          in
          Some (kind, indexes)
        in
        (* taint the nth non-"this" parameter (0-indexed) *)
        let taint_nth n kind =
          let first_index = if Typ.Procname.Java.is_static java_pname then n else n + 1 in
          if first_index < List.length actuals then Some (kind, IntSet.singleton first_index)
          else None
        in
        let get_external_sink class_name method_name =
          (* check the list of externally specified sinks *)
          let procedure = class_name ^ "." ^ method_name in
          List.find_map
            ~f:(fun (procedure_regex, kind, index) ->
              if Str.string_match procedure_regex procedure 0 then
                let kind = of_string kind in
                try
                  let n = int_of_string index in
                  taint_nth n kind
                with Failure _ ->
                  (* couldn't parse the index, just taint everything *)
                  taint_all kind
              else None )
            external_sinks
        in
        match
          (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
        with
        | "android.text.Html", "fromHtml" ->
            taint_nth 0 HTML
        | "android.util.Log", ("e" | "println" | "w" | "wtf") ->
            taint_all Logging
        | "java.io.File", "<init>"
        | "java.nio.file.FileSystem", "getPath"
        | "java.nio.file.Paths", "get" ->
            taint_all CreateFile
        | "java.io.ObjectInputStream", "<init>" ->
            taint_all Deserialization
        | "com.facebook.infer.builtins.InferTaint", "inferSensitiveSink" ->
            taint_nth 0 Other
        | "java.lang.ProcessBuilder", "<init>" ->
            taint_all ShellExec
        | "java.lang.ProcessBuilder", "command" ->
            taint_all ShellExec
        | class_name, method_name ->
            let taint_matching_supertype typename =
              match (Typ.Name.name typename, method_name) with
              | "android.app.Activity", ("startActivityFromChild" | "startActivityFromFragment") ->
                  taint_nth 1 StartComponent
              | "android.app.Activity", "startIntentSenderForResult" ->
                  taint_nth 2 StartComponent
              | "android.app.Activity", "startIntentSenderFromChild" ->
                  taint_nth 3 StartComponent
              | ( "android.content.Context"
                , ( "bindService"
                  | "sendBroadcast"
                  | "sendBroadcastAsUser"
                  | "sendOrderedBroadcast"
                  | "sendOrderedBroadcastAsUser"
                  | "sendStickyBroadcast"
                  | "sendStickyBroadcastAsUser"
                  | "sendStickyOrderedBroadcast"
                  | "sendStickyOrderedBroadcastAsUser"
                  | "startActivities"
                  | "startActivity"
                  | "startActivityForResult"
                  | "startActivityIfNeeded"
                  | "startNextMatchingActivity"
                  | "startService"
                  | "stopService" ) ) ->
                  taint_nth 0 StartComponent
              | "android.content.Context", "startIntentSender" ->
                  taint_nth 1 StartComponent
              | ( "android.content.Intent"
                , ( "parseUri"
                  | "getIntent"
                  | "getIntentOld"
                  | "setComponent"
                  | "setData"
                  | "setDataAndNormalize"
                  | "setDataAndType"
                  | "setDataAndTypeAndNormalize"
                  | "setPackage" ) ) ->
                  taint_nth 0 CreateIntent
              | "android.content.Intent", "setClassName" ->
                  taint_all CreateIntent
              | ( "android.webkit.WebView"
                , ( "evaluateJavascript"
                  | "loadData"
                  | "loadDataWithBaseURL"
                  | "loadUrl"
                  | "postUrl"
                  | "postWebMessage" ) ) ->
                  taint_all JavaScript
              | "java.lang.Runtime", "exec" ->
                  taint_nth 0 ShellExec
              | class_name, method_name ->
                  get_external_sink class_name method_name
            in
            PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
              (Typ.Name.Java.from_string class_name) )
    | pname when BuiltinDecl.is_declared pname ->
        None
    | pname ->
        L.(die InternalError) "Non-Java procname %a in Java analysis" Typ.Procname.pp pname


  let pp fmt kind =
    F.fprintf fmt
      ( match kind with
      | CreateFile ->
          "CreateFile"
      | CreateIntent ->
          "CreateIntent"
      | Deserialization ->
          "Deserialization"
      | HTML ->
          "HTML"
      | JavaScript ->
          "JavaScript"
      | Logging ->
          "Logging"
      | OpenDrawableResource ->
          "OpenDrawableResource"
      | ShellExec ->
          "ShellExec"
      | StartComponent ->
          "StartComponent"
      | Other ->
          "Other" )
end

module JavaSink = Sink.Make (SinkKind)

module JavaSanitizer = struct
  type t = All [@@deriving compare]

  let external_sanitizers =
    List.map
      ~f:(fun {QuandaryConfig.Sanitizer.procedure} -> Str.regexp procedure)
      (QuandaryConfig.Sanitizer.of_json Config.quandary_sanitizers)


  let get = function
    | Typ.Procname.Java java_pname ->
        let procedure_string =
          Printf.sprintf "%s.%s"
            (Typ.Procname.Java.get_class_name java_pname)
            (Typ.Procname.Java.get_method java_pname)
        in
        List.find_map
          ~f:(fun procedure_regex ->
            if Str.string_match procedure_regex procedure_string 0 then Some All else None )
          external_sanitizers
    | _ ->
        None


  let pp fmt = function All -> F.fprintf fmt "All"
end

include Trace.Make (struct
  module Source = JavaSource
  module Sink = JavaSink
  module Sanitizer = JavaSanitizer

  let get_report source sink sanitizers =
    match (Source.kind source, Sink.kind sink) with
    | _ when not (List.is_empty sanitizers) ->
        (* assume any sanitizer clears all forms of taint *)
        None
    | (Endpoint _ | Intent | UserControlledString | UserControlledURI), CreateIntent ->
        (* creating Intent from user-congrolled data *)
        Some IssueType.untrusted_intent_creation
    | (Intent | IntentFromURI | UserControlledString | UserControlledURI), CreateFile ->
        (* user-controlled file creation; may be vulnerable to path traversal + more *)
        Some IssueType.untrusted_file
    | Endpoint _, CreateFile ->
        (* user-controlled file creation; may be vulnerable to path traversal + more *)
        Some IssueType.untrusted_file_risk
    | ( (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI)
      , Deserialization ) ->
        (* shouldn't let anyone external control what we deserialize *)
        Some IssueType.untrusted_deserialization
    | (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI), HTML ->
        (* untrusted data flows into HTML; XSS risk *)
        Some IssueType.cross_site_scripting
    | (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI), JavaScript ->
        (* untrusted data flows into JS *)
        Some IssueType.javascript_injection
    | DrawableResource _, OpenDrawableResource ->
        (* not a security issue, but useful for debugging flows from resource IDs to inflation *)
        Some IssueType.quandary_taint_error
    | IntentFromURI, StartComponent ->
        (* create an intent/start a component using a (possibly user-controlled) URI. may or may not
           be an issue; depends on where the URI comes from *)
        Some IssueType.create_intent_from_uri
    | PrivateData, Logging ->
        Some IssueType.logging_private_data
    | (Intent | UserControlledString | UserControlledURI), ShellExec ->
        Some IssueType.shell_injection
    | Endpoint _, ShellExec ->
        Some IssueType.shell_injection_risk
    | Other, _ | _, Other ->
        (* for testing purposes, Other matches everything *)
        Some IssueType.quandary_taint_error
    | DrawableResource _, _
    | IntentFromURI, _
    | PrivateData, _
    | _, Logging
    | _, OpenDrawableResource
    | _, StartComponent ->
        None
end)
