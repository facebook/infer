(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module SourceKind = struct
  type t =
    | DrawableResource of Pvar.t  (** Drawable resource ID read from a global *)
    | Endpoint of (Mangled.t * Typ.desc)  (** source originating from formal of an endpoint *)
    | Intent  (** external Intent or a value read from one *)
    | IntentForInsecureIntentHandling of {exposed: bool}
    | IntentFromURI  (** Intent created from a URI *)
    | Other  (** for testing or uncategorized sources *)
    | PrivateData  (** private user or device-specific data *)
    | UserControlledString  (** data read from a text box or the clipboard service *)
    | UserControlledURI  (** resource locator originating from the browser bar *)
  [@@deriving compare]

  let is_exposed ~caller_pname =
    match caller_pname with
    | Typ.Procname.Java java_pname ->
        let class_name = Typ.Procname.Java.get_class_name java_pname in
        QuandaryConfig.is_endpoint class_name
    | _ ->
        false


  let intent_for_insecure_intent_handling ~caller_pname =
    let exposed = is_exposed ~caller_pname in
    IntentForInsecureIntentHandling {exposed}


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
      ~f:(fun {QuandaryConfig.Source.procedure; kinds} -> (Str.regexp procedure, kinds))
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


  let get ~caller_pname pname actuals tenv =
    let return = None in
    let get_external_source class_name method_name =
      (* check the list of externally specified sources *)
      let procedure = class_name ^ "." ^ method_name in
      let sources =
        List.concat_map external_sources ~f:(fun (procedure_regex, kinds) ->
            if Str.string_match procedure_regex procedure 0 then
              List.rev_map kinds ~f:(fun kind -> (of_string kind, return))
            else [] )
      in
      Option.some_if (not (List.is_empty sources)) sources
    in
    match pname with
    | Typ.Procname.Java pname ->
        let method_name = Typ.Procname.Java.get_method pname in
        let taint_matching_supertype typename =
          match (Typ.Name.name typename, method_name) with
          | "android.app.Activity", "getIntent" ->
              Some [(Intent, return); (intent_for_insecure_intent_handling ~caller_pname, return)]
          | "android.support.v4.app.FragmentActivity", "getIntent" ->
              Some [(intent_for_insecure_intent_handling ~caller_pname, return)]
          | "android.content.Intent", "<init>"
            when actual_has_type 2 "android.net.Uri" actuals tenv ->
              (* taint the [this] parameter passed to the constructor *)
              Some [(IntentFromURI, Some 0)]
          | ( "android.content.Intent"
            , ( "parseUri"
              | "setData"
              | "setDataAndNormalize"
              | "setDataAndType"
              | "setDataAndTypeAndNormalize" ) ) ->
              Some [(IntentFromURI, return)]
          | "android.content.Intent", "getData" ->
              Some [(intent_for_insecure_intent_handling ~caller_pname, return)]
          | "android.content.Intent", "getStringExtra" ->
              Some [(Intent, return)]
          | "android.content.SharedPreferences", "getString" ->
              Some [(PrivateData, return)]
          | ( ("android.content.ClipboardManager" | "android.text.ClipboardManager")
            , ("getPrimaryClip" | "getText") ) ->
              Some [(UserControlledString, return)]
          | ( "android.location.Location"
            , ("getAltitude" | "getBearing" | "getLatitude" | "getLongitude" | "getSpeed") ) ->
              Some [(PrivateData, return)]
          | ( "android.telephony.TelephonyManager"
            , ( "getDeviceId"
              | "getLine1Number"
              | "getSimSerialNumber"
              | "getSubscriberId"
              | "getVoiceMailNumber" ) ) ->
              Some [(PrivateData, return)]
          | "android.webkit.WebResourceRequest", "getUrl" ->
              Some [(UserControlledURI, return)]
          | "android.widget.EditText", "getText" ->
              Some [(UserControlledString, return)]
          | "com.facebook.infer.builtins.InferTaint", "inferSecretSource" ->
              Some [(Other, return)]
          | class_name, method_name ->
              get_external_source class_name method_name
        in
        PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
          (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name pname))
        |> Option.value ~default:[]
    | Typ.Procname.C _ when Typ.Procname.equal pname BuiltinDecl.__global_access -> (
      (* accessed global will be passed to us as the only parameter *)
      match List.map actuals ~f:HilExp.ignore_cast with
      | [HilExp.AccessExpression access_expr] -> (
        match HilExp.AccessExpression.to_access_path access_expr with
        | (Var.ProgramVar pvar, _), _ ->
            let pvar_string = Pvar.to_string pvar in
            (* checking substring instead of prefix because we expect field names like
               com.myapp.R$drawable.whatever *)
            if String.is_substring ~substring:AndroidFramework.drawable_prefix pvar_string then
              [(DrawableResource pvar, None)]
            else []
        | _ ->
            [] )
      | _ ->
          [] )
    | pname when BuiltinDecl.is_declared pname ->
        []
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
            if Mangled.is_this name then None else Some (make_source name typ.Typ.desc)
          in
          (name, typ, taint) )
        (Procdesc.get_formals pdesc)
    in
    let formals = Procdesc.get_formals pdesc in
    match Procdesc.get_proc_name pdesc with
    | Typ.Procname.Java java_pname as pname -> (
        let method_name = Typ.Procname.Java.get_method java_pname in
        let taint_matching_supertype typename =
          match (Typ.Name.name typename, method_name) with
          | ( ("android.app.Activity" | "android.app.Fragment" | "android.support.v4.app.Fragment")
            , ("onActivityResult" | "onNewIntent") ) ->
              Some (taint_formals_with_types ["android.content.Intent"] Intent formals)
          | ( "android.app.Service"
            , ("onBind" | "onRebind" | "onStart" | "onStartCommand" | "onTaskRemoved" | "onUnbind")
            ) ->
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
                (taint_formals_with_types
                   ["android.net.Uri"; "java.lang.String"]
                   UserControlledURI formals)
          | ( "android.webkit.WebChromeClient"
            , ("onJsAlert" | "onJsBeforeUnload" | "onJsConfirm" | "onJsPrompt") ) ->
              Some (taint_formals_with_types ["java.lang.String"] UserControlledURI formals)
          | ( "android.webkit.WebViewClient"
            , ("onLoadResource" | "shouldInterceptRequest" | "shouldOverrideUrlLoading") ) ->
              Some
                (taint_formals_with_types
                   ["android.webkit.WebResourceRequest"; "java.lang.String"]
                   UserControlledURI formals)
          | "codetoanalyze.java.quandary.TaintedFormals", "taintedContextBad" ->
              Some
                (taint_formals_with_types ["java.lang.Integer"; "java.lang.String"] Other formals)
          | _ -> (
            match Tenv.lookup tenv typename with
            | Some typ ->
                if
                  Annotations.struct_typ_has_annot typ Annotations.ia_is_thrift_service
                  && PatternMatch.override_exists ~check_current_type:false
                       (fun superclass_pname ->
                         String.equal (Typ.Procname.get_method superclass_pname) method_name )
                       tenv pname
                then
                  (* assume every non-this formal of a Thrift service is tainted *)
                  (* TODO: may not want to taint numbers or Enum's *)
                  Some (taint_all_but_this ~make_source:(fun name desc -> Endpoint (name, desc)))
                else None
            | _ ->
                None )
        in
        match
          PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
            (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_pname))
        with
        | Some tainted_formals ->
            tainted_formals
        | None ->
            Source.all_formals_untainted pdesc )
    | procname ->
        L.(die InternalError)
          "Non-Java procedure %a where only Java procedures are expected" Typ.Procname.pp procname


  let pp fmt kind =
    match kind with
    | DrawableResource pvar ->
        F.pp_print_string fmt (Pvar.to_string pvar)
    | Endpoint (formal_name, _) ->
        F.fprintf fmt "Endpoint(%s)" (Mangled.to_string formal_name)
    | Intent | IntentForInsecureIntentHandling _ ->
        F.pp_print_string fmt "Intent"
    | IntentFromURI ->
        F.pp_print_string fmt "IntentFromURI"
    | Other ->
        F.pp_print_string fmt "Other"
    | PrivateData ->
        F.pp_print_string fmt "PrivateData"
    | UserControlledString ->
        F.pp_print_string fmt "UserControlledString"
    | UserControlledURI ->
        F.pp_print_string fmt "UserControlledURI"
end

module JavaSource = Source.Make (SourceKind)

module SinkKind = struct
  type t =
    | ClassLoading
    | CreateFile  (** sink that creates a file *)
    | CreateIntent  (** sink that creates an Intent *)
    | OpenDrawableResource  (** sink that inflates a Drawable resource from an integer ID *)
    | Deserialization  (** sink that deserializes a Java object *)
    | HTML  (** sink that creates HTML *)
    | JavaScript  (** sink that passes its arguments to untrusted JS code *)
    | Logging  (** sink that logs one or more of its arguments *)
    | ShellExec  (** sink that runs a shell command *)
    | SQLInjection  (** unescaped query to a SQL database (could be a read or a write) *)
    | SQLRead  (** escaped read to a SQL database *)
    | SQLWrite  (** escaped write to a SQL database *)
    | StartComponent  (** sink that launches an Activity, Service, etc. *)
    | StartComponentForInsecureIntentHandling
    | Other  (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let matches ~caller ~callee = Int.equal 0 (compare caller callee)

  let of_string = function
    | "ClassLoading" ->
        ClassLoading
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
    | "SQLInjection" ->
        SQLInjection
    | "SQLRead" ->
        SQLRead
    | "SQLWrite" ->
        SQLWrite
    | "StartComponent" ->
        StartComponent
    | "StartComponentForInsecureIntentHandling" ->
        StartComponentForInsecureIntentHandling
    | _ ->
        Other


  let external_sinks =
    List.map
      ~f:(fun {QuandaryConfig.Sink.procedure; kinds; index} -> (Str.regexp procedure, kinds, index))
      (QuandaryConfig.Sink.of_json Config.quandary_sinks)


  let get pname actuals _ tenv =
    match pname with
    | Typ.Procname.Java java_pname ->
        (* taint all the inputs of [pname]. for non-static procedures, taints the "this" parameter
           only if [taint_this] is true. *)
        let taint_all ?(taint_this = false) kinds =
          let actuals_to_taint, offset =
            if Typ.Procname.Java.is_static java_pname || taint_this then (actuals, 0)
            else (List.tl_exn actuals, 1)
          in
          let indexes =
            IntSet.of_list (List.mapi ~f:(fun param_num _ -> param_num + offset) actuals_to_taint)
          in
          Some (List.rev_map kinds ~f:(fun kind -> (kind, indexes)))
        in
        (* taint the nth non-"this" parameter (0-indexed) *)
        let taint_nth n kinds =
          let first_index = if Typ.Procname.Java.is_static java_pname then n else n + 1 in
          if first_index < List.length actuals then
            let first_index = IntSet.singleton first_index in
            Some (List.rev_map kinds ~f:(fun kind -> (kind, first_index)))
          else None
        in
        let get_external_sink class_name method_name =
          (* check the list of externally specified sinks *)
          let procedure = class_name ^ "." ^ method_name in
          let sinks =
            List.concat_map external_sinks ~f:(fun (procedure_regex, kinds, index) ->
                if Str.string_match procedure_regex procedure 0 then
                  let kinds = List.rev_map ~f:of_string kinds in
                  let taints =
                    try
                      let n = int_of_string index in
                      taint_nth n kinds
                    with Failure _ ->
                      (* couldn't parse the index, just taint everything *)
                      taint_all kinds
                  in
                  Option.value taints ~default:[]
                else [] )
          in
          Option.some_if (not (List.is_empty sinks)) sinks
        in
        let method_name = Typ.Procname.Java.get_method java_pname in
        let taint_matching_supertype typename =
          match (Typ.Name.name typename, method_name) with
          | "android.app.Activity", ("startActivityFromChild" | "startActivityFromFragment") ->
              taint_nth 1 [StartComponent]
          | ( ( "android.app.Activity"
              | "android.content.Context"
              | "android.support.v4.app.Fragment" )
            , "startIntentSenderForResult" ) ->
              taint_nth 2 [StartComponent]
          | "android.app.Activity", "startIntentSenderFromChild" ->
              taint_nth 3 [StartComponent]
          | ( ( "android.app.Fragment"
              | "android.content.Context"
              | "android.support.v4.app.Fragment" )
            , "startActivity" ) ->
              taint_nth 0 [StartComponent; StartComponentForInsecureIntentHandling]
          | ( ( "android.app.Fragment"
              | "android.content.Context"
              | "android.support.v4.app.Fragment" )
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
              | "startActivityForResult"
              | "startActivityIfNeeded"
              | "startNextMatchingActivity"
              | "startService"
              | "stopService" ) ) ->
              taint_nth 0 [StartComponent]
          | "android.content.Context", "startIntentSender" ->
              taint_nth 1 [StartComponent]
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
              taint_nth 0 [CreateIntent]
          | "android.content.Intent", "setClassName" ->
              taint_all [CreateIntent]
          | "android.text.Html", "fromHtml" ->
              taint_nth 0 [HTML]
          | "android.util.Log", ("e" | "println" | "w" | "wtf") ->
              taint_all [Logging]
          | ( "android.webkit.WebView"
            , ( "evaluateJavascript"
              | "loadData"
              | "loadDataWithBaseURL"
              | "loadUrl"
              | "postUrl"
              | "postWebMessage" ) ) ->
              taint_all [JavaScript]
          | "com.facebook.infer.builtins.InferTaint", "inferSensitiveSink" ->
              taint_nth 0 [Other]
          | "java.io.File", "<init>"
          | "java.nio.file.FileSystem", "getPath"
          | "java.nio.file.Paths", "get" ->
              taint_all [CreateFile]
          | "java.io.ObjectInputStream", "<init>" ->
              taint_all [Deserialization]
          | "java.lang.Class", "forName" | "java.lang.ClassLoader", "loadClass" ->
              taint_nth 0 [ClassLoading]
          | "java.lang.ClassLoader", "defineClass" ->
              taint_nth 1 [ClassLoading]
          | "java.lang.ProcessBuilder", "<init>" ->
              taint_all [ShellExec]
          | "java.lang.ProcessBuilder", "command" ->
              taint_all [ShellExec]
          | "java.lang.Runtime", "exec" ->
              taint_nth 0 [ShellExec]
          (* TODO: separate non-injection sinks for PreparedStatement's *)
          | "java.sql.Statement", ("addBatch" | "execute") ->
              taint_nth 0 [SQLInjection]
          | "java.sql.Statement", "executeQuery" ->
              taint_nth 0 [SQLRead]
          | "java.sql.Statement", ("executeUpdate" | "executeLargeUpdate") ->
              taint_nth 0 [SQLWrite]
          | class_name, method_name ->
              get_external_sink class_name method_name
        in
        PatternMatch.supertype_find_map_opt tenv taint_matching_supertype
          (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_pname))
        |> Option.value ~default:[]
    | pname when BuiltinDecl.is_declared pname ->
        []
    | pname ->
        L.(die InternalError) "Non-Java procname %a in Java analysis" Typ.Procname.pp pname


  let pp fmt kind =
    F.fprintf fmt
      ( match kind with
      | ClassLoading ->
          "ClassLoading"
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
      | SQLInjection ->
          "SQLInjection"
      | SQLRead ->
          "SQLRead"
      | SQLWrite ->
          "SQLWrite"
      | StartComponent | StartComponentForInsecureIntentHandling ->
          "StartComponent"
      | Other ->
          "Other" )
end

module JavaSink = Sink.Make (SinkKind)

module JavaSanitizer = struct
  type t = All | StringConcatenation [@@deriving compare]

  let equal = [%compare.equal: t]

  let external_sanitizers =
    List.map
      ~f:(fun {QuandaryConfig.Sanitizer.procedure} -> Str.regexp procedure)
      (QuandaryConfig.Sanitizer.of_json Config.quandary_sanitizers)


  let get_external_sanitizer class_name method_name =
    let procedure_string = Printf.sprintf "%s.%s" class_name method_name in
    List.find_map
      ~f:(fun procedure_regex ->
        if Str.string_match procedure_regex procedure_string 0 then Some All else None )
      external_sanitizers


  let get pname tenv =
    match pname with
    | Typ.Procname.Java java_pname ->
        let method_name = Typ.Procname.Java.get_method java_pname in
        let sanitizer_matching_supertype typename =
          match (Typ.Name.name typename, method_name) with
          | "java.lang.StringBuilder", "append" ->
              Some StringConcatenation
          | class_name, method_name ->
              get_external_sanitizer class_name method_name
        in
        PatternMatch.supertype_find_map_opt tenv sanitizer_matching_supertype
          (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_pname))
    | _ ->
        None


  let pp fmt kind =
    F.pp_print_string fmt
      (match kind with All -> "All" | StringConcatenation -> "StringConcatenation")
end

include Trace.Make (struct
  module Source = JavaSource
  module Sink = JavaSink
  module Sanitizer = JavaSanitizer

  let get_report source sink sanitizers =
    match (Source.kind source, Sink.kind sink) with
    | _ when List.mem sanitizers Sanitizer.All ~equal:Sanitizer.equal ->
        (* the All sanitizer clears any form of taint; don't report *)
        None
    | _, ClassLoading when List.mem sanitizers Sanitizer.StringConcatenation ~equal:Sanitizer.equal
      ->
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
    | (Intent | IntentFromURI | UserControlledString | UserControlledURI), Deserialization ->
        (* shouldn't let anyone external control what we deserialize *)
        Some IssueType.untrusted_deserialization
    | Endpoint _, Deserialization ->
        (* shouldn't let anyone external control what we deserialize *)
        Some IssueType.untrusted_deserialization_risk
    | (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI), HTML ->
        (* untrusted data flows into HTML; XSS risk *)
        Some IssueType.cross_site_scripting
    | (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI), JavaScript
      ->
        (* untrusted data flows into JS *)
        Some IssueType.javascript_injection
    | ( (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI)
      , SQLInjection ) ->
        (* untrusted and unescaped data flows to SQL *)
        Some IssueType.sql_injection_risk
    | ( (Endpoint _ | Intent | IntentFromURI | UserControlledString | UserControlledURI)
      , (SQLRead | SQLWrite) ) ->
        (* untrusted data flows to SQL *)
        Some IssueType.user_controlled_sql_risk
    | DrawableResource _, OpenDrawableResource ->
        (* not a security issue, but useful for debugging flows from resource IDs to inflation *)
        Some IssueType.quandary_taint_error
    | IntentForInsecureIntentHandling {exposed= true}, StartComponentForInsecureIntentHandling ->
        Some IssueType.exposed_insecure_intent_handling
    | IntentForInsecureIntentHandling {exposed= false}, StartComponentForInsecureIntentHandling ->
        Some IssueType.insecure_intent_handling
    | IntentFromURI, StartComponent ->
        (* create an intent/start a component using a (possibly user-controlled) URI. may or may not
           be an issue; depends on where the URI comes from *)
        Some IssueType.create_intent_from_uri
    | PrivateData, Logging ->
        Some IssueType.logging_private_data
    | (Intent | UserControlledString | UserControlledURI), (ClassLoading | ShellExec) ->
        Some IssueType.shell_injection
    | Endpoint _, (ClassLoading | ShellExec) ->
        Some IssueType.shell_injection_risk
    | Other, _ | _, Other ->
        (* for testing purposes, Other matches everything *)
        Some IssueType.quandary_taint_error
    | (DrawableResource _ | IntentForInsecureIntentHandling _ | IntentFromURI | PrivateData), _
    | _, (Logging | OpenDrawableResource | StartComponent | StartComponentForInsecureIntentHandling)
      ->
        None
end)
