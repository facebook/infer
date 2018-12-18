(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let parse_clang_procedure procedure kinds index =
  try Some (QualifiedCppName.Match.of_fuzzy_qual_names [procedure], kinds, index)
  with QualifiedCppName.ParseError _ ->
    (* Java and Clang sources/sinks live in the same inferconfig entry. If we try to parse a Java
         procedure that happens to be an invalid Clang qualified name (e.g., MyClass.<init>),
         parsing will crash. In the future, we can avoid this by requiring JSON source/sink
         specifications to indicate the language *)
    None


module SourceKind = struct
  type t =
    | CommandLineFlag of (Var.t * Typ.desc)  (** source that was read from a command line flag *)
    | Endpoint of (Mangled.t * Typ.desc)  (** source originating from formal of an endpoint *)
    | EnvironmentVariable  (** source that was read from an environment variable *)
    | ReadFile  (** source that was read from a file *)
    | Other  (** for testing or uncategorized sources *)
    | UserControlledEndpoint of (Mangled.t * Typ.desc)
        (** source originating from formal of an endpoint that is known to hold user-controlled data *)
  [@@deriving compare]

  let matches ~caller ~callee = Int.equal 0 (compare caller callee)

  let of_string = function
    | "CommandLineFlag" ->
        L.die UserError "User-specified CommandLineFlag sources are not supported"
    | "Endpoint" ->
        Endpoint (Mangled.from_string "NONE", Typ.Tvoid)
    | "EnvironmentVariable" ->
        EnvironmentVariable
    | "ReadFile" ->
        ReadFile
    | "UserControlledEndpoint" ->
        Endpoint (Mangled.from_string "NONE", Typ.Tvoid)
    | _ ->
        Other


  let external_sources =
    List.filter_map
      ~f:(fun {QuandaryConfig.Source.procedure; kinds; index} ->
        parse_clang_procedure procedure kinds index )
      (QuandaryConfig.Source.of_json Config.quandary_sources)


  (* return a list of source kinds if [procedure_name] is in the list of externally specified sources *)
  let get_external_source qualified_pname =
    let return = None in
    List.concat_map external_sources ~f:(fun (qualifiers, kinds, index) ->
        if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname then
          let source_index = try Some (int_of_string index) with Failure _ -> return in
          List.rev_map kinds ~f:(fun kind -> (of_string kind, source_index))
        else [] )


  let get ~caller_pname:_ pname actuals tenv =
    let return = None in
    match pname with
    | Typ.Procname.ObjC_Cpp cpp_name -> (
        let qualified_pname = Typ.Procname.get_qualifiers pname in
        match
          ( QualifiedCppName.to_list
              (Typ.Name.unqualified_name (Typ.Procname.ObjC_Cpp.get_class_type_name cpp_name))
          , Typ.Procname.get_method pname )
        with
        | ( ["std"; ("basic_istream" | "basic_iostream")]
          , ("getline" | "read" | "readsome" | "operator>>") ) ->
            [(ReadFile, Some 1)]
        | _ ->
            get_external_source qualified_pname )
    | Typ.Procname.C _ when Typ.Procname.equal pname BuiltinDecl.__global_access -> (
        (* is this var a command line flag created by the popular C++ gflags library for creating
           command-line flags (https://github.com/gflags/gflags)? *)
        let is_gflag access_path =
          let pvar_is_gflag pvar =
            String.is_substring ~substring:"FLAGS_" (Pvar.get_simplified_name pvar)
          in
          match access_path with
          | (Var.ProgramVar pvar, _), _ ->
              Pvar.is_global pvar && pvar_is_gflag pvar
          | _ ->
              false
        in
        (* accessed global will be passed to us as the only parameter *)
        match List.map actuals ~f:HilExp.ignore_cast with
        | [HilExp.AccessExpression access_expr] ->
            let access_path = HilExp.AccessExpression.to_access_path access_expr in
            if is_gflag access_path then
              let (global_pvar, _), _ = access_path in
              let typ_desc =
                match AccessPath.get_typ access_path tenv with
                | Some {Typ.desc} ->
                    desc
                | None ->
                    Typ.void_star.desc
              in
              [(CommandLineFlag (global_pvar, typ_desc), None)]
            else []
        | _ ->
            [] )
    | Typ.Procname.C _ -> (
      match Typ.Procname.to_string pname with
      | "getenv" ->
          [(EnvironmentVariable, return)]
      | _ ->
          get_external_source (Typ.Procname.get_qualifiers pname) )
    | Typ.Procname.Block _ ->
        []
    | pname ->
        L.(die InternalError) "Non-C++ procname %a in C++ analysis" Typ.Procname.pp pname


  let get_tainted_formals pdesc tenv =
    if PredSymb.equal_access (Procdesc.get_attributes pdesc).ProcAttributes.access PredSymb.Private
    then Source.all_formals_untainted pdesc
    else
      let overrides_service_method pname tenv =
        PatternMatch.override_exists
          (function
            | Typ.Procname.ObjC_Cpp cpp_pname ->
                let class_name = Typ.Procname.ObjC_Cpp.get_class_name cpp_pname in
                let res =
                  String.is_suffix ~suffix:"SvIf" class_name
                  || String.is_suffix ~suffix:"SvAsyncIf" class_name
                in
                res
            | _ ->
                false)
          tenv pname
      in
      (* taint all formals except for [this] *)
      let taint_all_but_this_and_return ~make_source =
        List.map
          ~f:(fun (name, typ) ->
            let taint =
              match Mangled.to_string name with
              | "this" | "_return" ->
                  (* thrift methods implement returning values using dummy _return parameters that
                     the C++ code assigns to. these are sinks, not sources *)
                  None
              | _ ->
                  Some (make_source name typ.Typ.desc)
            in
            (name, typ, taint) )
          (Procdesc.get_formals pdesc)
      in
      match Procdesc.get_proc_name pdesc with
      | Typ.Procname.ObjC_Cpp cpp_pname as pname ->
          let qualified_pname =
            F.sprintf "%s::%s"
              (Typ.Procname.ObjC_Cpp.get_class_name cpp_pname)
              (Typ.Procname.get_method pname)
          in
          if QuandaryConfig.is_endpoint qualified_pname then
            taint_all_but_this_and_return ~make_source:(fun name desc ->
                UserControlledEndpoint (name, desc) )
          else if overrides_service_method pname tenv then
            taint_all_but_this_and_return ~make_source:(fun name desc -> Endpoint (name, desc))
          else Source.all_formals_untainted pdesc
      | _ ->
          Source.all_formals_untainted pdesc


  let pp fmt = function
    | Endpoint (formal_name, _) ->
        F.fprintf fmt "Endpoint(%s)" (Mangled.to_string formal_name)
    | EnvironmentVariable ->
        F.pp_print_string fmt "EnvironmentVariable"
    | ReadFile ->
        F.pp_print_string fmt "File"
    | CommandLineFlag (var, _) ->
        F.fprintf fmt "CommandLineFlag(%a)" Var.pp var
    | Other ->
        F.pp_print_string fmt "Other"
    | UserControlledEndpoint (formal_name, _) ->
        F.fprintf fmt "UserControlledEndpoint(%s)" (Mangled.to_string formal_name)
end

module CppSource = Source.Make (SourceKind)

module SinkKind = struct
  type t =
    | BufferAccess  (** read/write an array *)
    | CreateFile  (** create/open a file *)
    | EnvironmentChange  (** change environment variable or gflag *)
    | HeapAllocation  (** heap memory allocation *)
    | ShellExec  (** shell exec function *)
    | SQLInjection  (** unescaped query to a SQL database (could be read or write) *)
    | SQLRead  (** escaped read to a SQL database *)
    | SQLWrite  (** escaped write to a SQL database *)
    | StackAllocation  (** stack memory allocation *)
    | URL  (** URL creation *)
    | Other  (** for testing or uncategorized sinks *)
  [@@deriving compare]

  let matches ~caller ~callee = Int.equal 0 (compare caller callee)

  let of_string = function
    | "BufferAccess" ->
        BufferAccess
    | "CreateFile" ->
        CreateFile
    | "EnvironmentChange" ->
        EnvironmentChange
    | "HeapAllocation" ->
        HeapAllocation
    | "ShellExec" ->
        ShellExec
    | "SQLInjection" ->
        SQLInjection
    | "SQLRead" ->
        SQLRead
    | "SQLWrite" ->
        SQLWrite
    | "StackAllocation" ->
        StackAllocation
    | "URL" ->
        URL
    | _ ->
        Other


  let external_sinks =
    List.filter_map
      ~f:(fun {QuandaryConfig.Sink.procedure; kinds; index} ->
        parse_clang_procedure procedure kinds index )
      (QuandaryConfig.Sink.of_json Config.quandary_sinks)


  (* taint the nth parameter (0-indexed) *)
  let taint_nth n kinds actuals =
    if n < List.length actuals then
      let indexes = IntSet.singleton n in
      List.rev_map kinds ~f:(fun kind -> (kind, indexes))
    else []


  (* taint all parameters after the nth (exclusive) *)
  let taint_after_nth n kinds actuals =
    match
      List.filter_mapi ~f:(fun actual_num _ -> Option.some_if (actual_num > n) actual_num) actuals
    with
    | [] ->
        []
    | to_taint ->
        let indexes = IntSet.of_list to_taint in
        List.rev_map kinds ~f:(fun kind -> (kind, indexes))


  let taint_all kinds actuals =
    let indexes = IntSet.of_list (List.mapi ~f:(fun actual_num _ -> actual_num) actuals) in
    List.rev_map kinds ~f:(fun kind -> (kind, indexes))


  (* return Some(sink kinds) if [procedure_name] is in the list of externally specified sinks *)
  let get_external_sink pname actuals =
    let qualified_pname = Typ.Procname.get_qualifiers pname in
    List.find_map
      ~f:(fun (qualifiers, kinds, index) ->
        if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname then
          let kinds = List.rev_map ~f:of_string kinds in
          try
            let n = int_of_string index in
            let taint = taint_nth n kinds actuals in
            Option.some_if (not (List.is_empty taint)) taint
          with Failure _ ->
            (* couldn't parse the index, just taint everything *)
            Some (taint_all kinds actuals)
        else None )
      external_sinks
    |> Option.value ~default:[]


  let get pname actuals _ _ =
    let is_buffer_like pname =
      (* assume it's a buffer class if it's "vector-y", "array-y", or "string-y". don't want to
         report on accesses to maps etc., but also want to recognize custom vectors like fbvector
         rather than overfitting to std::vector *)
      let typename =
        Typ.Procname.get_qualifiers pname |> QualifiedCppName.strip_template_args
        |> QualifiedCppName.to_qual_string |> String.lowercase
      in
      String.is_substring ~substring:"vec" typename
      || String.is_substring ~substring:"array" typename
      || String.is_substring ~substring:"string" typename
    in
    match pname with
    | Typ.Procname.ObjC_Cpp cpp_name -> (
      match
        ( QualifiedCppName.to_list
            (Typ.Name.unqualified_name (Typ.Procname.ObjC_Cpp.get_class_type_name cpp_name))
        , Typ.Procname.get_method pname )
      with
      | ( ["std"; ("basic_fstream" | "basic_ifstream" | "basic_ofstream")]
        , ("basic_fstream" | "basic_ifstream" | "basic_ofstream" | "open") ) ->
          taint_nth 1 [CreateFile] actuals
      | _, "operator[]" when Config.developer_mode && is_buffer_like pname ->
          taint_nth 1 [BufferAccess] actuals
      | _ ->
          get_external_sink pname actuals )
    | Typ.Procname.C _
      when String.is_substring ~substring:"SetCommandLineOption" (Typ.Procname.to_string pname) ->
        taint_nth 1 [EnvironmentChange] actuals
    | Typ.Procname.C _
      when Config.developer_mode && Typ.Procname.equal pname BuiltinDecl.__array_access ->
        taint_all [BufferAccess] actuals
    | Typ.Procname.C _ when Typ.Procname.equal pname BuiltinDecl.__set_array_length ->
        (* called when creating a stack-allocated array *)
        taint_nth 1 [StackAllocation] actuals
    | Typ.Procname.C _ -> (
      match Typ.Procname.to_string pname with
      | "creat" | "fopen" | "freopen" | "open" ->
          taint_nth 0 [CreateFile] actuals
      | "curl_easy_setopt" -> (
          (* magic constant for setting request URL *)
          let controls_request = function
            | 10002 (* CURLOPT_URL *) | 10015 (* CURLOPT_POSTFIELDS *) ->
                true
            | _ ->
                false
          in
          (* first two actuals are curl object + integer code for data kind. *)
          match List.nth actuals 1 with
          | Some exp -> (
            match HilExp.eval exp with
            | Some (Const.Cint i) ->
                (* check if the data kind might be CURLOPT_URL *)
                IntLit.to_int i
                |> Option.value_map ~default:[] ~f:(fun n ->
                       if controls_request n then taint_after_nth 1 [URL] actuals else [] )
            | _ ->
                (* can't statically resolve data kind; taint it just in case *)
                taint_after_nth 1 [URL] actuals )
          | None ->
              [] )
      | "execl" | "execlp" | "execle" | "execv" | "execve" | "execvp" | "system" ->
          taint_all [ShellExec] actuals
      | "openat" ->
          taint_nth 1 [CreateFile] actuals
      | "popen" ->
          taint_nth 0 [ShellExec] actuals
      | "putenv" ->
          taint_nth 0 [EnvironmentChange] actuals
      | ("brk" | "calloc" | "malloc" | "realloc" | "sbrk") when Config.developer_mode ->
          taint_all [HeapAllocation] actuals
      | "rename" ->
          taint_all [CreateFile] actuals
      | "strcpy" when Config.developer_mode ->
          (* warn if source array is tainted *)
          taint_nth 1 [BufferAccess] actuals
      | ("memcpy" | "memmove" | "memset" | "strncpy" | "wmemcpy" | "wmemmove")
        when Config.developer_mode ->
          (* warn if count argument is tainted *)
          taint_nth 2 [BufferAccess] actuals
      | _ ->
          get_external_sink pname actuals )
    | Typ.Procname.Block _ ->
        []
    | pname ->
        L.(die InternalError) "Non-C++ procname %a in C++ analysis" Typ.Procname.pp pname


  let pp fmt kind =
    F.pp_print_string fmt
      ( match kind with
      | BufferAccess ->
          "BufferAccess"
      | CreateFile ->
          "CreateFile"
      | EnvironmentChange ->
          "EnvironmentChange"
      | HeapAllocation ->
          "HeapAllocation"
      | ShellExec ->
          "ShellExec"
      | SQLInjection ->
          "SQLInjection"
      | SQLRead ->
          "SQLRead"
      | SQLWrite ->
          "SQLWrite"
      | StackAllocation ->
          "StackAllocation"
      | URL ->
          "URL"
      | Other ->
          "Other" )
end

module CppSink = Sink.Make (SinkKind)

module CppSanitizer = struct
  type t =
    | EscapeShell  (** escape string to sanitize shell commands *)
    | EscapeSQL  (** escape string to sanitize SQL queries *)
    | EscapeURL  (** escape string to sanitize URLs (e.g., prevent injecting GET/POST params) *)
    | All  (** sanitizes all forms of taint *)
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let of_string = function
    | "EscapeShell" ->
        EscapeShell
    | "EscapeSQL" ->
        EscapeSQL
    | "EscapeURL" ->
        EscapeURL
    | _ ->
        All


  let external_sanitizers =
    List.map
      ~f:(fun {QuandaryConfig.Sanitizer.procedure; kind} ->
        (QualifiedCppName.Match.of_fuzzy_qual_names [procedure], of_string kind) )
      (QuandaryConfig.Sanitizer.of_json Config.quandary_sanitizers)


  let get pname _tenv =
    let qualified_pname = Typ.Procname.get_qualifiers pname in
    List.find_map
      ~f:(fun (qualifiers, kind) ->
        if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname then Some kind
        else None )
      external_sanitizers


  let pp fmt = function
    | EscapeShell ->
        F.pp_print_string fmt "EscapeShell"
    | EscapeSQL ->
        F.pp_print_string fmt "EscapeSQL"
    | EscapeURL ->
        F.pp_print_string fmt "EscapeURL"
    | All ->
        F.pp_print_string fmt "All"
end

include Trace.Make (struct
  module Source = CppSource
  module Sink = CppSink
  module Sanitizer = CppSanitizer

  (* return true if code injection is possible because the source is a string/is not sanitized with
     [escape_sanitizer] *)
  let is_injection_possible ?typ escape_sanitizer sanitizers =
    let is_escaped = List.mem sanitizers escape_sanitizer ~equal:Sanitizer.equal in
    (not is_escaped)
    &&
    match typ with
    | Some (Typ.Tint _ | Tfloat _ | Tvoid) ->
        false
    | _ ->
        (* possible a string/object/struct type; assume injection possible *)
        true


  let get_report source sink sanitizers =
    match (Source.kind source, Sink.kind sink) with
    | _ when List.mem sanitizers Sanitizer.All ~equal:Sanitizer.equal ->
        (* the All sanitizer clears any form of taint; don't report *)
        None
    | (Endpoint (_, typ) | UserControlledEndpoint (_, typ)), CreateFile ->
        Option.some_if
          (is_injection_possible ~typ Sanitizer.EscapeShell sanitizers)
          IssueType.untrusted_file_risk
    | (Endpoint (_, typ) | UserControlledEndpoint (_, typ)), URL ->
        Option.some_if
          (is_injection_possible ~typ Sanitizer.EscapeURL sanitizers)
          IssueType.untrusted_url_risk
    | ( (CommandLineFlag (_, typ) | Endpoint (_, typ) | UserControlledEndpoint (_, typ))
      , SQLInjection ) ->
        if is_injection_possible ~typ Sanitizer.EscapeSQL sanitizers then
          (* SQL injection if the caller of the endpoint doesn't sanitize on its end *)
          Some IssueType.sql_injection_risk
        else
          (* no injection risk, but still user-controlled *)
          Some IssueType.user_controlled_sql_risk
    | (Endpoint _ | UserControlledEndpoint _), (SQLRead | SQLWrite) ->
        (* no injection risk, but still user-controlled *)
        Some IssueType.user_controlled_sql_risk
    | (Endpoint _ | UserControlledEndpoint _), EnvironmentChange ->
        (* user-controlled environment mutation *)
        Some IssueType.untrusted_environment_change_risk
    | (CommandLineFlag (_, typ) | Endpoint (_, typ) | UserControlledEndpoint (_, typ)), ShellExec
      ->
        (* code injection if the caller of the endpoint doesn't sanitize on its end *)
        Option.some_if
          (is_injection_possible ~typ Sanitizer.EscapeShell sanitizers)
          IssueType.shell_injection_risk
    | ( ( UserControlledEndpoint _
        | Endpoint _
        | CommandLineFlag _
        | EnvironmentVariable
        | ReadFile
        | Other )
      , BufferAccess ) ->
        (* untrusted data of any kind flowing to buffer *)
        Some IssueType.untrusted_buffer_access
    | (EnvironmentVariable | ReadFile | Other), ShellExec ->
        (* environment var, or file data flowing to shell *)
        Option.some_if
          (is_injection_possible Sanitizer.EscapeShell sanitizers)
          IssueType.shell_injection
    | (EnvironmentVariable | ReadFile | Other), SQLInjection ->
        (* untrusted flag, environment var, or file data flowing to SQL *)
        Option.some_if
          (is_injection_possible Sanitizer.EscapeSQL sanitizers)
          IssueType.sql_injection
    | Other, URL ->
        (* untrusted flag, environment var, or file data flowing to URL *)
        Option.some_if
          (is_injection_possible Sanitizer.EscapeURL sanitizers)
          IssueType.untrusted_url_risk
    | ( ( CommandLineFlag _
        | Endpoint _
        | UserControlledEndpoint _
        | EnvironmentVariable
        | ReadFile
        | Other )
      , HeapAllocation ) ->
        (* untrusted data of any kind flowing to heap allocation. this can cause crashes or DOS. *)
        Some IssueType.untrusted_heap_allocation
    | ( ( CommandLineFlag _
        | Endpoint _
        | UserControlledEndpoint _
        | EnvironmentVariable
        | ReadFile
        | Other )
      , StackAllocation ) ->
        (* untrusted data of any kind flowing to stack buffer allocation. trying to allocate a stack
           buffer that's too large will cause a stack overflow. *)
        Some IssueType.untrusted_variable_length_array
    | ( (CommandLineFlag _ | EnvironmentVariable | ReadFile)
      , (CreateFile | EnvironmentChange | SQLRead | SQLWrite | URL) ) ->
        None
    | Other, _ ->
        (* Other matches everything *)
        Some IssueType.quandary_taint_error
    | _, Other ->
        Some IssueType.quandary_taint_error
end)
