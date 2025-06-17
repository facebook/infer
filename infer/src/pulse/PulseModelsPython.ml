(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
module DSL = PulseModelsDSL

let bool_tname = TextualSil.python_bool_type_name

let dict_tname = TextualSil.python_dict_type_name

let int_tname = TextualSil.python_int_type_name

let string_tname = TextualSil.python_string_type_name

let tuple_tname = TextualSil.python_tuple_type_name

let none_tname = TextualSil.python_none_type_name

(* sys.stdlib_module_names *)
let stdlib_modules : IString.Set.t =
  IString.Set.of_list
    [ "abc"
    ; "aifc"
    ; "antigravity"
    ; "argparse"
    ; "array"
    ; "ast"
    ; "asynchat"
    ; "asyncio"
    ; "asyncore"
    ; "atexit"
    ; "audioop"
    ; "base64"
    ; "bdb"
    ; "binascii"
    ; "binhex"
    ; "bisect"
    ; "builtins"
    ; "bz2"
    ; "cProfile"
    ; "calendar"
    ; "cgi"
    ; "cgitb"
    ; "chunk"
    ; "cmath"
    ; "cmd"
    ; "code"
    ; "codecs"
    ; "codeop"
    ; "collections"
    ; "colorsys"
    ; "compileall"
    ; "concurrent"
    ; "configparser"
    ; "contextlib"
    ; "contextvars"
    ; "copy"
    ; "copyreg"
    ; "crypt"
    ; "csv"
    ; "ctypes"
    ; "curses"
    ; "dataclasses"
    ; "datetime"
    ; "dbm"
    ; "decimal"
    ; "difflib"
    ; "dis"
    ; "distutils"
    ; "doctest"
    ; "email"
    ; "encodings"
    ; "ensurepip"
    ; "enum"
    ; "errno"
    ; "faulthandler"
    ; "fcntl"
    ; "filecmp"
    ; "fileinput"
    ; "fnmatch"
    ; "fractions"
    ; "ftplib"
    ; "functools"
    ; "gc"
    ; "genericpath"
    ; "getopt"
    ; "getpass"
    ; "gettext"
    ; "glob"
    ; "graphlib"
    ; "grp"
    ; "gzip"
    ; "hashlib"
    ; "heapq"
    ; "hmac"
    ; "html"
    ; "http"
    ; "idlelib"
    ; "imaplib"
    ; "imghdr"
    ; "imp"
    ; "importlib"
    ; "inspect"
    ; "io"
    ; "ipaddress"
    ; "itertools"
    ; "json"
    ; "keyword"
    ; "lib2to3"
    ; "linecache"
    ; "locale"
    ; "logging"
    ; "lzma"
    ; "mailbox"
    ; "mailcap"
    ; "marshal"
    ; "math"
    ; "mimetypes"
    ; "mmap"
    ; "modulefinder"
    ; "msilib"
    ; "msvcrt"
    ; "multiprocessing"
    ; "netrc"
    ; "nis"
    ; "nntplib"
    ; "nt"
    ; "ntpath"
    ; "nturl2path"
    ; "numbers"
    ; "opcode"
    ; "operator"
    ; "optparse"
    ; "os"
    ; "ossaudiodev"
    ; "pathlib"
    ; "pdb"
    ; "pickle"
    ; "pickletools"
    ; "pipes"
    ; "pkgutil"
    ; "platform"
    ; "plistlib"
    ; "poplib"
    ; "posix"
    ; "posixpath"
    ; "pprint"
    ; "profile"
    ; "pstats"
    ; "pty"
    ; "pwd"
    ; "py_compile"
    ; "pyclbr"
    ; "pydoc"
    ; "pydoc_data"
    ; "pyexpat"
    ; "queue"
    ; "quopri"
    ; "random"
    ; "re"
    ; "readline"
    ; "reprlib"
    ; "resource"
    ; "rlcompleter"
    ; "runpy"
    ; "sched"
    ; "secrets"
    ; "select"
    ; "selectors"
    ; "shelve"
    ; "shlex"
    ; "shutil"
    ; "signal"
    ; "site"
    ; "smtpd"
    ; "smtplib"
    ; "sndhdr"
    ; "socket"
    ; "socketserver"
    ; "spwd"
    ; "sqlite3"
    ; "sre_compile"
    ; "sre_constants"
    ; "sre_parse"
    ; "ssl"
    ; "stat"
    ; "statistics"
    ; "string"
    ; "stringprep"
    ; "struct"
    ; "subprocess"
    ; "sunau"
    ; "symtable"
    ; "sys"
    ; "sysconfig"
    ; "syslog"
    ; "tabnanny"
    ; "tarfile"
    ; "telnetlib"
    ; "tempfile"
    ; "termios"
    ; "textwrap"
    ; "this"
    ; "threading"
    ; "time"
    ; "timeit"
    ; "tkinter"
    ; "token"
    ; "tokenize"
    ; "trace"
    ; "traceback"
    ; "tracemalloc"
    ; "tty"
    ; "turtle"
    ; "turtledemo"
    ; "types"
    ; "typing"
    ; "unicodedata"
    ; "unittest"
    ; "urllib"
    ; "uu"
    ; "uuid"
    ; "venv"
    ; "warnings"
    ; "wave"
    ; "weakref"
    ; "webbrowser"
    ; "winreg"
    ; "winsound"
    ; "wsgiref"
    ; "xdrlib"
    ; "xml"
    ; "xmlrpc"
    ; "zipapp"
    ; "zipfile"
    ; "zipimport"
    ; "zlib"
    ; "zoneinfo" ]


let module_tname module_name = Typ.PythonClass (PythonClassName.Globals module_name)

let as_constant_string_exn aval : string DSL.model_monad =
  let open DSL.Syntax in
  let* opt_str = as_constant_string aval in
  let str =
    Option.value_or_thunk opt_str ~default:(fun () ->
        L.die InternalError "Python frontend should have put a constant string here" )
  in
  ret str


module ThreeValuedLogic = struct
  type t = bool option

  let is_true = function None -> false | Some b -> b
end

module Dict = struct
  let make keys args ~const_strings_only : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    if not (Int.equal (List.length args) (List.length keys)) then
      L.die InternalError "Dict.make expects two list of same length@\n" ;
    let bindings = List.zip_exn keys args in
    construct_dict ~deref:false dict_tname bindings ~const_strings_only


  let propagate_static_type_on_load dict key load_res : unit DSL.model_monad =
    let open DSL.Syntax in
    let rec propagate_field_type seen tname key =
      let field = Fieldname.make tname key in
      if Fieldname.Set.mem field seen then
        (* such a strange cyclic depency can happen in a file that imports itself *)
        ret ()
      else
        let seen = Fieldname.Set.add field seen in
        let* opt_info = tenv_resolve_field_info tname field in
        match opt_info with
        | Some {Struct.typ= field_typ} -> (
          match Typ.name (Typ.strip_ptr field_typ) with
          | Some (PythonClass (ModuleAttribute {module_name; attr_name})) ->
              let tname = Typ.PythonClass (Globals module_name) in
              let* tname_is_defined = tenv_type_is_defined tname in
              if tname_is_defined then propagate_field_type seen tname attr_name
              else
                Typ.Name.Python.concatenate_package_name_and_file_name tname attr_name
                |> option_iter ~f:(fun static_tname -> add_static_type static_tname load_res)
          | Some static_tname ->
              add_static_type static_tname load_res
          | None ->
              ret () )
        | None -> (
          match tname with
          | Typ.PythonClass (Globals module_name) ->
              let static_type = Typ.PythonClass (ModuleAttribute {module_name; attr_name= key}) in
              add_static_type static_type load_res
          | _ ->
              ret () )
    in
    let* opt_static_type = get_static_type dict in
    option_iter opt_static_type ~f:(fun tname -> propagate_field_type Fieldname.Set.empty tname key)


  let contains_str_key dict key : ThreeValuedLogic.t DSL.model_monad =
    let open DSL.Syntax in
    let* fields = get_known_fields dict in
    let field_names =
      List.fold fields ~init:[] ~f:(fun acc field ->
          match (field : Access.t) with
          | FieldAccess fieldname ->
              Fieldname.to_string fieldname :: acc
          | _ ->
              acc )
    in
    if List.mem field_names key ~equal:String.( = ) then Some true |> ret
    else
      let* dict_contains_const_keys = is_dict_contain_const_keys dict in
      if dict_contains_const_keys then Some false |> ret else None |> ret


  let get_str_key ?(dict_read_const_key = false) ?(propagate_static_type = false) dict key :
      DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make dict_tname key in
    let* () = if dict_read_const_key then add_dict_read_const_key dict field else ret () in
    let* load_res = load_access ~deref:false dict (FieldAccess field) in
    let* () =
      if propagate_static_type then propagate_static_type_on_load dict key load_res else ret ()
    in
    (* note: we do not try static type propagation here *)
    ret load_res


  (* beware: key is expected to be a constant string! *)
  let get_exn dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* key = as_constant_string_exn key in
    let* load_res = get_str_key ~propagate_static_type:true dict key in
    ret load_res


  (* used for py dict access
     1. Doesn't throw error if key is not str
     2. Enables DictMissingKey error through dict_read_const_key option
  *)
  let get dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* key = as_constant_string key in
    let* res =
      match key with
      | None ->
          fresh ()
      | Some key ->
          get_str_key ~dict_read_const_key:true ~propagate_static_type:true dict key
    in
    ret res


  let set_str_key dict key value : unit DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make dict_tname key in
    let* () = store_field ~deref:false ~ref:dict field value in
    ret ()


  (* beware: key is expected to be a constant string! *)
  let set dict key value : unit DSL.model_monad =
    let open DSL.Syntax in
    let* key = as_constant_string_exn key in
    set_str_key dict key value


  let builtin args : DSL.aval DSL.model_monad =
    let default_value ~const_strings_only = make [] [] ~const_strings_only in
    let open DSL.Syntax in
    match args with
    | [] ->
        default_value ~const_strings_only:true
    | [arg] -> (
        let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg in
        match arg_dynamic_type_data with
        | Some {Formula.typ= {desc= Tstruct (PythonClass (Builtin PyDict))}} ->
            ret arg
        | _ ->
            default_value ~const_strings_only:false )
    | _ ->
        default_value ~const_strings_only:false
end

module Integer = struct
  let make arg : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* opt_int = as_constant_int arg in
    match opt_int with
    | None ->
        constructor ~deref:false int_tname []
    | Some i ->
        let* int_value = int i in
        (* we wrap the Pulse integer in a box, because all Pulse integers with a same value are shared *)
        constructor ~deref:false int_tname [("val", int_value)]


  let get boxed_int : int option DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make int_tname "val" in
    let* unboxed = load_access ~deref:false boxed_int (FieldAccess field) in
    as_constant_int unboxed
end

module Bool = struct
  let make bool : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    (* Contrary to Integer above, we don't wrap the pulse integer value. Should be safe as long as it is the only
       pulse integer value tagged with a dynamic type information *)
    let* bool = int (if bool then 1 else 0) in
    let* () = and_dynamic_type_is bool (Typ.mk_struct bool_tname) in
    ret bool


  let make_random () : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* res = fresh () in
    let* () = and_dynamic_type_is res (Typ.mk_struct bool_tname) in
    ret res
end

module Tuple = struct
  let str_field_of_int i = F.asprintf "#%d" i

  let make args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let bindings =
      List.mapi args ~f:(fun i aval ->
          let field = str_field_of_int i in
          (field, aval) )
    in
    let* dict = constructor ~deref:false tuple_tname bindings in
    ret dict


  let get_idx_int tuple i : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make tuple_tname (str_field_of_int i) in
    load_access ~deref:false tuple (FieldAccess field)


  let get tuple idx : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* opt_int =
      dynamic_dispatch idx
        ~cases:[(int_tname, fun () -> Integer.get idx)]
        ~default:(fun () -> ret None)
    in
    match opt_int with None -> fresh () | Some i -> get_idx_int tuple i


  let append _list value : unit DSL.model_monad =
    let open DSL.Syntax in
    remove_allocation_attr_transitively [value]
end

module PyModule = struct
  let make name : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    constructor ~deref:false (module_tname name) []
end

let build_tuple args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* tuple = Tuple.make args in
  assign_ret tuple


let init_method_name = "__init__"

let init_method_name_suffix = "::" ^ init_method_name

let super_attribute_name = "__infer_single_super"

let class_companion_attribute_name = "__infer_class_companion"

let build_class closure _name base_classes () : unit DSL.model_monad =
  let open DSL.Syntax in
  (* Note: we only deal with single inheritance for now *)
  let super_keys, super_values =
    match base_classes with
    | [] ->
        ([], [])
    | base_class :: _ignored_for_now ->
        ([super_attribute_name], [base_class])
  in
  let* class_ = Dict.make super_keys super_values ~const_strings_only:true in
  let gen_closure_args _ = ret [class_] in
  let* _ = apply_python_closure closure gen_closure_args in
  assign_ret class_


let call_closure_dsl ~closure ~arg_names:_ ~args : DSL.aval DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let gen_closure_args opt_proc_attrs =
    let python_args =
      match opt_proc_attrs with
      | Some {ProcAttributes.python_args} when List.length python_args <= List.length args ->
          python_args
      | Some {ProcAttributes.python_args} ->
          L.d_printfln "[ocaml model] %d argument required but %d were given"
            (List.length python_args) (List.length args) ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
      | None ->
          L.d_printfln "[ocaml model] Failed to load attributes" ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
    in
    let positional_args, named_args = List.split_n args (List.length python_args) in
    let* locals = Dict.make python_args positional_args ~const_strings_only:true in
    let* () = remove_allocation_attr_transitively named_args in
    ret [locals]
  in
  apply_python_closure closure gen_closure_args


let try_run_constructor ~class_companion ~self args =
  let open DSL.Syntax in
  let* init_closure =
    Dict.get_str_key ~propagate_static_type:true class_companion init_method_name
  in
  let* opt = get_dynamic_type ~ask_specialization:true init_closure in
  if Option.is_some opt then
    let args = self :: args in
    call_closure_dsl ~closure:init_closure ~arg_names:[] ~args |> ignore
  else ret ()


let await_awaitable arg : unit DSL.model_monad =
  fst arg |> AddressAttributes.await_awaitable |> DSL.Syntax.exec_command


let must_be_awaited arg : unit DSL.model_monad =
  let open DSL.Syntax in
  let* opt_allocation_trace = get_unawaited_awaitable arg in
  match opt_allocation_trace with
  | None ->
      abduce_must_be_awaited arg
  | Some allocation_trace ->
      report_unawaited_awaitable allocation_trace @@> await_awaitable arg


let make_type arg : DSL.aval option DSL.model_monad =
  let open DSL.Syntax in
  let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg in
  let* res = fresh () in
  match arg_dynamic_type_data with
  | Some {Formula.typ= {desc= Tstruct (PythonClass (Builtin builtin_type))}} -> (
    match PythonClassName.get_builtin_closure_from_builtin_type builtin_type with
    | Some builtin_closure ->
        let* () =
          and_dynamic_type_is res
            (Typ.mk_struct (PythonClass (PythonClassName.BuiltinClosure builtin_closure)))
        in
        ret (Some res)
    | None ->
        ret (Some res) )
  | _ ->
      ret (Some res)


let make_str_internal arg : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* opt_str = as_constant_string arg in
  match opt_str with
  | None ->
      constructor ~deref:false string_tname []
  | Some i ->
      let* res = string i in
      let* () = and_dynamic_type_is res (Typ.mk_struct string_tname) in
      ret res


let is_module_captured module_name =
  let function_name = "__module_body__" in
  let module_name = PythonClassName.Filename module_name in
  let proc_name = Procname.make_python ~module_name ~function_name in
  IRAttributes.load proc_name |> Option.map ~f:(fun _ -> proc_name)


let lookup_module module_name =
  let open DSL.Syntax in
  match is_module_captured module_name with
  | Some proc_name ->
      let* module_ = PyModule.make module_name in
      ret (module_, Some proc_name)
  | None -> (
      (* it is not a capture module name *)
      let module_name_init = module_name ^ init_method_name_suffix in
      match is_module_captured module_name_init with
      | None ->
          (* neither it is a captured package name *)
          if not (IString.Set.mem module_name stdlib_modules) then
            StatsLogging.log_message ~label:"python_missing_module" ~message:module_name ;
          let* module_ = PyModule.make module_name in
          ret (module_, None)
      | Some proc_name ->
          (* it is a captured package name *)
          let* module_ = PyModule.make module_name_init in
          ret (module_, Some proc_name) )


let get_supers_class_names ~module_name ~class_name =
  let open DSL.Syntax in
  let type_name : Typ.name = PythonClass (ClassCompanion {module_name; class_name}) in
  let* supers = tenv_get_supers type_name in
  List.filter_map supers ~f:(function
    | Typ.PythonClass (ClassCompanion {class_name}) ->
        Some class_name
    | _ ->
        None )
  |> ret


let initialize_class_companion_and_supers ~module_name ~class_name class_companion_object =
  let open DSL.Syntax in
  let* module_, _ = lookup_module module_name in
  let call_class_initializer class_name class_companion_object =
    let module_name = PythonClassName.Filename module_name in
    let class_initializer = Procname.make_python ~module_name ~function_name:class_name in
    python_call class_initializer [("globals", module_); ("locals", class_companion_object)]
    |> ignore
  in
  let seen, marked_as_seen =
    let set = ref IString.Set.empty in
    let seen name = IString.Set.mem name !set in
    let marked_as_seen name = set := IString.Set.add name !set in
    (seen, marked_as_seen)
  in
  let rec initialize class_name class_companion_object =
    if seen class_name then (* should not happen but you can never be too cautious *)
      ret ()
    else (
      marked_as_seen class_name ;
      let* supers = get_supers_class_names ~module_name ~class_name in
      if List.is_empty supers then (
        L.d_printfln ~color:Orange "initializing class %s" class_name ;
        call_class_initializer class_name class_companion_object )
      else (
        L.d_printfln ~color:Orange "initializing class %s and its super classe(s) %a" class_name
          (Pp.comma_seq F.pp_print_string) supers ;
        let single_super_class_name = List.hd_exn supers in
        (* we dont deal with multi-inheritance yet *)
        let* parent_class_companion =
          Dict.get_str_key ~propagate_static_type:true class_companion_object super_attribute_name
        in
        let* () = initialize single_super_class_name parent_class_companion in
        call_class_initializer class_name class_companion_object ) )
  in
  initialize class_name class_companion_object


let get_class_companion aval =
  let open DSL.Syntax in
  let* opt_static_type = get_static_type aval in
  match opt_static_type with
  | Some (Typ.PythonClass (ClassCompanion {module_name; class_name})) ->
      ret (Some (module_name, class_name))
  | _ ->
      ret None


let initialize_if_class_companion value : unit DSL.model_monad =
  let open DSL.Syntax in
  let* opt_class_companion = get_class_companion value in
  option_iter opt_class_companion ~f:(fun (module_name, class_name) ->
      let* already_initialized = Dict.contains_str_key value "__name__" in
      if ThreeValuedLogic.is_true already_initialized then ret ()
      else (
        L.d_printfln ~color:Orange "initializing class companion %s" class_name ;
        initialize_class_companion_and_supers ~module_name ~class_name value ) )


let is_async_function_name function_name =
  Option.exists Config.python_async_function_naming_convention_regex ~f:(fun regexp ->
      Str.string_match regexp function_name 0 )


let is_async_method_name method_name =
  Option.exists Config.python_async_method_naming_convention_regex ~f:(fun regexp ->
      Str.string_match regexp method_name 0 )


let detect_async_call opt_static_typ =
  match opt_static_typ with
  | Some (Typ.PythonClass (Globals name)) ->
      is_async_function_name name
  | _ ->
      false


let call_dsl ~callable ~arg_names ~args =
  let open DSL.Syntax in
  let* opt_static_type = get_static_type callable in
  match opt_static_type with
  | Some (Typ.PythonClass (ClassCompanion {module_name; class_name})) ->
      let instance_type = Typ.PythonClass (ClassInstance {module_name; class_name}) in
      L.d_printfln ~color:Orange "calling a constructor for type %a" Typ.Name.pp instance_type ;
      let* () = initialize_if_class_companion callable in
      let* self = constructor ~deref:false instance_type [] in
      let* () = Dict.set_str_key self class_companion_attribute_name callable in
      let* () = try_run_constructor ~class_companion:callable ~self args in
      ret self
  | _ ->
      L.d_printfln ~color:Orange "calling closure %a" AbstractValue.pp (fst callable) ;
      let* res = call_closure_dsl ~closure:callable ~arg_names ~args in
      let* () =
        if detect_async_call opt_static_type then allocation Attribute.Awaitable res else ret ()
      in
      ret res


module LibModel = struct
  let match_pattern ~pattern ~module_name ~name =
    Option.exists pattern ~f:(fun regexp ->
        let str = Printf.sprintf "%s::%s" module_name name in
        Str.string_match regexp str 0 )


  let is_release ~module_name ~name =
    match_pattern ~pattern:Config.pulse_model_release_pattern ~module_name ~name


  let is_deep_release ~module_name ~name =
    match_pattern ~pattern:Config.pulse_model_deep_release_pattern ~module_name ~name


  let is_await_sync_decorator ~module_name ~name =
    IString.PairSet.mem (module_name, name) Config.python_decorator_modelled_as_await_async


  let gen_awaitable _args =
    let open DSL.Syntax in
    let* res = fresh () in
    let* () = allocation Attribute.Awaitable res in
    ret (Some res)


  let await_sync_decorator_registered args =
    (* this is executed when the decorated function is registered *)
    let open DSL.Syntax in
    let* res =
      match args with
      | [registered_closure] ->
          let typ = Typ.PythonClass (BuiltinClosure AwaitAsyncDecorator) in
          constructor ~deref:false typ [("fun", registered_closure)]
      | _ ->
          fresh ()
    in
    ret (Some res)


  let await_sync_decorator_called callable args =
    (* this is executed when the decorated function is called *)
    call_dsl ~callable ~arg_names:None ~args


  let deep_release args =
    let open DSL.Syntax in
    let* () = remove_allocation_attr_transitively args in
    let* res = fresh () in
    ret (Some res)


  let release args =
    let open DSL.Syntax in
    let* () = list_iter args ~f:await_awaitable in
    let* res = fresh () in
    ret (Some res)
end

type pymodel =
  | PyLib of {module_name: string; name: string}
  | PyBuiltin of PythonClassName.builtin_closure

(* Only Python frontend builtins ($builtins.py_) have a C-style syntax, so we
   must catch other specific calls here *)

let modelled_python_call model closure args : DSL.aval option DSL.model_monad =
  let open DSL.Syntax in
  match (model, args) with
  | PyLib {module_name= "asyncio"; name= "run"}, _ ->
      LibModel.release args
  | PyLib {module_name; name}, _ when LibModel.is_release ~module_name ~name ->
      LibModel.release args
  | PyLib {module_name= "asyncio"; name= "gather"}, _ ->
      LibModel.deep_release args
  | PyLib {module_name; name}, _ when LibModel.is_deep_release ~module_name ~name ->
      LibModel.deep_release args
  | PyLib {module_name= "asyncio"; name= "sleep"}, _ ->
      LibModel.gen_awaitable args
  | PyLib {module_name; name}, _ when LibModel.is_await_sync_decorator ~module_name ~name ->
      LibModel.await_sync_decorator_registered args
  | PyBuiltin AwaitAsyncDecorator, args ->
      let* res = LibModel.await_sync_decorator_called closure args in
      ret (Some res)
  | PyBuiltin DictFun, args ->
      let* dict = Dict.builtin args in
      ret (Some dict)
  | PyBuiltin IntFun, [arg] ->
      let* res = Integer.make arg in
      ret (Some res)
  | PyBuiltin StrFun, [arg] ->
      let* res = make_str_internal arg in
      ret (Some res)
  | PyBuiltin TypeFun, [arg] ->
      make_type arg
  | PyBuiltin TypeFun, _ | PyBuiltin IntFun, _ | PyBuiltin StrFun, _ ->
      let* res = fresh () in
      ret (Some res)
  | PyLib _, _ ->
      ret None


let try_catch_lib_model type_name closure args =
  let open DSL.Syntax in
  match Typ.Name.Python.split_module_attr type_name with
  | Some (module_name, name) ->
      modelled_python_call (PyLib {module_name; name}) closure args
  | _ ->
      ret None


let try_catch_lib_model_using_static_type ~default closure args =
  let open DSL.Syntax in
  let* opt_static_type = get_static_type closure in
  match opt_static_type with
  | Some (Typ.PythonClass (ModuleAttribute {module_name; attr_name= name}) as type_name) -> (
      let* opt_special_call = modelled_python_call (PyLib {module_name; name}) closure args in
      match opt_special_call with
      | None ->
          default ()
      | Some res ->
          L.d_printfln "catching reserved lib call using static type %a" Typ.Name.pp type_name ;
          ret res )
  | _ ->
      default ()


let call_function_ex closure tuple dict () : unit DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true closure in
  let args = [tuple; dict] in
  let* res =
    match opt_dynamic_type_data with
    | None ->
        try_catch_lib_model_using_static_type ~default:fresh closure args
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
        let* opt_catched_lib_model_res = try_catch_lib_model type_name closure args in
        match opt_catched_lib_model_res with
        | Some res ->
            L.d_printfln "catching reserved lib call using dynamic type %a" Typ.Name.pp type_name ;
            ret res
        | None ->
            fresh () )
    | _ ->
        fresh ()
  in
  assign_ret res


let gen_start_coroutine () : unit DSL.model_monad =
  let open DSL.Syntax in
  ret ()


let get_class_instance aval =
  (* For now we just try to catch instances using static types. This is a bit limited in
     case of interprocedural propagation *)
  let open DSL.Syntax in
  let* opt_static_type = get_dynamic_type ~ask_specialization:true aval in
  match opt_static_type with
  | Some {Formula.typ= {desc= Tstruct (Typ.PythonClass (ClassInstance {module_name; class_name}))}}
    ->
      ret (Some (module_name, class_name))
  | _ ->
      ret None


type attribute_search_result =
  | Found of {result: DSL.aval; class_object: DSL.aval}
  | NotFoundMayBeMissing
(* TODO add NotFoundMustBeMissing *)

let rec get_attr_class_object_dsl class_object attr : attribute_search_result DSL.model_monad =
  let open DSL.Syntax in
  let* key_in_current_class = Dict.contains_str_key class_object attr in
  if ThreeValuedLogic.is_true key_in_current_class then
    let* result = Dict.get_str_key ~propagate_static_type:true class_object attr in
    ret (Found {result; class_object})
  else
    let* must_have_super =
      is_known_field class_object (Fieldname.make dict_tname super_attribute_name)
    in
    if must_have_super then
      let* super = Dict.get_str_key ~propagate_static_type:true class_object super_attribute_name in
      get_attr_class_object_dsl super attr
    else
      ret NotFoundMayBeMissing (* refine the logic to return NotFoundMustBeMissing when possible *)


let lookup_class_companion ~module_name ~class_name obj : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* has_companion_class = Dict.contains_str_key obj class_companion_attribute_name in
  if ThreeValuedLogic.is_true has_companion_class then
    Dict.get_str_key ~propagate_static_type:true obj class_companion_attribute_name
  else
    (* for now, we don't try to set the [class_companion_attribute_name] field in this case *)
    let* class_companion_object = Dict.make ~const_strings_only:true [] [] in
    let* () =
      initialize_class_companion_and_supers ~module_name ~class_name class_companion_object
    in
    ret class_companion_object


let get_attr_dsl obj attr : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* attr = as_constant_string_exn attr in
  (* TODO: look into companion class object if necessary *)
  let* key_in = Dict.contains_str_key obj attr in
  let* res =
    if ThreeValuedLogic.is_true key_in then Dict.get_str_key ~propagate_static_type:true obj attr
    else
      let* opt_class_instance = get_class_instance obj in
      match opt_class_instance with
      | None ->
          (* will return an unknown value and assume the key was there, but an other option would
             be to mark this case as unreachable. We may come back later if specialization gives us
             some dynamic type for [obj]. *)
          Dict.get_str_key ~propagate_static_type:true obj attr
      | Some (module_name, class_name) -> (
          let* class_companion_object = lookup_class_companion ~module_name ~class_name obj in
          L.d_printfln ~color:Orange
            "checking if the attribute %s belongs to the companion class %s.%s (in value %a)...@;"
            attr module_name class_name AbstractValue.pp (fst class_companion_object) ;
          let* try_get_attribute_from_companion =
            get_attr_class_object_dsl class_companion_object attr
          in
          match try_get_attribute_from_companion with
          | Found {result; class_object} ->
              L.d_printfln ~color:Orange "attribute was find in %a@;" AbstractValue.pp
                (fst class_object) ;
              ret result
          | _ ->
              Dict.get_str_key ~propagate_static_type:true obj attr )
  in
  let* () = initialize_if_class_companion res in
  ret res


let get_attr obj attr () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = get_attr_dsl obj attr in
  assign_ret res


let call callable arg_names args () : unit DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true callable in
  let* res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (BuiltinClosure builtin))}} -> (
        let* opt_special_call = modelled_python_call (PyBuiltin builtin) callable args in
        match opt_special_call with
        | None ->
            L.die InternalError "builtin %s was not successfully recognized"
              (PythonClassName.to_string (BuiltinClosure builtin))
        | Some res ->
            L.d_printfln "catching reserved builtin call %s"
              (PythonClassName.to_string (BuiltinClosure builtin)) ;
            ret res )
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
        let* opt_catched_lib_model_res = try_catch_lib_model type_name callable args in
        match opt_catched_lib_model_res with
        | Some res ->
            L.d_printfln "catching reserved lib call using dynamic type %a" Typ.Name.pp type_name ;
            ret res
        | None ->
            call_dsl ~callable ~arg_names ~args )
    | _ ->
        let default () = call_dsl ~callable ~arg_names ~args in
        try_catch_lib_model_using_static_type ~default callable args
  in
  assign_ret res


let call_method name obj arg_names args () : unit DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  L.d_printfln "call_method model" ;
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true obj in
  let* str_name = as_constant_string_exn name in
  let* res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (Globals module_name))}} -> (
        (* since module types are final, static type will save us most of the time *)
        let* opt_special_call =
          modelled_python_call (PyLib {module_name; name= str_name}) obj args
        in
        match opt_special_call with
        | None ->
            L.d_printfln "calling method %s on module object %s" str_name module_name ;
            let* callable = Dict.get_exn obj name in
            call_dsl ~callable ~arg_names ~args
        | Some res ->
            L.d_printfln "catching special call %s on module object %s" str_name module_name ;
            ret res )
    | _ ->
        L.d_printfln ~color:Orange "can not resolve method call!" ;
        let* callable = get_attr_dsl obj name in
        (* TODO: for OO method, gives self argument *)
        let* res = call_dsl ~callable ~arg_names ~args:(obj :: args) in
        let* () =
          if is_async_method_name str_name then allocation Attribute.Awaitable res else ret ()
        in
        ret res
  in
  assign_ret res


let get_awaitable arg () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* () = await_awaitable arg in
  assign_ret arg


let dict_set_item _dict _key value () : unit DSL.model_monad =
  (* TODO: when the dict is a constant, we could just update it *)
  await_awaitable value


let is_package aval : string option DSL.model_monad =
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:false aval in
  ret
    ( match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (Filename name))}}
      when String.is_suffix name ~suffix:init_method_name_suffix ->
        Some name
    | _ ->
        None )


let is_global aval : bool DSL.model_monad =
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:false aval in
  ret
    ( match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (Globals _))}} ->
        true
    | _ ->
        false )


let import_module module_name : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* module_, opt_body_procname = lookup_module module_name in
  let* () =
    option_iter opt_body_procname ~f:(fun proc_name ->
        python_call proc_name [("globals", module_)] |> ignore )
  in
  ret module_


let import_module_from_package package ~module_path ~module_name =
  let open DSL.Syntax in
  let* module_ = Dict.get_str_key package module_name in
  let* already_imported = is_global module_ in
  if already_imported then ret ()
  else
    let* imported = import_module module_path in
    Dict.set_str_key package module_name imported


let rec import_chain parents ?root_parent ?path chain : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  match (parents, chain, root_parent) with
  | _ :: _, [], Some root_parent ->
      ret root_parent
  | parent :: _, name :: chain, _ ->
      let path =
        Option.value_map path ~default:name ~f:(fun path -> F.asprintf "%s::%s" path name)
      in
      let* () = import_module_from_package parent ~module_path:path ~module_name:name in
      let* module_ = Dict.get_str_key parent name in
      let root_parent = Option.value root_parent ~default:module_ in
      import_chain (module_ :: parents) ~path ~root_parent chain
  | _, _, _ ->
      L.die InternalError "import_chain should never be called on an empty parents list"


let import_from name module_ () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* opt_is_package = is_package module_ in
  let* name = as_constant_string_exn name in
  let* () =
    option_iter opt_is_package ~f:(fun package_name ->
        let module_path = F.asprintf "%s::%s" package_name name in
        import_module_from_package module_ ~module_path ~module_name:name )
  in
  let* res = Dict.get_str_key module_ name in
  assign_ret res


let is_tuple aval : bool DSL.model_monad =
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:false aval in
  let res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} ->
        Typ.Name.equal tuple_tname type_name
    | _ ->
        false
  in
  ret res


let split_module_path path =
  let rec loop acc = function
    | [] ->
        L.die InternalError "split_module_path: unexpected case"
    | [last] ->
        let pos = last + 2 in
        String.sub path ~pos ~len:(String.length path - pos) :: acc |> List.rev
    | pos :: (next_pos :: _ as rest) ->
        let pos = pos + 2 in
        let acc = String.sub path ~pos ~len:(next_pos - pos) :: acc in
        loop acc rest
  in
  let positions = String.substr_index_all path ~may_overlap:false ~pattern:"::" in
  loop [] (-2 :: positions)


let import_name globals name fromlist _level () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* module_name = as_constant_string_exn name in
  let* fromlist_is_tuple = is_tuple fromlist in
  let names = split_module_path module_name in
  if
    fromlist_is_tuple
    (* this is a from ... import ... [as ...] *)
    || List.length names <= 1
    (* this is a import <simple_name> [as ...] *)
  then
    let* module_ = import_module module_name in
    assign_ret module_
  else
    (* this is a import <package> [as ...] *)
    let* first_parent = import_chain [globals] names in
    assign_ret first_parent


let list_append list arg () : unit DSL.model_monad = Tuple.append list arg

let load_fast name locals () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* value = Dict.get_exn locals name in
  assign_ret value


let tag_if_builtin name aval : unit DSL.model_monad =
  let open DSL.Syntax in
  let opt_builtin : PythonClassName.builtin_closure option =
    match name with
    | "str" ->
        Some StrFun
    | "int" ->
        Some IntFun
    | "type" ->
        Some TypeFun
    | "dict" ->
        Some DictFun
    | _ ->
        None
  in
  match opt_builtin with
  | Some builtin ->
      and_dynamic_type_is aval
        (Typ.mk_struct (PythonClass (PythonClassName.BuiltinClosure builtin)))
  | _ ->
      ret ()


let load_global name globals () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* name = as_constant_string_exn name in
  let* value = Dict.get_str_key ~propagate_static_type:true globals name in
  let* () = tag_if_builtin name value in
  let* () = initialize_if_class_companion value in
  assign_ret value


let load_name name locals _globals () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* value = Dict.get_exn locals name in
  (* TODO: decide what we do if the binding is missing in locals *)
  assign_ret value


let is_dynamic_type_maybe_string addr : bool DSL.model_monad =
  let open DSL.Syntax in
  let* val_dynamic_type_data = get_dynamic_type ~ask_specialization:true addr in
  match val_dynamic_type_data with
  | None | Some {Formula.typ= {desc= Tstruct (PythonClass (Builtin PyString))}} ->
      ret true
  | _ ->
      ret false


let build_map (args : DSL.aval list) () : unit DSL.model_monad =
  let open DSL.Syntax in
  if List.length args mod 2 <> 0 then
    L.die InternalError "py_build_map expects even number of arguments" ;
  let key_val_pairs = List.chunks_of args ~length:2 in
  let const_strings_only = ref true in
  let* keys, vals =
    list_fold key_val_pairs ~init:([], []) ~f:(fun (keys, values) key_val_list ->
        match key_val_list with
        | [key; value] -> (
            let* key_str = as_constant_string key in
            match key_str with
            | None ->
                let* is_maybe_string = is_dynamic_type_maybe_string key in
                if is_maybe_string then const_strings_only := false ;
                let* () = remove_allocation_attr_transitively [key; value] in
                (keys, values) |> ret
            | Some key_str ->
                (key_str :: keys, value :: values) |> ret )
        | _ ->
            L.die InternalError "py_build_map expects even number of arguments" )
  in
  let* dict = Dict.make keys vals ~const_strings_only:!const_strings_only in
  assign_ret dict


let make_function closure _default_values _default_values_kw _annotations _cells_for_closure () :
    unit DSL.model_monad =
  let open DSL.Syntax in
  assign_ret closure


let make_int arg () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = Integer.make arg in
  assign_ret res


let binary_add arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = fresh () in
  (* lists and tuples share the same type for now *)
  let* arg1_is_tuple = is_tuple arg1 in
  let* () = if arg1_is_tuple then remove_allocation_attr_transitively [arg1] else ret () in
  let* arg2_is_tuple = is_tuple arg2 in
  let* () = if arg2_is_tuple then remove_allocation_attr_transitively [arg2] else ret () in
  assign_ret res


let bool arg () : unit DSL.model_monad =
  let open DSL.Syntax in
  must_be_awaited arg
  @@>
  let* is_allocated = is_allocated arg in
  let* res =
    if is_allocated then Bool.make true
    else
      dynamic_dispatch arg
        ~cases:[(bool_tname, fun () -> ret arg); (none_tname, fun () -> Bool.make false)]
        ~default:(fun () -> Bool.make_random ())
  in
  assign_ret res


let bool_false () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = Bool.make false in
  assign_ret res


let bool_true () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = Bool.make true in
  assign_ret res


let load_assertion_error () : unit DSL.model_monad =
  let open DSL.Syntax in
  report_assert_error


let make_string arg () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = make_str_internal arg in
  assign_ret res


let make_none () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* none = constructor ~deref:false none_tname [] in
  assign_ret none


let nullify_locals locals names () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* zero = null in
  list_iter names ~f:(fun name -> Dict.set locals name zero)


let store_fast name locals value () : unit DSL.model_monad = Dict.set locals name value

let store_global name globals value () : unit DSL.model_monad = Dict.set globals name value

let store_name name locals _globals value () : unit DSL.model_monad = Dict.set locals name value

let store_subscript dict key value () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* key_str = as_constant_string key in
  match key_str with
  | None ->
      let* is_maybe_string = is_dynamic_type_maybe_string key in
      if is_maybe_string then
        remove_allocation_attr_transitively [value] @@> remove_dict_contain_const_keys dict
      else remove_allocation_attr_transitively [value]
  | Some key ->
      Dict.set_str_key dict key value @@> ret ()


let subscript seq idx () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res =
    dynamic_dispatch seq
      ~cases:[(tuple_tname, fun () -> Tuple.get seq idx); (dict_tname, fun () -> Dict.get seq idx)]
        (* TODO: other sequence types *)
      ~default:(fun () -> fresh ())
  in
  assign_ret res


let unknown ~deep_release ?(must_all_be_awaited = false) args () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = fresh () in
  let* () = if deep_release then remove_allocation_attr_transitively args else ret () in
  let* () = if must_all_be_awaited then list_iter ~f:must_be_awaited args else ret () in
  assign_ret res


let yield value () : unit DSL.model_monad =
  let open DSL.Syntax in
  remove_allocation_attr_transitively [value]


let compare_eq arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res =
    let* arg1_is_unawaited_awaitable = is_unawaited_awaitable arg1 in
    let* arg2_is_unawaited_awaitable = is_unawaited_awaitable arg2 in
    if not (phys_equal arg1_is_unawaited_awaitable arg2_is_unawaited_awaitable) then Bool.make false
    else
      let* arg1_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg1 in
      let* arg2_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg2 in
      match (arg1_dynamic_type_data, arg2_dynamic_type_data) with
      | ( Some {Formula.typ= {desc= Tstruct arg1_type_name}}
        , Some {Formula.typ= {desc= Tstruct arg2_type_name}} )
        when Typ.Name.Python.is_singleton arg1_type_name
             || Typ.Name.Python.is_singleton arg2_type_name ->
          Bool.make (Typ.Name.equal arg1_type_name arg2_type_name)
      | _ ->
          L.d_printfln "py_compare_eq: at least one unknown dynamic type: unknown result" ;
          Bool.make_random ()
  in
  assign_ret res


let compare_in arg1 arg2 () : unit DSL.model_monad =
  let open DSL.Syntax in
  must_be_awaited arg1 @@> must_be_awaited arg2
  @@>
  let* arg1_str = as_constant_string arg1 in
  match arg1_str with
  | Some s -> (
      let* arg2_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg2 in
      match arg2_dynamic_type_data with
      | Some {Formula.typ= {desc= Tstruct (PythonClass (Builtin PyDict))}} ->
          let* dict_contains_key = Dict.contains_str_key arg2 s in
          let* b_res =
            match dict_contains_key with None -> Bool.make_random () | Some res -> Bool.make res
          in
          assign_ret b_res
      | _ ->
          let* res = Bool.make_random () in
          assign_ret res )
  | None ->
      let* res = Bool.make_random () in
      assign_ret res


let builtins_matcher builtin args =
  let expect_1_arg () =
    match args with
    | [arg] ->
        arg
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while 1 was expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_2_args () =
    match args with
    | [arg1; arg2] ->
        (arg1, arg2)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while 2 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_3_args () =
    match args with
    | [arg1; arg2; arg3] ->
        (arg1, arg2, arg3)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while 3 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_4_args () =
    match args with
    | [arg1; arg2; arg3; arg4] ->
        (arg1, arg2, arg3, arg4)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while 4 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_5_args () =
    match args with
    | [arg1; arg2; arg3; arg4; arg5] ->
        (arg1, arg2, arg3, arg4, arg5)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while 5 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_at_least_1_arg () =
    match args with
    | arg1 :: args ->
        (arg1, args)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while at least 1 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_at_least_2_args () =
    match args with
    | arg1 :: arg2 :: args ->
        (arg1, arg2, args)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while at least 2 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  let expect_at_least_3_args () =
    match args with
    | arg1 :: arg2 :: arg3 :: args ->
        (arg1, arg2, arg3, args)
    | _ ->
        L.die InternalError "builtin %s was given %d arguments while at least 3 were expected"
          (PythonProcname.show_builtin builtin)
          (List.length args)
  in
  match (builtin : PythonProcname.builtin) with
  | AsyncGenValueWrapperNew | AttributesOfMatchClass ->
      (unknown ~deep_release:false) args
  | BinaryAdd ->
      let arg1, arg2 = expect_2_args () in
      binary_add arg1 arg2
  | BinaryAnd
  | BinaryFloorDivide
  | BinaryLshift
  | BinaryMatrixMultiply
  | BinaryModulo
  | BinaryMultiply
  | BinaryOr
  | BinaryPower
  | BinaryRshift
  | BinarySlice
  | BinarySubstract
  | BinaryTrueDivide
  | BinaryXor ->
      (unknown ~deep_release:false) args
  | BoolFalse ->
      bool_false
  | BoolOfMatchClass ->
      (unknown ~deep_release:false) args
  | Bool ->
      bool (expect_1_arg ())
  | BoolTrue ->
      bool_true
  | BuildClass ->
      let arg1, arg2, args = expect_at_least_2_args () in
      build_class arg1 arg2 args
  | BuildFrozenSet ->
      (unknown ~deep_release:true) args
  | BuildList ->
      build_tuple args
  | BuildMap ->
      build_map args
  | BuildSet | BuildSlice ->
      (unknown ~deep_release:true) args
  | BuildString ->
      (unknown ~deep_release:false) args
  | BuildTuple ->
      build_tuple args
  | BuildUnpackList | BuildUnpackMap | BuildUnpackSet ->
      (unknown ~deep_release:true) args
  | BuildUnpackTuple ->
      build_tuple args
  | Call ->
      let arg1, arg2, args = expect_at_least_2_args () in
      call arg1 arg2 args
  | CallFunctionEx ->
      let arg1, arg2, arg3 = expect_3_args () in
      call_function_ex arg1 arg2 arg3
  | CallMethod ->
      let arg1, arg2, arg3, args = expect_at_least_3_args () in
      call_method arg1 arg2 arg3 args
  | CompareBad ->
      unknown ~deep_release:false ~must_all_be_awaited:true args
  | CompareEq ->
      let arg1, arg2 = expect_2_args () in
      compare_eq arg1 arg2
  | CompareException | CompareGe | CompareGt ->
      unknown ~deep_release:false ~must_all_be_awaited:true args
  | CompareIn ->
      let arg1, arg2 = expect_2_args () in
      compare_in arg1 arg2
  | CompareIs ->
      let arg1, arg2 = expect_2_args () in
      compare_eq arg1 arg2
  | CompareIsNot | CompareLe | CompareLt | CompareNeq | CompareNotIn | CopyFreeVars ->
      unknown ~deep_release:false ~must_all_be_awaited:true args
  | DeleteAttr | DeleteDeref | DeleteFast | DeleteGlobal | DeleteName | DeleteSubscr | DictMerge ->
      unknown ~deep_release:true args
  | DictSetItem ->
      let arg1, arg2, arg3 = expect_3_args () in
      dict_set_item arg1 arg2 arg3
  | DictUpdate ->
      unknown ~deep_release:true args
  | Format | FormatFnAscii | FormatFnRepr | FormatFnStr | GenStartAsyncGenerator ->
      unknown ~deep_release:false args
  | GenStartCoroutine ->
      gen_start_coroutine
  | GenStartGenerator ->
      unknown ~deep_release:false args
  | GetAiter ->
      unknown ~deep_release:true args
  | GetAttr ->
      let arg1, arg2 = expect_2_args () in
      get_attr arg1 arg2
  | GetAwaitable ->
      let arg1 = expect_1_arg () in
      get_awaitable arg1
  | GetIter | GetLen | GetPreviousException ->
      unknown ~deep_release:false ~must_all_be_awaited:true args
  | GetYieldFromIter | HasNextIter ->
      unknown ~deep_release:true args
  | ImportFrom ->
      let arg1, arg2 = expect_2_args () in
      import_from arg1 arg2
  | ImportName ->
      let arg1, arg2, arg3, arg4 = expect_4_args () in
      import_name arg1 arg2 arg3 arg4
  | ImportStar ->
      unknown ~deep_release:true args
  | InplaceAdd
  | InplaceAnd
  | InplaceFloorDivide
  | InplaceLshift
  | InplaceMatrixMultiply
  | InplaceModulo
  | InplaceMultiply
  | InplaceOr
  | InplacePower
  | InplaceRshift
  | InplaceSubstract
  | InplaceTrueDivide
  | InplaceXor
  | InvalidUnicode ->
      unknown ~deep_release:false args
  | IterData ->
      unknown ~deep_release:true args
  | ListAppend ->
      let arg1, arg2 = expect_2_args () in
      list_append arg1 arg2
  | ListExtend | ListToTuple ->
      unknown ~deep_release:true args
  | LoadAssertionError ->
      load_assertion_error
  | LoadClassDeref | LoadClosure | LoadDeref ->
      unknown ~deep_release:false args
  | LoadFast | LoadFastAndClear ->
      let arg1, arg2 = expect_2_args () in
      load_fast arg1 arg2
  | LoadFastCheck | LoadFromDictOrDeref ->
      unknown ~deep_release:false args
  | LoadGlobal ->
      let arg1, arg2 = expect_2_args () in
      load_global arg1 arg2
  | LoadName ->
      let arg1, arg2, arg3 = expect_3_args () in
      load_name arg1 arg2 arg3
  | LoadLocals | LoadSuperAttr | MakeBytes | MakeComplex | MakeCell | MakeFloat ->
      unknown ~deep_release:false args
  | MakeFunction ->
      let arg1, arg2, arg3, arg4, arg5 = expect_5_args () in
      make_function arg1 arg2 arg3 arg4 arg5
  | MakeInt ->
      let arg = expect_1_arg () in
      make_int arg
  | MakeNone ->
      make_none
  | MakeString ->
      let arg = expect_1_arg () in
      make_string arg
  | MatchClass | MatchSequence | NextIter ->
      unknown ~deep_release:false args
  | NullifyLocals ->
      let arg1, args = expect_at_least_1_arg () in
      nullify_locals arg1 args
  | PrepReraiseStar ->
      unknown ~deep_release:false args
  | SetAdd ->
      unknown ~deep_release:true args
  | SetAttr ->
      let arg1, arg2, arg3 = expect_3_args () in
      store_subscript arg1 arg2 arg3
  | SetUpdate ->
      unknown ~deep_release:true args
  | SetupAnnotations ->
      unknown ~deep_release:false args
  | StoreDeref ->
      unknown ~deep_release:true args
  | StoreFast ->
      let arg1, arg2, arg3 = expect_3_args () in
      store_fast arg1 arg2 arg3
  | StoreGlobal ->
      let arg1, arg2, arg3 = expect_3_args () in
      store_global arg1 arg2 arg3
  | StoreName ->
      let arg1, arg2, arg3, arg4 = expect_4_args () in
      store_name arg1 arg2 arg3 arg4
  | StoreSlice ->
      unknown ~deep_release:true args
  | StoreSubscript ->
      let arg1, arg2, arg3 = expect_3_args () in
      store_subscript arg1 arg2 arg3
  | Subscript ->
      let arg1, arg2 = expect_2_args () in
      subscript arg1 arg2
  | SetFunctionTypeParams
  | TypevarWithBound
  | TypevarWithConstraints
  | UnaryInvert
  | UnaryNegative
  | UnaryNot
  | UnaryPos
  | UnaryPositive
  | UnpackEx ->
      unknown ~deep_release:false args
  | Yield ->
      let arg = expect_1_arg () in
      yield arg
  | YieldFrom ->
      unknown ~deep_release:true args
