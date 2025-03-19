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
open PulseModelsImport
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

let build_tuple args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* tuple = Tuple.make args in
  assign_ret tuple


let build_class closure _name _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* class_ = Dict.make [] [] ~const_strings_only:true in
  let gen_closure_args _ = ret [class_] in
  let* _ = apply_python_closure closure gen_closure_args in
  assign_ret class_


let call_closure_dsl ~closure ~arg_names:_ ~args : DSL.aval DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let gen_closure_args opt_proc_attrs =
    let python_args =
      match opt_proc_attrs with
      | Some {ProcAttributes.python_args}
        when Int.equal (List.length python_args) (List.length args) ->
          python_args
      | Some {ProcAttributes.python_args} ->
          L.d_printfln "[ocaml model] %d argument required but %d were given"
            (List.length python_args) (List.length args) ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
      | None ->
          L.d_printfln "[ocaml model] Failed to load attributes" ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
    in
    let* locals = Dict.make python_args args ~const_strings_only:true in
    ret [locals]
  in
  apply_python_closure closure gen_closure_args


let call_dsl ~callable ~arg_names ~args =
  let open DSL.Syntax in
  let* opt_static_type = get_static_type callable in
  match opt_static_type with
  | Some (Typ.PythonClass (ClassCompanion {module_name; class_name})) ->
      let instance_type = Typ.PythonClass (ClassInstance {module_name; class_name}) in
      L.d_printfln ~color:Orange "calling a constructor for type %a" Typ.Name.pp instance_type ;
      let* new_instance = constructor ~deref:false instance_type [] in
      let* init_closure = Dict.get_str_key ~propagate_static_type:true callable "__init__" in
      let args = new_instance :: args in
      let* _ = call_closure_dsl ~closure:init_closure ~arg_names:[] ~args in
      ret new_instance
  | _ ->
      L.d_printfln ~color:Orange "calling closure %a" AbstractValue.pp (fst callable) ;
      call_closure_dsl ~closure:callable ~arg_names ~args


let await_awaitable arg : unit DSL.model_monad =
  fst arg |> AddressAttributes.await_awaitable |> DSL.Syntax.exec_command


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


module LibModel = struct
  let match_pattern ~pattern ~module_name ~name =
    Option.exists pattern ~f:(fun regexp ->
        let str = Printf.sprintf "%s::%s" module_name name in
        Str.string_match regexp str 0 )


  let is_release ~module_name ~name =
    match_pattern ~pattern:Config.pulse_model_release_pattern ~module_name ~name


  let is_deep_release ~module_name ~name =
    match_pattern ~pattern:Config.pulse_model_deep_release_pattern ~module_name ~name


  let gen_awaitable _args =
    let open DSL.Syntax in
    let* res = fresh () in
    let* () = allocation Attribute.Awaitable res in
    ret (Some res)


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
let modelled_python_call model args : DSL.aval option DSL.model_monad =
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


let try_catch_lib_model type_name args =
  let open DSL.Syntax in
  match Typ.Name.Python.split_module_attr type_name with
  | Some (module_name, name) ->
      modelled_python_call (PyLib {module_name; name}) args
  | _ ->
      ret None


let try_catch_lib_model_using_static_type ~default closure args =
  let open DSL.Syntax in
  let* opt_static_type = get_static_type closure in
  match opt_static_type with
  | Some (Typ.PythonClass (ModuleAttribute {module_name; attr_name= name}) as type_name) -> (
      let* opt_special_call = modelled_python_call (PyLib {module_name; name}) args in
      match opt_special_call with
      | None ->
          default ()
      | Some res ->
          L.d_printfln "catching reserved lib call using static type %a" Typ.Name.pp type_name ;
          ret res )
  | _ ->
      default ()


let call callable arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true callable in
  let* res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (BuiltinClosure builtin))}} -> (
        let* opt_special_call = modelled_python_call (PyBuiltin builtin) args in
        match opt_special_call with
        | None ->
            L.die InternalError "builtin %s was not successfully recognized"
              (PythonClassName.to_string (BuiltinClosure builtin))
        | Some res ->
            L.d_printfln "catching reserved builtin call %s"
              (PythonClassName.to_string (BuiltinClosure builtin)) ;
            ret res )
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
        let* opt_catched_lib_model_res = try_catch_lib_model type_name args in
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


let call_function_ex closure tuple dict : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true closure in
  let args = [tuple; dict] in
  let* res =
    match opt_dynamic_type_data with
    | None ->
        try_catch_lib_model_using_static_type ~default:fresh closure args
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
        let* opt_catched_lib_model_res = try_catch_lib_model type_name args in
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


let gen_start_coroutine : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let is_module_captured module_name =
  let function_name = "__module_body__" in
  let class_name = PythonClassName.Filename module_name in
  let proc_name = Procname.make_python ~class_name:(Some class_name) ~function_name in
  IRAttributes.load proc_name |> Option.map ~f:(fun _ -> proc_name)


let lookup_module module_name =
  let open DSL.Syntax in
  match is_module_captured module_name with
  | Some proc_name ->
      let* module_ = PyModule.make module_name in
      ret (module_, Some proc_name)
  | None -> (
      (* it is not a capture module name *)
      let module_name_init = module_name ^ "::__init__" in
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


let initialize_class_companion ~module_name ~class_name class_companion_object =
  let open DSL.Syntax in
  let* module_, _ = lookup_module module_name in
  let module_tname = PythonClassName.Filename module_name in
  let class_initializer =
    Procname.make_python ~class_name:(Some module_tname) ~function_name:class_name
  in
  python_call class_initializer [("globals", module_); ("locals", class_companion_object)] |> ignore


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
      L.d_printfln ~color:Orange "initializing companion class %s" class_name ;
      initialize_class_companion ~module_name ~class_name value )


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
      | Some (module_name, class_name) ->
          let* class_companion_object = Dict.make ~const_strings_only:true [] [] in
          let* () = initialize_class_companion ~module_name ~class_name class_companion_object in
          L.d_printfln ~color:Orange
            "checking if the attribute %s belongs to the companion class %s.%s (in value %a)...@;"
            attr module_name class_name AbstractValue.pp (fst class_companion_object) ;
          let* key_in_companion = Dict.contains_str_key class_companion_object attr in
          if ThreeValuedLogic.is_true key_in_companion then
            Dict.get_str_key ~propagate_static_type:true class_companion_object attr
          else
            (* for now, we assume in such a situation that the attribute is in the instance. It would
               be great to 'record' such an assumption to make sure the caller context agrees *)
            Dict.get_str_key ~propagate_static_type:true obj attr
  in
  let* () = initialize_if_class_companion res in
  ret res


let get_attr obj attr : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = get_attr_dsl obj attr in
  assign_ret res


let call_method name obj arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true obj in
  let* res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (Globals module_name))}} -> (
        (* since module types are final, static type will save us most of the time *)
        let* str_name = as_constant_string_exn name in
        let* opt_special_call = modelled_python_call (PyLib {module_name; name= str_name}) args in
        match opt_special_call with
        | None ->
            L.d_printfln "calling method %s on module object %s" str_name module_name ;
            let* callable = Dict.get_exn obj name in
            call_dsl ~callable ~arg_names ~args
        | Some res ->
            L.d_printfln "catching special call %s on module object %s" str_name module_name ;
            ret res )
    | _ ->
        let* callable = get_attr_dsl obj name in
        (* TODO: for OO method, gives self argument *)
        call_dsl ~callable ~arg_names ~args:(obj :: args)
  in
  assign_ret res


let get_awaitable arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* () = await_awaitable arg in
  assign_ret arg


let dict_set_item _dict _key value : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  (* TODO: when the dict is a constant, we could just update it *)
  await_awaitable value


let is_package aval : string option DSL.model_monad =
  let open DSL.Syntax in
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:false aval in
  ret
    ( match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct (PythonClass (Filename name))}}
      when String.is_suffix name ~suffix:"::__init__" ->
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


let import_from name module_ : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
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


let import_name globals name fromlist _level : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
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


let list_append list arg : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Tuple.append list arg


let load_fast name locals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
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


let load_global name globals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* name = as_constant_string_exn name in
  let* value = Dict.get_str_key ~propagate_static_type:true globals name in
  let* () = tag_if_builtin name value in
  let* () = initialize_if_class_companion value in
  assign_ret value


let load_name name locals _globals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = Dict.get_exn locals name in
  (* TODO: decide what we do if the binding is missing in locals *)
  assign_ret value


let get_key_as_str i keys : string option DSL.model_monad =
  let open DSL.Syntax in
  let* key_addr = Tuple.get_idx_int keys i in
  as_constant_string key_addr


let is_dynamic_type_maybe_string addr : bool DSL.model_monad =
  let open DSL.Syntax in
  let* val_dynamic_type_data = get_dynamic_type ~ask_specialization:true addr in
  match val_dynamic_type_data with
  | None | Some {Formula.typ= {desc= Tstruct (PythonClass (Builtin PyString))}} ->
      ret true
  | _ ->
      ret false


let get_bindings (keys : DSL.aval) (vals : DSL.aval list) :
    (string list * DSL.aval list * bool) DSL.model_monad =
  let open DSL.Syntax in
  let const_strings_only = ref true in
  let* ks, vs, _ =
    list_fold vals ~init:([], [], 0) ~f:(fun acc v ->
        let acc_keys, acc_vals, c = acc in
        let* key = get_key_as_str c keys in
        match key with
        (* For now we ignore keys that are not const strings *)
        | None ->
            let* is_maybe_string = is_dynamic_type_maybe_string keys in
            if is_maybe_string then const_strings_only := false ;
            (acc_keys, acc_vals, c + 1) |> ret
        | Some key ->
            (key :: acc_keys, v :: acc_vals, c + 1) |> ret )
  in
  ret (List.rev ks, List.rev vs, !const_strings_only)


let make_const_key_map (keys : DSL.aval) (values : DSL.aval list) : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* keys, vals, const_strings_only = get_bindings keys values in
  let* dict = Dict.make keys vals ~const_strings_only in
  assign_ret dict


let make_function closure _default_values _default_values_kw _annotations _cells_for_closure : model
    =
  let open DSL.Syntax in
  start_model @@ fun () -> assign_ret closure


let make_int arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = Integer.make arg in
  assign_ret res


let binary_add arg1 arg2 : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = fresh () in
  (* lists and tuples share the same type for now *)
  let* arg1_is_tuple = is_tuple arg1 in
  let* () = if arg1_is_tuple then remove_allocation_attr_transitively [arg1] else ret () in
  let* arg2_is_tuple = is_tuple arg2 in
  let* () = if arg2_is_tuple then remove_allocation_attr_transitively [arg2] else ret () in
  assign_ret res


let bool arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* is_awaitable = is_allocated arg in
  let* res =
    if is_awaitable then Bool.make true
    else
      dynamic_dispatch arg
        ~cases:[(bool_tname, fun () -> ret arg); (none_tname, fun () -> Bool.make false)]
        ~default:(fun () -> Bool.make_random ())
  in
  assign_ret res


let bool_false : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = Bool.make false in
  assign_ret res


let bool_true : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = Bool.make true in
  assign_ret res


let make_string arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = make_str_internal arg in
  assign_ret res


let make_none : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* none = constructor ~deref:false none_tname [] in
  assign_ret none


let nullify_locals locals names : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* zero = null in
  list_iter names ~f:(fun name -> Dict.set locals name zero)


let store_fast name locals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let store_global name globals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set globals name value


let store_name name locals _globals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let store_subscript dict key value =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* key_str = as_constant_string key in
  match key_str with
  | None ->
      let* is_maybe_string = is_dynamic_type_maybe_string key in
      if is_maybe_string then
        remove_allocation_attr_transitively [value] @@> remove_dict_contain_const_keys dict
      else remove_allocation_attr_transitively [value]
  | Some key ->
      Dict.set_str_key dict key value @@> ret ()


let subscript seq idx : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res =
    dynamic_dispatch seq
      ~cases:[(tuple_tname, fun () -> Tuple.get seq idx); (dict_tname, fun () -> Dict.get seq idx)]
        (* TODO: other sequence types *)
      ~default:(fun () -> fresh ())
  in
  assign_ret res


let yield_from _ _ : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let unknown ~deep_release args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = fresh () in
  let* () = if deep_release then remove_allocation_attr_transitively args else ret () in
  assign_ret res


let yield value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> remove_allocation_attr_transitively [value]


let die_if_other_builtin (_, proc_name) _ =
  if
    Language.curr_language_is Python
    && Procname.get_class_name proc_name |> Option.exists ~f:(String.equal "$builtins")
  then L.die InternalError "unknown builtin %a" Procname.pp proc_name ;
  false


let compare_eq arg1 arg2 : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* arg1_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg1 in
  let* arg2_dynamic_type_data = get_dynamic_type ~ask_specialization:true arg2 in
  let* res =
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


let compare_in arg1 arg2 : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
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


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  [ -"$builtins" &:: "py_async_gen_value_wrapper_new" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_attributes_of_match_class" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_add" <>$ arg $+ arg $--> binary_add
  ; -"$builtins" &:: "py_binary_and" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_floor_divide" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_lshift" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_matrix_multiply" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_modulo" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_multiply" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_or" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_power" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_rshift" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_slice" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_substract" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_true_divide" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_binary_xor" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_bool_false" <>--> bool_false
  ; -"$builtins" &:: "py_bool_of_match_class" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_bool" <>$ arg $--> bool
  ; -"$builtins" &:: "py_bool_true" <>--> bool_true
  ; -"$builtins" &:: "py_build_class" <>$ arg $+ arg $+++$--> build_class
  ; -"$builtins" &:: "py_build_const_key_map" <>$ arg $+++$--> make_const_key_map
  ; -"$builtins" &:: "py_build_frozen_set" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_list" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_build_map" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_set" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_slice" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_string" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_build_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_build_unpack_list" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_unpack_map" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_unpack_set" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_build_unpack_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_call" <>$ arg $+ arg $+++$--> call
  ; -"$builtins" &:: "py_call_function_ex" <>$ arg $+ arg $+ arg $--> call_function_ex
  ; -"$builtins" &:: "py_call_method" <>$ arg $+ arg $+ arg $+++$--> call_method
  ; -"$builtins" &:: "py_compare_bad" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_eq" <>$ arg $+ arg $--> compare_eq
  ; -"$builtins" &:: "py_compare_exception" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_ge" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_gt" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_in" <>$ arg $+ arg $--> compare_in
  ; -"$builtins" &:: "py_compare_is" <>$ arg $+ arg $--> compare_eq
  ; -"$builtins" &:: "py_compare_is_not" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_le" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_lt" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_neq" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_compare_not_in" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_copy_free_vars" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_delete_attr" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_delete_deref" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_delete_fast" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_delete_global" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_delete_name" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_delete_subscr" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_dict_merge" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_dict_set_item" <>$ arg $+ arg $+ arg $--> dict_set_item
  ; -"$builtins" &:: "py_dict_update" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_format" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_format_fn_ascii" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_format_fn_repr" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_format_fn_str" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_gen_start_async_generator" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_gen_start_coroutine" <>--> gen_start_coroutine
  ; -"$builtins" &:: "py_gen_start_generator" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_get_aiter" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_get_attr" <>$ arg $+ arg $--> get_attr
  ; -"$builtins" &:: "py_get_awaitable" <>$ arg $--> get_awaitable
  ; -"$builtins" &:: "py_get_iter" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_get_len" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_get_previous_exception" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_get_yield_from_iter" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_has_next_iter" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_import_from" <>$ arg $+ arg $--> import_from
  ; -"$builtins" &:: "py_import_name" <>$ arg $+ arg $+ arg $+ arg $--> import_name
  ; -"$builtins" &:: "py_import_star" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_inplace_add" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_and" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_floor_divide" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_lshift" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_matrix_multiply" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_modulo" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_multiply" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_or" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_power" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_rshift" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_substract" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_true_divide" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_inplace_xor" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_invalid_unicode" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_iter_data" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_list_append" <>$ arg $+ arg $--> list_append
  ; -"$builtins" &:: "py_list_extend" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_list_to_tuple" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_load_assertion_error" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_class_deref" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_closure" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_deref" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_fast" <>$ arg $+ arg $--> load_fast
  ; -"$builtins" &:: "py_load_fast_and_clear" <>$ arg $+ arg $--> load_fast
  ; -"$builtins" &:: "py_load_fast_check" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_from_dict_or_deref" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_global" <>$ arg $+ arg $--> load_global
  ; -"$builtins" &:: "py_load_name" <>$ arg $+ arg $+ arg $--> load_name
  ; -"$builtins" &:: "py_load_locals" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_load_super_attr" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_make_bytes" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_make_complex" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_make_cell" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_make_float" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_make_function" <>$ arg $+ arg $+ arg $+ arg $+ arg $--> make_function
  ; -"$builtins" &:: "py_make_int" <>$ arg $--> make_int
  ; -"$builtins" &:: "py_make_none" <>--> make_none
  ; -"$builtins" &:: "py_make_string" <>$ arg $--> make_string
  ; -"$builtins" &:: "py_match_class" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_match_sequence" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_next_iter" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_nullify_locals" <>$ arg $+++$--> nullify_locals
  ; -"$builtins" &:: "py_prep_reraise_star" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_set_add" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_set_attr" <>$ arg $+ arg $+ arg $--> store_subscript
  ; -"$builtins" &:: "py_set_update" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_setup_annotations" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_store_deref" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_store_fast" <>$ arg $+ arg $+ arg $--> store_fast
  ; -"$builtins" &:: "py_store_global" <>$ arg $+ arg $+ arg $--> store_global
  ; -"$builtins" &:: "py_store_name" <>$ arg $+ arg $+ arg $+ arg $--> store_name
  ; -"$builtins" &:: "py_store_slice" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_store_subscript" <>$ arg $+ arg $+ arg $--> store_subscript
  ; -"$builtins" &:: "py_subscript" <>$ arg $+ arg $--> subscript
  ; -"$builtins" &:: "py_set_function_type_params" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_typevar_with_bound" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_typevar_with_constraints" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unary_invert" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unary_negative" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unary_not" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unary_pos" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unary_positive" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_unpack_ex" &::.*+++> unknown ~deep_release:false
  ; -"$builtins" &:: "py_yield" <>$ arg $--> yield
  ; -"$builtins" &:: "py_yield_from" &::.*+++> unknown ~deep_release:true
  ; -"$builtins" &:: "py_yield_from" <>$ arg $+ arg $--> yield_from
  ; +die_if_other_builtin &::.*+++> unknown ~deep_release:false ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
