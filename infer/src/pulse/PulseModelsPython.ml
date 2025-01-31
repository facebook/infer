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


module Dict = struct
  let make keys args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    if not (Int.equal (List.length args) (List.length keys)) then
      L.die InternalError "Dict.make expects two list of same length@\n" ;
    let bindings = List.zip_exn keys args in
    let* dict = constructor ~deref:false dict_tname bindings in
    ret dict


  let propagate_static_type_on_load dict key load_res : unit DSL.model_monad =
    let open DSL.Syntax in
    let rec propagate_field_type tname key =
      let field = Fieldname.make tname key in
      let* opt_info = tenv_resolve_field_info tname field in
      match opt_info with
      | Some {Struct.typ= field_typ} -> (
        match Typ.name (Typ.strip_ptr field_typ) with
        | Some (PythonClass (ModuleAttribute {module_name; attr_name})) ->
            let tname = Typ.PythonClass (Globals module_name) in
            let* tname_is_defined = tenv_type_is_defined tname in
            if tname_is_defined then propagate_field_type tname attr_name
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
    option_iter opt_static_type ~f:(fun tname -> propagate_field_type tname key)


  let get_str_key ?(propagate_static_type = false) dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make dict_tname key in
    let* load_res = load_access ~deref:false dict (FieldAccess field) in
    let* () =
      if propagate_static_type then propagate_static_type_on_load dict key load_res else ret ()
    in
    (* note: we do not try static type propagation here *)
    ret load_res


  (* beware: key is expected to be a constant string! *)
  let get dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* key = as_constant_string_exn key in
    let* load_res = get_str_key ~propagate_static_type:true dict key in
    ret load_res


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


  let get tuple idx : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* opt_int = as_constant_int idx in
    match opt_int with
    | None ->
        fresh ()
    | Some i ->
        let field = Fieldname.make tuple_tname (str_field_of_int i) in
        load_access ~deref:false tuple (FieldAccess field)


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
  let* class_ = Dict.make [] [] in
  let gen_closure_args _ = ret [class_] in
  let* _ = apply_python_closure closure gen_closure_args in
  assign_ret class_


let call_dsl ~closure ~arg_names:_ ~args : DSL.aval DSL.model_monad =
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
    let* locals = Dict.make python_args args in
    ret [locals]
  in
  apply_python_closure closure gen_closure_args


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


let make_int_internal arg : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* opt_int = as_constant_int arg in
  match opt_int with
  | None ->
      constructor ~deref:false int_tname []
  | Some i ->
      let* res = int i in
      let* () = and_dynamic_type_is res (Typ.mk_struct int_tname) in
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
  | PyBuiltin IntFun, [arg] ->
      let* res = make_int_internal arg in
      ret (Some res)
  | PyBuiltin StrFun, _ ->
      let* res = fresh () in
      ret (Some res)
  | PyBuiltin TypeFun, [arg] ->
      make_type arg
  | PyBuiltin TypeFun, _ | PyBuiltin IntFun, _ ->
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


let call closure arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true closure in
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
            call_dsl ~closure ~arg_names ~args )
    | _ ->
        call_dsl ~closure ~arg_names ~args
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
    | None -> (
        let* opt_static_type = get_static_type closure in
        match opt_static_type with
        | Some (Typ.PythonClass (ModuleAttribute {module_name; attr_name= name}) as type_name) -> (
            let* opt_special_call = modelled_python_call (PyLib {module_name; name}) args in
            match opt_special_call with
            | None ->
                fresh ()
            | Some res ->
                L.d_printfln "catching reserved lib call using static type %a" Typ.Name.pp type_name ;
                ret res )
        | _ ->
            fresh () )
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
            let* closure = Dict.get obj name in
            call_dsl ~closure ~arg_names ~args
        | Some res ->
            L.d_printfln "catching special call %s on module object %s" str_name module_name ;
            ret res )
    | _ ->
        let* closure = Dict.get obj name in
        (* TODO: for OO method, gives self argument *)
        call_dsl ~closure ~arg_names ~args
  in
  assign_ret res


let gen_start_coroutine : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let get_attr obj attr : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* attr = as_constant_string_exn attr in
  (* TODO: look into companion class object if necessary *)
  let* res = Dict.get_str_key ~propagate_static_type:true obj attr in
  assign_ret res


let get_awaitable arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* () = await_awaitable arg in
  assign_ret arg


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
  let* value = Dict.get locals name in
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
  assign_ret value


let load_name name locals _globals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = Dict.get locals name in
  (* TODO: decide what we do if the binding is missing in locals *)
  assign_ret value


let make_dictionary _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  (* TODO: take args into account *)
  let* dict = Dict.make [] [] in
  assign_ret dict


let make_function closure _default_values _default_values_kw _annotations _cells_for_closure : model
    =
  let open DSL.Syntax in
  start_model @@ fun () -> assign_ret closure


let make_int arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = make_int_internal arg in
  assign_ret res


let make_bool bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* bool = int (if bool then 1 else 0) in
  let* () = and_dynamic_type_is bool (Typ.mk_struct bool_tname) in
  ret bool


let make_random_bool () =
  let open DSL.Syntax in
  let* res = fresh () in
  let* () = and_dynamic_type_is res (Typ.mk_struct bool_tname) in
  ret res


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


let store_subscript _dict _key value =
  let open DSL.Syntax in
  start_model @@ fun () -> remove_allocation_attr_transitively [value]


let subscript seq idx : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res =
    dynamic_dispatch seq
      ~cases:[(tuple_tname, fun () -> Tuple.get seq idx)] (* TODO: other sequence types *)
      ~default:(fun () -> fresh ())
  in
  assign_ret res


let yield_from _ _ : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let unknown _ : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = fresh () in
  assign_ret res


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
        make_bool (Typ.Name.equal arg1_type_name arg2_type_name)
    | _ ->
        L.d_printfln "py_compare_eq: at least one unknown dynamic type: unknown result" ;
        make_random_bool ()
  in
  assign_ret res


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  [ -"$builtins" &:: "py_attributes_of_match_class" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_add" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_and" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_floor_divide" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_lshift" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_matrix_multiply" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_modulo" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_multiply" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_or" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_power" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_rshift" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_substract" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_true_divide" &::.*+++> unknown
  ; -"$builtins" &:: "py_binary_xor" &::.*+++> unknown
  ; -"$builtins" &:: "py_bool_false" &::.*+++> unknown
  ; -"$builtins" &:: "py_bool_of_match_class" &::.*+++> unknown
  ; -"$builtins" &:: "py_bool_true" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_class" <>$ arg $+ arg $+++$--> build_class
  ; -"$builtins" &:: "py_build_const_key_map" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_frozen_set" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_list" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_build_map" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_set" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_slice" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_string" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_build_unpack_list" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_unpack_map" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_unpack_set" &::.*+++> unknown
  ; -"$builtins" &:: "py_build_unpack_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_call" <>$ arg $+ arg $+++$--> call
  ; -"$builtins" &:: "py_call_function_ex" <>$ arg $+ arg $+ arg $--> call_function_ex
  ; -"$builtins" &:: "py_call_method" <>$ arg $+ arg $+ arg $+++$--> call_method
  ; -"$builtins" &:: "py_compare_bad" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_eq" <>$ arg $+ arg $--> compare_eq
  ; -"$builtins" &:: "py_compare_exception" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_ge" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_gt" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_in" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_is" <>$ arg $+ arg $--> compare_eq
  ; -"$builtins" &:: "py_compare_is_not" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_le" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_lt" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_neq" &::.*+++> unknown
  ; -"$builtins" &:: "py_compare_not_in" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_attr" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_deref" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_fast" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_global" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_name" &::.*+++> unknown
  ; -"$builtins" &:: "py_delete_subscr" &::.*+++> unknown
  ; -"$builtins" &:: "py_dict_merge" &::.*+++> unknown
  ; -"$builtins" &:: "py_dict_set_item" &::.*+++> unknown
  ; -"$builtins" &:: "py_dict_update" &::.*+++> unknown
  ; -"$builtins" &:: "py_format" &::.*+++> unknown
  ; -"$builtins" &:: "py_format_fn_ascii" &::.*+++> unknown
  ; -"$builtins" &:: "py_format_fn_repr" &::.*+++> unknown
  ; -"$builtins" &:: "py_format_fn_str" &::.*+++> unknown
  ; -"$builtins" &:: "py_gen_start_async_generator" &::.*+++> unknown
  ; -"$builtins" &:: "py_gen_start_coroutine" <>--> gen_start_coroutine
  ; -"$builtins" &:: "py_gen_start_generator" &::.*+++> unknown
  ; -"$builtins" &:: "py_get_aiter" &::.*+++> unknown
  ; -"$builtins" &:: "py_get_attr" <>$ arg $+ arg $--> get_attr
  ; -"$builtins" &:: "py_get_awaitable" <>$ arg $--> get_awaitable
  ; -"$builtins" &:: "py_get_iter" &::.*+++> unknown
  ; -"$builtins" &:: "py_get_len" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_get_previous_exception" &::.*+++> unknown
  ; -"$builtins" &:: "py_get_yield_from_iter" &::.*+++> unknown
  ; -"$builtins" &:: "py_has_next_iter" &::.*+++> unknown
  ; -"$builtins" &:: "py_import_from" <>$ arg $+ arg $--> import_from
  ; -"$builtins" &:: "py_import_name" <>$ arg $+ arg $+ arg $+ arg $--> import_name
  ; -"$builtins" &:: "py_import_star" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_add" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_and" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_floor_divide" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_lshift" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_matrix_multiply" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_modulo" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_multiply" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_or" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_power" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_rshift" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_substract" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_true_divide" &::.*+++> unknown
  ; -"$builtins" &:: "py_inplace_xor" &::.*+++> unknown
  ; -"$builtins" &:: "py_invalid_unicode" &::.*+++> unknown
  ; -"$builtins" &:: "py_iter_data" &::.*+++> unknown
  ; -"$builtins" &:: "py_list_append" <>$ arg $+ arg $--> list_append
  ; -"$builtins" &:: "py_list_extend" &::.*+++> unknown
  ; -"$builtins" &:: "py_list_to_tuple" &::.*+++> unknown
  ; -"$builtins" &:: "py_load_assertion_error" &::.*+++> unknown
  ; -"$builtins" &:: "py_load_class_deref" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_load_closure" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_load_deref" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_load_fast" <>$ arg $+ arg $--> load_fast
  ; -"$builtins" &:: "py_load_global" <>$ arg $+ arg $--> load_global
  ; -"$builtins" &:: "py_load_name" <>$ arg $+ arg $+ arg $--> load_name
  ; -"$builtins" &:: "py_make_bytes" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_make_complex" &::.*+++> unknown
  ; -"$builtins" &:: "py_make_dictionary" &::.*+++> make_dictionary
  ; -"$builtins" &:: "py_make_float" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_make_function" <>$ arg $+ arg $+ arg $+ arg $+ arg $--> make_function
  ; -"$builtins" &:: "py_make_int" <>$ arg $--> make_int
  ; -"$builtins" &:: "py_make_none" <>--> make_none
  ; -"$builtins" &:: "py_make_string" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_match_class" &::.*+++> unknown
  ; -"$builtins" &:: "py_match_sequence" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_next_iter" &::.*+++> unknown
  ; -"$builtins" &:: "py_nullify_locals" <>$ arg $+++$--> nullify_locals
  ; -"$builtins" &:: "py_set_add" &::.*+++> unknown
  ; -"$builtins" &:: "py_set_attr" &::.*+++> unknown
  ; -"$builtins" &:: "py_set_update" &::.*+++> unknown
  ; -"$builtins" &:: "py_setup_annotations" &::.*+++> unknown
  ; -"$builtins" &:: "py_store_deref" &::.*+++> unknown
  ; -"$builtins" &:: "py_store_fast" <>$ arg $+ arg $+ arg $--> store_fast
  ; -"$builtins" &:: "py_store_global" <>$ arg $+ arg $+ arg $--> store_global
  ; -"$builtins" &:: "py_store_name" <>$ arg $+ arg $+ arg $+ arg $--> store_name
  ; -"$builtins" &:: "py_store_subscript" <>$ arg $+ arg $+ arg $--> store_subscript
  ; -"$builtins" &:: "py_subscript" <>$ arg $+ arg $--> subscript
  ; -"$builtins" &:: "py_unary_invert" &::.*+++> unknown
  ; -"$builtins" &:: "py_unary_negative" &::.*+++> unknown
  ; -"$builtins" &:: "py_unary_not" &::.*+++> unknown
  ; -"$builtins" &:: "py_unary_positive" &::.*+++> unknown
  ; -"$builtins" &:: "py_unpack_ex" &::.*+++> unknown
  ; -"$builtins" &:: "py_yield" <>$ arg $--> unknown
  ; -"$builtins" &:: "py_yield_from" &::.*+++> unknown
  ; -"$builtins" &:: "py_yield_from" <>$ arg $+ arg $--> yield_from
  ; +die_if_other_builtin &::.*+++> unknown ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
