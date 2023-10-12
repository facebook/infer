(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module Debug = PyDebug
module Builtin = PyBuiltin
module SMap = PyCommon.SMap
module SSet = PyCommon.SSet

module Const = struct
  (** This module is a small layer above [FFI.Constant]. It turns all "byte" strings into strings,
      and the tuples from array to list.

      TODO: Maybe merge it with FFI.Constant.t *)

  type t =
    | Int of int64
    | Bool of bool
    | Float of float
    | String of string
    | Tuple of t list
    | Code of FFI.Code.t
    | Null
  [@@deriving compare]

  let rec pp fmt = function
    | Int i ->
        Int64.pp fmt i
    | Bool b ->
        Bool.pp fmt b
    | Float f ->
        Float.pp fmt f
    | String s ->
        String.pp fmt s
    | Null ->
        F.pp_print_string fmt "None"
    | Tuple tuple ->
        F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " pp) tuple
    | Code {FFI.Code.co_name; co_freevars; co_cellvars} ->
        let sz = Array.length co_freevars + Array.length co_cellvars in
        let maybe_closure = sz <> 0 in
        F.fprintf fmt "Code(%s, %b)" co_name maybe_closure


  let rec from_const c =
    match (c : FFI.Constant.t) with
    | PYCBool b ->
        Bool b
    | PYCInt i ->
        Int i
    | PYCFloat f ->
        Float f
    | PYCString s ->
        String s
    | PYCBytes b ->
        let s = Bytes.to_string b in
        String s
    | PYCNone ->
        Null
    | PYCTuple tuple ->
        let rev_values =
          Array.fold ~init:[]
            ~f:(fun consts c ->
              let c = from_const c in
              c :: consts )
            tuple
        in
        Tuple (List.rev rev_values)
    | PYCCode code ->
        Code code


  let as_name = function String s -> Some s | _ -> None

  let as_names = function
    | String s ->
        Ok [s]
    | Null ->
        Ok []
    | Tuple tuple ->
        let opt_lst = List.map ~f:as_name tuple in
        let opt_lst = Option.all opt_lst in
        Result.of_option ~error:() opt_lst
    | _ ->
        Error ()
end

module ConstMap = Caml.Map.Make (Const)

module Ident = struct
  (** An identifier is a sequence of strings, a qualified name. We only keep track of some shallow
      source: an identifier can be [Imported], a [Builtin], or a [Normal] identifier otherwise *)

  type kind = Builtin | Imported | Normal

  (* An identifier is never empty, hence the [root] field. Then we mostly add
     or remove qualifier at the end of the path, so [path] is a reversed list
     of the attribute access on [root] *)
  type t = {root: string; path: string list; kind: kind}

  let pp fmt {root; path} =
    if List.is_empty path then F.pp_print_string fmt root
    else F.fprintf fmt "%s.%a" root (Pp.seq ~sep:"." F.pp_print_string) (List.rev path)


  let mk ?(kind = Normal) root = {root; path= []; kind}

  let from_string ?(kind = Normal) ~on s =
    let l = String.split ~on s in
    match l with
    | [] ->
        L.die ExternalError "Ident.from_string with an empty string"
    | hd :: tl ->
        {root= hd; path= List.rev tl; kind}


  let extend {root; path; kind} attr = {root; path= attr :: path; kind}

  let append {root; path; kind} attrs = {root; path= List.rev_append attrs path; kind}
    [@@warning "-unused-value-declaration"]


  let pop {root; path; kind} = match path with [] -> None | _ :: path -> Some {root; path; kind}

  let length {path} = 1 + List.length path

  let root {root; kind} = {root; path= []; kind}
end

module SSA = struct
  type t = int

  let pp fmt i = F.fprintf fmt "n%d" i
end

module Exp = struct
  type collection = List | Set | Tuple | Slice | Map | String

  let show_binary op =
    match (op : Builtin.binary_op) with
    | Add ->
        "Add"
    | And ->
        "And"
    | FloorDivide ->
        "FloorDivide"
    | LShift ->
        "LShift"
    | MatrixMultiply ->
        "MatrixMultiply"
    | Modulo ->
        "Modulo"
    | Multiply ->
        "Multiply"
    | Or ->
        "Or"
    | Power ->
        "Power"
    | RShift ->
        "RShift"
    | Subtract ->
        "Subtract"
    | TrueDivide ->
        "TrueDivide"
    | Xor ->
        "Xor"


  let show_unary op =
    match (op : Builtin.unary_op) with
    | Positive ->
        "Positive"
    | Negative ->
        "Negative"
    | Not ->
        "Not"
    | Invert ->
        "Invert"


  type import_name = {id: Ident.t; fromlist: string list}

  let pp_import_name fmt {id; fromlist} =
    F.fprintf fmt "$ImportName(%a,@ from_list=[@[%a@]])" Ident.pp id
      (Pp.seq ~sep:", " F.pp_print_string)
      fromlist


  type format_function = Str | Repr | Ascii

  let show_format_function = function Str -> "str" | Repr -> "repr" | Ascii -> "ascii"

  (** An expression is an abstraction of the state of Python bytecode interpreter, waiting to be
      turned into Textual. During the translation from bytecode to expressions, we introduce SSA
      identifiers for "yet to be named" expressions.

      In this IR, name resolution is done so naming is (mostly, cf comment on [MAKE_FUNCTION]) not
      ambiguous. Also, we have reconstructed the CFG of the program, lost during Python compilation
      (TODO) *)
  type t =
    | Const of Const.t
    | Var of Ident.t  (** Non ambiguous name for variables, imports, functions, classes, ... *)
    | LocalVar of string
    | Temp of SSA.t
    | Subscript of {exp: t; index: t}  (** foo[bar] *)
    | Collection of {kind: collection; values: t list}
        (** Helper for [BUILD_LIST] and other builder opcodes *)
    | ConstMap of t ConstMap.t
    | Inplace of Builtin.binary_op
    | Binary of Builtin.binary_op
    | Unary of Builtin.unary_op
    | Compare of Builtin.Compare.t
    | Function of
        {qualname: Ident.t; code: FFI.Code.t; annotations: t ConstMap.t (* TODO: default values *)}
        (** Result of the [MAKE_FUNCTION] opcode *)
    | BuildClass  (** [LOAD_BUILD_CLASS] *)
    | Class of t list
        (** Result of calling [LOAD_BUILD_CLASS] to create a class. Not much is processed of its
            content for now *)
    | GetAttr of (t * string) (* foo.bar *)
    | LoadMethod of (t * string)  (** [LOAD_METHOD] *)
    | ImportName of import_name  (** [IMPORT_NAME] *)
    | ImportFrom of {from: import_name; name: string}  (** [IMPORT_FROM] *)
    | LoadClosure of string  (** [LOAD_CLOSURE] *)
    | Format
    | FormatFn of format_function

  let rec pp fmt = function
    | Const c ->
        Const.pp fmt c
    | Var id ->
        Ident.pp fmt id
    | LocalVar s ->
        F.pp_print_string fmt s
    | Temp i ->
        SSA.pp fmt i
    | Subscript {exp; index} ->
        F.fprintf fmt "%a[@[%a@]]" pp exp pp index
    | Collection {kind; values} -> (
      match kind with
      | List ->
          F.fprintf fmt "[@[%a@]]" (Pp.seq ~sep:", " pp) values
      | Set ->
          F.fprintf fmt "{@[%a@]}" (Pp.seq ~sep:", " pp) values
      | Map ->
          F.fprintf fmt "{|@[%a@]|}" (Pp.seq ~sep:", " pp) values
      | Tuple ->
          F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " pp) values
      | String ->
          F.fprintf fmt "$Concat(%a)" (Pp.seq ~sep:", " pp) values
      | Slice ->
          F.fprintf fmt "[@[%a@]]" (Pp.seq ~sep:":" pp) values )
    | ConstMap map ->
        F.fprintf fmt "{@[" ;
        ConstMap.iter (fun key exp -> F.fprintf fmt "%a: %a, " Const.pp key pp exp) map ;
        F.fprintf fmt "@]}"
    | Binary op ->
        let op = show_binary op in
        F.fprintf fmt "$Binary.%s" op
    | Inplace op ->
        let op = show_binary op in
        F.fprintf fmt "$Inplace.%s" op
    | Unary op ->
        let op = show_unary op in
        F.fprintf fmt "$Unary.%s" op
    | Compare op ->
        F.fprintf fmt "$Compare.%a" Builtin.Compare.pp op
    | Function {qualname; code} ->
        let {FFI.Code.co_name} = code in
        F.fprintf fmt "$FuncObj(%s, %a)" co_name Ident.pp qualname
    | BuildClass ->
        F.pp_print_string fmt "LOAD_BUILD_CLASS"
    | Class cls ->
        F.fprintf fmt "$ClassObj(%a)" (Pp.seq ~sep:", " pp) cls
    | GetAttr (t, name) ->
        F.fprintf fmt "%a.%s" pp t name
    | LoadMethod (self, meth) ->
        F.fprintf fmt "$LoadMethod(%a, %s)" pp self meth
    | ImportName {id; fromlist} ->
        F.fprintf fmt "$ImportName(%a,@ from_list= [@[%a@]])" Ident.pp id
          (Pp.seq ~sep:", " F.pp_print_string)
          fromlist
    | ImportFrom {from; name} ->
        F.fprintf fmt "$ImportFrom(%a,@ name= %s)" pp_import_name from name
    | LoadClosure s ->
        F.fprintf fmt "$LoadClosure(%s)" s
    | Format ->
        F.pp_print_string fmt "$Format"
    | FormatFn fn ->
        F.fprintf fmt "$FormatFn.%s" (show_format_function fn)


  let as_short_string = function Const (String s) -> Some s | _ -> None
end

module Error = struct
  type kind =
    | EmptyStack of string
    | UnsupportedOpcode of string
    | MakeFunction of string * Exp.t
    | BuildConstKeyMapLength of int * int
    | BuildConstKeyMapKeys of Exp.t
    | LoadBuildClass of Exp.t list
    | LoadBuildClassName of Exp.t
    | ImportNameFromList of (string * Exp.t)
    | ImportNameLevel of (string * Exp.t)
    | ImportNameDepth of (Ident.t * int)
    | ImportFrom of (string * Exp.t)
    | CompareOp of int
    | UnpackSequence of int
    | FormatValueSpec of Exp.t

  type t = L.error * kind

  let pp_kind fmt = function
    | EmptyStack op ->
        F.fprintf fmt "Cannot %s, stack is empty" op
    | UnsupportedOpcode s ->
        F.fprintf fmt "Unsupported opcode: %s" s
    | MakeFunction (kind, exp) ->
        F.fprintf fmt "MAKE_FUNCTION: expected %s but got %a" kind Exp.pp exp
    | BuildConstKeyMapLength (m, n) ->
        F.fprintf fmt "BUILD_CONST_KEY_MAP: expected %d keys but got %d" m n
    | BuildConstKeyMapKeys exp ->
        F.fprintf fmt "BUILD_CONST_KEY_MAP: expect constant set of keys but got %a" Exp.pp exp
    | LoadBuildClass args ->
        F.fprintf fmt "LOAD_BUILD_CLASS: not enough arguments. Got %a" (Pp.seq ~sep:", " Exp.pp)
          args
    | LoadBuildClassName arg ->
        F.fprintf fmt "LOAD_BUILD_CLASS: expected a name, but got %a" Exp.pp arg
    | ImportNameFromList (name, args) ->
        F.fprintf fmt "IMPORT_NAME(%s): expected constant fromlist but got %a" name Exp.pp args
    | ImportNameLevel (name, arg) ->
        F.fprintf fmt "IMPORT_NAME(%s); expected int but got %a" name Exp.pp arg
    | ImportNameDepth (module_name, depth) ->
        F.fprintf fmt
          "IMPORT_NAME: module path %a is not deep enough for relative import with level %d"
          Ident.pp module_name depth
    | ImportFrom (name, from) ->
        F.fprintf fmt "IMPORT_FROM(%s): expected an `import` but got %a" name Exp.pp from
    | CompareOp n ->
        F.fprintf fmt "COMPARE_OP(%d): invalid operation" n
    | UnpackSequence n ->
        F.fprintf fmt "UNPACK_SEQUENCE: invalid count %d" n
    | FormatValueSpec exp ->
        F.fprintf fmt "FORMAT_VALUE: expected string literal, got %a" Exp.pp exp
end

module Stmt = struct
  (** Statements are an extension of Textual's statements: we still keep some specific Python
      behavior around, like the difference between [CALL_FUNCTION] and [CALL_METHOD] *)
  type call_arg = {name: string option; value: Exp.t}

  let pp_call_arg fmt {name; value} =
    match name with
    | None ->
        Exp.pp fmt value
    | Some name ->
        F.fprintf fmt "%s= %a" name Exp.pp value


  let unnamed_call_arg value = {name= None; value}

  let unnamed_call_args args = List.map ~f:unnamed_call_arg args

  type t =
    | Return of Exp.t
    | Assign of {lhs: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: call_arg list}
    | CallMethod of {lhs: SSA.t; call: Exp.t; args: Exp.t list}
    | ImportName of Exp.import_name
    | SetupAnnotations

  let pp fmt = function
    | Return exp ->
        F.fprintf fmt "return %a" Exp.pp exp
    | Assign {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" Exp.pp lhs Exp.pp rhs
    | Call {lhs; exp; args} ->
        F.fprintf fmt "%a <- %a(@[%a@])" SSA.pp lhs Exp.pp exp (Pp.seq ~sep:", " pp_call_arg) args
    | CallMethod {lhs; call; args} ->
        F.fprintf fmt "%a <- $CallMethod(%a, @[%a@])" SSA.pp lhs Exp.pp call
          (Pp.seq ~sep:", " Exp.pp) args
    | ImportName import_name ->
        Exp.pp_import_name fmt import_name
    | SetupAnnotations ->
        F.pp_print_string fmt "$SETUP_ANNOTATIONS"
end

module Object = struct
  (** Everything in Python is an [Object]. A function is an [Object], a class is an [Object]. We
      will refine these into functions, classes and closures during the translation to Textual. *)

  type t =
    { name: Ident.t
    ; toplevel: Stmt.t list
    ; objects: t list (* TODO: maybe turn this into a map using classes/functions *)
    ; classes: SSet.t
    ; functions: Ident.t SMap.t }

  let rec pp fmt {name; toplevel; objects; classes; functions} =
    F.fprintf fmt "@[<hv2>object %a:@\n" Ident.pp name ;
    if not (List.is_empty toplevel) then (
      F.fprintf fmt "@[<hv2>code:@\n" ;
      List.iter toplevel ~f:(F.fprintf fmt "@[<hv2>%a@]@\n" Stmt.pp) ;
      F.fprintf fmt "@]@\n" ) ;
    if not (List.is_empty objects) then (
      F.fprintf fmt "@[<hv2>objects:@\n" ;
      List.iter objects ~f:(F.fprintf fmt "@[%a@]@\n" pp) ;
      F.fprintf fmt "@]@\n" ) ;
    if not (SSet.is_empty classes) then (
      F.fprintf fmt "@[<hv2>classes:@\n" ;
      SSet.iter (F.fprintf fmt "@[%a@]@\n" F.pp_print_string) classes ;
      F.fprintf fmt "@]@\n" ) ;
    if not (SMap.is_empty functions) then (
      F.fprintf fmt "@[<hv2>functions:@\n" ;
      SMap.iter (fun short long -> F.fprintf fmt "@[%s -> %a@]@\n" short Ident.pp long) functions ;
      F.fprintf fmt "@]@\n" )
end

module Module = struct
  (** A module is just the "main" [Object] of a Python file, often called toplevel too *)
  type t = Object.t

  let pp fmt obj = F.fprintf fmt "module@\n%a@\n@\n" Object.pp obj
end

module State = struct
  (** Internal state of the Bytecode -> IR compiler *)
  type t =
    { module_name: Ident.t
    ; debug: bool
    ; global_names: Ident.t SMap.t  (** Translation cache for global names *)
    ; stack: Exp.t list
    ; stmts: Stmt.t list
    ; fresh_id: SSA.t
    ; classes: SSet.t
    ; functions: Ident.t SMap.t
    ; names: Ident.t SMap.t  (** Translation cache for local names *) }

  let known_global_names =
    let builtins =
      [ "print"
      ; "range"
      ; "open"
      ; "len"
      ; "type"
      ; "str"
      ; "int"
      ; "float"
      ; "bool"
      ; "object"
      ; "super"
      ; "hasattr" ]
    in
    List.fold builtins ~init:SMap.empty ~f:(fun names name ->
        SMap.add name (Ident.mk ~kind:Builtin name) names )


  let known_local_names =
    let locals = ["__name__"; "staticmethod"] in
    List.fold locals ~init:SMap.empty ~f:(fun names name ->
        SMap.add name (Ident.mk ~kind:Builtin name) names )


  let empty debug module_name =
    { module_name
    ; debug
    ; global_names= known_global_names
    ; stack= []
    ; stmts= []
    ; fresh_id= 0
    ; classes= SSet.empty
    ; functions= SMap.empty
    ; names= known_local_names }


  (** Each time a new object is discovered (they can be heavily nested), we need to clear the state
      of temporary data like the SSA counter, but keep other parts of it, like global and local
      names *)
  let enter {debug; global_names; names} module_name =
    let st = empty debug module_name in
    {st with global_names; names}


  let fresh_id ({fresh_id} as st) =
    let st = {st with fresh_id= fresh_id + 1} in
    (fresh_id, st)


  let is_toplevel {module_name} = Int.equal 1 (Ident.length module_name)

  let resolve_local_name names global_names name =
    match SMap.find_opt name names with
    | Some id ->
        Some id
    | None ->
        SMap.find_opt name global_names


  (** Python can look up names in the current namespace, or directly in the global namespace, which
      is why we keep them separated *)
  let resolve_name ~global ({debug; names; global_names} as st) name =
    let global = global || is_toplevel st in
    let debug = if debug then Debug.todo else Debug.p in
    debug "resolve_name global= %b name= %s" global name ;
    let res =
      if global then SMap.find_opt name global_names else resolve_local_name names global_names name
    in
    match res with
    | Some id ->
        debug " -> %a@\n" Ident.pp id ;
        id
    | None ->
        debug " -> Not found@\n" ;
        let prefix = Ident.mk "$unknown" in
        Ident.extend prefix name


  let register_name ~global ({debug; names; global_names} as st) name id =
    let global = global || is_toplevel st in
    let debug = if debug then Debug.todo else Debug.p in
    debug "register_name global= %b name= %s id= %a@\n" global name Ident.pp id ;
    if global then
      let global_names = SMap.add name id global_names in
      {st with global_names}
    else
      let names = SMap.add name id names in
      {st with names}


  let push ({stack} as st) exp = {st with stack= exp :: stack}

  let pop ({stack} as st) =
    match stack with
    | [] ->
        Error (L.InternalError, Error.EmptyStack "pop")
    | exp :: stack ->
        let st = {st with stack} in
        Ok (exp, st)


  let peek {stack} =
    match stack with [] -> Error (L.InternalError, Error.EmptyStack "peek") | exp :: _ -> Ok exp


  let pop_n st n =
    let rec aux acc st n =
      let open IResult.Let_syntax in
      if n > 0 then
        let* hd, st = pop st in
        aux (hd :: acc) st (n - 1)
      else Ok (acc, st)
    in
    aux [] st n


  let push_stmt ({stmts} as st) stmt = {st with stmts= stmt :: stmts}

  let register_class ({classes} as st) cls = {st with classes= SSet.add cls classes}

  let register_function ({functions} as st) name fn = {st with functions= SMap.add name fn functions}
end

(** Helper to compile the binary/unary/... ops into IR *)
let parse_op st exp n =
  let open IResult.Let_syntax in
  let* args, st = State.pop_n st n in
  let args = Stmt.unnamed_call_args args in
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.Call {lhs; exp; args} in
  let st = State.push_stmt st stmt in
  let st = State.push st (Exp.Temp lhs) in
  Ok st


(** Helper to compile the BUILD_ opcodes into IR *)
let build_collection st kind count =
  let open IResult.Let_syntax in
  let* values, st = State.pop_n st count in
  let exp = Exp.Collection {kind; values} in
  let st = State.push st exp in
  Ok st


let make_function st flags =
  let open IResult.Let_syntax in
  let* qualname, st = State.pop st in
  let* qualname =
    (* During MAKE_FUNCTION, all names within a def/class seems to be prefixed
       with the value that is stored in [__module__]. We'll let the next phase
       IR -> Textual generate the right names.

       TODO: try to make things clear here once and for all *)
    match (qualname : Exp.t) with
    | Const (String s) ->
        let lnames = Ident.from_string ~on:'.' s in
        Ok lnames
    | _ ->
        Error (L.InternalError, Error.MakeFunction ("a qualified named", qualname))
  in
  let* codeobj, st = State.pop st in
  let* code =
    match (codeobj : Exp.t) with
    | Const (Code c) ->
        Ok c
    | _ ->
        Error (L.InternalError, Error.MakeFunction ("a code object", codeobj))
  in
  let {FFI.Code.co_name= fn} = code in
  let* st =
    if flags land 0x08 <> 0 then
      (* TODO: closure *)
      let* _cells_for_closure, st = State.pop st in
      Ok st
    else Ok st
  in
  let* annotations, st =
    if flags land 0x04 <> 0 then
      let* annotations, st = State.pop st in
      let* annotations =
        match (annotations : Exp.t) with
        | ConstMap map ->
            Ok map
        | _ ->
            Error (L.InternalError, Error.MakeFunction ("some type annotations", annotations))
      in
      Ok (annotations, st)
    else Ok (ConstMap.empty, st)
  in
  let* st =
    if flags land 0x02 <> 0 then
      (* TODO: kw defaults *)
      let* _kw_defaults, st = State.pop st in
      Ok st
    else Ok st
  in
  (* TODO defaults *)
  let* _defaults, st =
    if flags land 0x01 <> 0 then
      let* defaults, st = State.pop st in
      Ok (Some defaults, st)
    else Ok (None, st)
  in
  let exp = Exp.Function {annotations; qualname; code} in
  let st = State.push st exp in
  let st = State.register_function st fn qualname in
  Ok st


let call_function_with_unnamed_args st exp args =
  let args = Stmt.unnamed_call_args args in
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.Call {lhs; exp; args} in
  let st = State.push_stmt st stmt in
  Ok (lhs, st)


let call_function st arg =
  let open IResult.Let_syntax in
  let* args, st = State.pop_n st arg in
  let* fun_exp, st = State.pop st in
  match (fun_exp : Exp.t) with
  | BuildClass ->
      (* Building a class looks like
          LOAD_BUILD_CLASS
          LOAD_CONST               CODE_OBJ
          LOAD_CONST               SHORT_NAME
          ... more arguments ...
          MAKE_FUNCTION            M
          LOAD_CONST               SHORT_NAME
          ... more arguments ...
          CALL_FUNCTION            N

         so we'll process it in a minimal way to extract the short name
         which is the second argument (the first being the code object)
      *)
      let sz = List.length args in
      let* () = if sz < 2 then Error (L.ExternalError, Error.LoadBuildClass args) else Ok () in
      let* short_name =
        match args with
        | _code :: short_name :: _ ->
            Result.of_option
              ~error:(L.InternalError, Error.LoadBuildClassName short_name)
              (Exp.as_short_string short_name)
        | _ ->
            Error (L.ExternalError, Error.LoadBuildClass args)
      in
      let st = State.register_class st short_name in
      let exp = Exp.Class args in
      let st = State.push st exp in
      Ok st
  | _ ->
      let* id, st = call_function_with_unnamed_args st fun_exp args in
      let st = State.push st (Exp.Temp id) in
      Ok st


let import_name st name =
  let open IResult.Let_syntax in
  let {State.module_name} = st in
  let* fromlist, st = State.pop st in
  let* fromlist =
    let error = (L.ExternalError, Error.ImportNameFromList (name, fromlist)) in
    match (fromlist : Exp.t) with
    | Const c ->
        Result.map_error ~f:(fun () -> error) (Const.as_names c)
    | _ ->
        Error error
  in
  let* level, st = State.pop st in
  let* level =
    match (level : Exp.t) with
    | Const (Int z) ->
        Ok (Int64.to_int_exn z)
    | _ ->
        Error (L.ExternalError, Error.ImportNameLevel (name, level))
  in
  let id = Ident.from_string ~kind:Imported ~on:'.' name in
  let* import_path =
    match level with
    | 0 ->
        (* Absolute path *)
        Ok id
    | _ -> (
        (* Relative path *)
        let rec pop_levels n id =
          match (n, id) with
          | 0, None ->
              Some `Root
          | 0, Some id ->
              Some (`Path id)
          | _, None ->
              None
          | _, Some id ->
              let next_id = Ident.pop id in
              pop_levels (n - 1) next_id
        in
        let prefix = pop_levels level (Some module_name) in
        match prefix with
        | Some `Root ->
            Ok id
        | Some (`Path prefix) ->
            if String.is_empty name then Ok prefix else Ok (Ident.extend prefix name)
        | None ->
            Error (L.ExternalError, Error.ImportNameDepth (module_name, level)) )
  in
  let import_name = {Exp.id= import_path; fromlist} in
  let exp = Exp.ImportName import_name in
  let st = State.push st exp in
  let stmt = Stmt.ImportName import_name in
  (* Keeping them as a statement so IR -> Textual can correctly generate the toplevel/side-effect call *)
  let st = State.push_stmt st stmt in
  Ok st


let import_from st name =
  let open IResult.Let_syntax in
  (* Does not pop the top of stack, which should be the result of an IMPORT_NAME *)
  let* from = State.peek st in
  let* ({Exp.fromlist} as from) =
    match (from : Exp.t) with
    | ImportName from ->
        Ok from
    | _ ->
        Error (L.ExternalError, Error.ImportFrom (name, from))
  in
  if not (List.mem ~equal:String.equal fromlist name) then
    L.user_warning "Import from / name mismatch: cannot find '%s'@\n" name ;
  let exp = Exp.ImportFrom {from; name} in
  let st = State.push st exp in
  Ok st


let target_of_import ~default exp =
  let target =
    match (exp : Exp.t) with
    | ImportName {id} ->
        Some id
    | ImportFrom {from= {id}; name} ->
        Some (Ident.extend id name)
    | _ ->
        None
  in
  Option.value ~default target


let unpack_sequence st count =
  let open IResult.Let_syntax in
  let* tos, st = State.pop st in
  let rec unpack st n =
    if n < 0 then st
    else
      let index = Exp.Const (Int (Int64.of_int n)) in
      let exp = Exp.Subscript {exp= tos; index} in
      let st = State.push st exp in
      unpack st (n - 1)
  in
  let* () = if count <= 0 then Error (L.ExternalError, Error.UnpackSequence count) else Ok () in
  let st = unpack st (count - 1) in
  Ok st


let format_value st flags =
  let open IResult.Let_syntax in
  let has_fmt_spec flags = Int.equal (flags land 0x04) 0x04 in
  let mk_conv flags =
    match flags land 0x03 with
    | 0x00 ->
        None
    | 0x01 ->
        Some Exp.Str
    | 0x02 ->
        Some Exp.Repr
    | 0x03 ->
        Some Exp.Ascii
    | _ ->
        L.die InternalError "FORMAT_VALUE: unreachable"
  in
  let* fmt_spec, st =
    (* fmt_spec must be a string literal *)
    if has_fmt_spec flags then
      let* fmt_spec, st = State.pop st in
      match (fmt_spec : Exp.t) with
      | Const (String _) ->
          Ok (fmt_spec, st)
      | _ ->
          Error (L.ExternalError, Error.FormatValueSpec fmt_spec)
    else Ok (Exp.Const Null, st)
  in
  let* exp, st = State.pop st in
  let conv_fn = mk_conv flags in
  let* exp, st =
    match conv_fn with
    | None ->
        Ok (exp, st)
    | Some conv_fn ->
        let fn = Exp.FormatFn conv_fn in
        let* id, st = call_function_with_unnamed_args st fn [exp] in
        Ok (Exp.Temp id, st)
  in
  let* id, st = call_function_with_unnamed_args st Exp.Format [exp; fmt_spec] in
  let st = State.push st (Exp.Temp id) in
  Ok st


let parse_bytecode st {FFI.Code.co_consts; co_names; co_varnames; co_cellvars; co_freevars}
    {FFI.Instruction.opname; starts_line; arg} =
  let open IResult.Let_syntax in
  let debug =
    let {State.debug} = st in
    if debug then Debug.todo else Debug.p
  in
  if Option.is_some starts_line then debug ">@\n" ;
  debug "> %s %d (0x%x)@\n" opname arg arg ;
  match opname with
  | "LOAD_CONST" ->
      let c = co_consts.(arg) in
      let c = Const.from_const c in
      let exp = Exp.Const c in
      let st = State.push st exp in
      Ok st
  | "LOAD_NAME" ->
      let name = co_names.(arg) in
      let target = State.resolve_name ~global:false st name in
      let exp = Exp.Var target in
      let st = State.push st exp in
      Ok st
  | "LOAD_GLOBAL" ->
      let name = co_names.(arg) in
      let target = State.resolve_name ~global:true st name in
      let exp = Exp.Var target in
      let st = State.push st exp in
      Ok st
  | "LOAD_FAST" ->
      let name = co_varnames.(arg) in
      let exp = Exp.LocalVar name in
      let st = State.push st exp in
      Ok st
  | "LOAD_ATTR" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let exp = Exp.GetAttr (tos, name) in
      let st = State.push st exp in
      Ok st
  | "STORE_NAME" ->
      let name = co_names.(arg) in
      let {State.module_name} = st in
      let target = Ident.extend module_name name in
      let lhs = Exp.Var target in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let target = target_of_import rhs ~default:target in
      let st = State.register_name ~global:false st name target in
      let st = State.push_stmt st stmt in
      Ok st
  | "STORE_GLOBAL" ->
      let name = co_names.(arg) in
      let {State.module_name} = st in
      let root = Ident.root module_name in
      let target = Ident.extend root name in
      let lhs = Exp.Var target in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let target = target_of_import rhs ~default:target in
      let st = State.register_name ~global:true st name target in
      let st = State.push_stmt st stmt in
      Ok st
  | "STORE_FAST" ->
      let name = co_varnames.(arg) in
      let lhs = Exp.LocalVar name in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok st
  | "STORE_ATTR" ->
      let name = co_names.(arg) in
      let* root, st = State.pop st in
      let* rhs, st = State.pop st in
      let lhs = Exp.GetAttr (root, name) in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok st
  | "STORE_SUBSCR" ->
      (* Implements TOS1[TOS] = TOS2.  *)
      let* index, st = State.pop st in
      let* exp, st = State.pop st in
      let* rhs, st = State.pop st in
      let lhs = Exp.Subscript {exp; index} in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok st
  | "RETURN_VALUE" ->
      let* ret, st = State.pop st in
      let stmt = Stmt.Return ret in
      let st = State.push_stmt st stmt in
      Ok st
  | "CALL_FUNCTION" ->
      call_function st arg
  | "POP_TOP" -> (
      (* keep the popped values around in case their construction involves side effects *)
      let* rhs, st = State.pop st in
      match (rhs : Exp.t) with
      | ImportName _
      (* IMPORT_NAME are kept around for multiple 'from' statement and
         then popped. The translation to textual will use the statement
         version do deal with their side effect at the right location *)
      | Temp _ ->
          Ok st
      | _ ->
          let id, st = State.fresh_id st in
          let stmt = Stmt.Assign {lhs= Exp.Temp id; rhs} in
          let st = State.push_stmt st stmt in
          Ok st )
  | "BINARY_ADD" ->
      parse_op st (Exp.Binary Add) 2
  | "BINARY_SUBTRACT" ->
      parse_op st (Exp.Binary Subtract) 2
  | "BINARY_AND" ->
      parse_op st (Exp.Binary And) 2
  | "BINARY_FLOOR_DIVIDE" ->
      parse_op st (Exp.Binary FloorDivide) 2
  | "BINARY_LSHIFT" ->
      parse_op st (Exp.Binary LShift) 2
  | "BINARY_MATRIX_MULTIPLY" ->
      parse_op st (Exp.Binary MatrixMultiply) 2
  | "BINARY_MODULO" ->
      parse_op st (Exp.Binary Modulo) 2
  | "BINARY_MULTIPLY" ->
      parse_op st (Exp.Binary Multiply) 2
  | "BINARY_OR" ->
      parse_op st (Exp.Binary Or) 2
  | "BINARY_POWER" ->
      parse_op st (Exp.Binary Power) 2
  | "BINARY_RSHIFT" ->
      parse_op st (Exp.Binary RShift) 2
  | "BINARY_TRUE_DIVIDE" ->
      parse_op st (Exp.Binary TrueDivide) 2
  | "BINARY_XOR" ->
      parse_op st (Exp.Binary Xor) 2
  | "INPLACE_ADD" ->
      parse_op st (Exp.Inplace Add) 2
  | "INPLACE_SUBTRACT" ->
      parse_op st (Exp.Inplace Subtract) 2
  | "INPLACE_AND" ->
      parse_op st (Exp.Inplace And) 2
  | "INPLACE_FLOOR_DIVIDE" ->
      parse_op st (Exp.Inplace FloorDivide) 2
  | "INPLACE_LSHIFT" ->
      parse_op st (Exp.Inplace LShift) 2
  | "INPLACE_MATRIX_MULTIPLY" ->
      parse_op st (Exp.Inplace MatrixMultiply) 2
  | "INPLACE_MODULO" ->
      parse_op st (Exp.Inplace Modulo) 2
  | "INPLACE_MULTIPLY" ->
      parse_op st (Exp.Inplace Multiply) 2
  | "INPLACE_OR" ->
      parse_op st (Exp.Inplace Or) 2
  | "INPLACE_POWER" ->
      parse_op st (Exp.Inplace Power) 2
  | "INPLACE_RSHIFT" ->
      parse_op st (Exp.Inplace RShift) 2
  | "INPLACE_TRUE_DIVIDE" ->
      parse_op st (Exp.Inplace TrueDivide) 2
  | "INPLACE_XOR" ->
      parse_op st (Exp.Inplace Xor) 2
  | "UNARY_POSITIVE" ->
      parse_op st (Exp.Unary Positive) 1
  | "UNARY_NEGATIVE" ->
      parse_op st (Exp.Unary Negative) 1
  | "UNARY_NOT" ->
      parse_op st (Exp.Unary Not) 1
  | "UNARY_INVERT" ->
      parse_op st (Exp.Unary Invert) 1
  | "MAKE_FUNCTION" ->
      make_function st arg
  | "BUILD_CONST_KEY_MAP" ->
      let* keys, st = State.pop st in
      let* keys =
        match (keys : Exp.t) with
        | Const (Tuple keys) ->
            Ok keys
        | _ ->
            Error (L.InternalError, Error.BuildConstKeyMapKeys keys)
      in
      let nr_keys = List.length keys in
      let* tys, st = State.pop_n st arg in
      let* () =
        if Int.equal nr_keys arg then Ok ()
        else Error (L.InternalError, Error.BuildConstKeyMapLength (arg, nr_keys))
      in
      let map =
        List.fold2_exn ~init:ConstMap.empty ~f:(fun map key ty -> ConstMap.add key ty map) keys tys
      in
      let st = State.push st (Exp.ConstMap map) in
      Ok st
  | "BUILD_LIST" ->
      build_collection st List arg
  | "BUILD_SET" ->
      build_collection st Set arg
  | "BUILD_TUPLE" ->
      build_collection st Tuple arg
  | "BUILD_SLICE" ->
      build_collection st Slice arg
  | "BUILD_STRING" ->
      build_collection st String arg
  | "BUILD_MAP" ->
      build_collection st Map (2 * arg)
  | "BINARY_SUBSCR" ->
      let* index, st = State.pop st in
      let* exp, st = State.pop st in
      let exp = Exp.Subscript {exp; index} in
      let st = State.push st exp in
      Ok st
  | "LOAD_BUILD_CLASS" ->
      let st = State.push st BuildClass in
      Ok st
  | "LOAD_METHOD" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let exp = Exp.LoadMethod (tos, name) in
      let st = State.push st exp in
      Ok st
  | "CALL_METHOD" ->
      let* args, st = State.pop_n st arg in
      let* call, st = State.pop st in
      let lhs, st = State.fresh_id st in
      let stmt = Stmt.CallMethod {lhs; call; args} in
      let st = State.push_stmt st stmt in
      let st = State.push st (Exp.Temp lhs) in
      Ok st
  | "SETUP_ANNOTATIONS" ->
      let {State.module_name} = st in
      let annotations = Ident.extend module_name PyCommon.annotations in
      let st = State.register_name ~global:false st PyCommon.annotations annotations in
      let st = State.push_stmt st SetupAnnotations in
      Ok st
  | "IMPORT_NAME" ->
      let name = co_names.(arg) in
      import_name st name
  | "IMPORT_FROM" ->
      let name = co_names.(arg) in
      import_from st name
  | "COMPARE_OP" ->
      let* cmp_op =
        match List.nth Builtin.Compare.all arg with
        | Some op ->
            Ok op
        | None ->
            Error (L.ExternalError, Error.CompareOp arg)
      in
      let* rhs, st = State.pop st in
      let* lhs, st = State.pop st in
      let exp = Exp.Compare cmp_op in
      let* id, st = call_function_with_unnamed_args st exp [lhs; rhs] in
      let st = State.push st (Exp.Temp id) in
      Ok st
  | "LOAD_CLOSURE" ->
      let cell =
        let sz = Array.length co_cellvars in
        if arg < sz then co_cellvars.(arg) else co_freevars.(arg - sz)
      in
      (* We currently do nothing. It will be up to the IR -> Textual step to deal with these *)
      let exp = Exp.LoadClosure cell in
      let st = State.push st exp in
      Ok st
  | "DUP_TOP" ->
      let* tos = State.peek st in
      let st = State.push st tos in
      Ok st
  | "UNPACK_SEQUENCE" ->
      unpack_sequence st arg
  | "FORMAT_VALUE" ->
      format_value st arg
  | _ ->
      Error (L.InternalError, Error.UnsupportedOpcode opname)


let rec mk_stmts st code instructions =
  let open IResult.Let_syntax in
  match instructions with
  | [] ->
      Ok st
  | instr :: instructions ->
      let* st = parse_bytecode st code instr in
      let* st = mk_stmts st code instructions in
      Ok st


let rec mk_object st ({FFI.Code.instructions; co_consts} as code) =
  let open IResult.Let_syntax in
  let {State.module_name} = st in
  let* ({State.stmts; classes; functions} as st) = mk_stmts st code instructions in
  let toplevel = List.rev stmts in
  let* objects =
    Array.fold_result co_consts ~init:[] ~f:(fun objects constant ->
        match constant with
        | FFI.Constant.PYCCode code ->
            let {FFI.Code.co_name} = code in
            let module_name = Ident.extend module_name co_name in
            let st = State.enter st module_name in
            let* obj = mk_object st code in
            Ok (obj :: objects)
        | _ ->
            Ok objects )
  in
  let objects = List.rev objects in
  Ok {Object.name= module_name; toplevel; objects; classes; functions}


let mk ~debug ({FFI.Code.co_filename} as code) =
  let open IResult.Let_syntax in
  let file_path =
    let sz = String.length co_filename in
    if sz >= 2 && String.equal "./" (String.sub co_filename ~pos:0 ~len:2) then
      String.sub co_filename ~pos:2 ~len:(sz - 2)
    else co_filename
  in
  let file_path = Stdlib.Filename.remove_extension file_path in
  let module_name = Ident.from_string ~on:'/' file_path in
  let empty = State.empty debug module_name in
  let* obj = mk_object empty code in
  Ok obj
