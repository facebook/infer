(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module T = Textual
module Debug = PyDebug
module Builtin = PyBuiltin

let type_name = PyCommon.type_name

module DataStack = struct
  type cell =
    | Const of int
    | Name of {global: bool; ndx: int}
    | VarName of int
    | Temp of T.Ident.t
    | Code of {fun_or_class: bool; code_name: string; code: FFI.Code.t}
    | Map of (string * cell) list
    | BuiltinBuildClass
    | Import of {import_path: string; symbols: string list}
      (* TODO: change import_path into a list when we supported structured path foo.bar.baz *)
    | ImportCall of T.qualified_procname
    | MethodCall of {receiver: T.Exp.t; name: T.qualified_procname}
    | StaticCall of T.qualified_procname
  [@@deriving show]

  let as_code FFI.Code.{co_consts} = function
    | Const n ->
        let code = co_consts.(n) in
        FFI.Constant.as_code code
    | Code {code} ->
        Some code
    | Name _
    | Temp _
    | VarName _
    | Map _
    | BuiltinBuildClass
    | Import _
    | ImportCall _
    | MethodCall _
    | StaticCall _ ->
        None


  let as_name FFI.Code.{co_varnames; co_names; co_consts} = function
    | Const ndx ->
        let cst = co_consts.(ndx) in
        FFI.Constant.as_name cst
    | Name {ndx} ->
        Some co_names.(ndx)
    | VarName ndx ->
        Some co_varnames.(ndx)
    | Code _
    | Temp _
    | Map _
    | BuiltinBuildClass
    | Import _
    | ImportCall _
    | MethodCall _
    | StaticCall _ ->
        None


  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)

  let peek = function [] -> None | hd :: _ -> Some hd
end

module Labels = Caml.Map.Make (Int)

(* TODO(vsiles): maybe revamp Signature maps to benefits from the new
   qualified name structure *)
module Signature = struct
  type t = {annotations: PyCommon.annotated_name list; is_static: bool}
end

module SMap = Caml.Map.Make (String)

type info = {is_code: bool; is_class: bool; typ: T.Typ.t}

module Symbol = struct
  module Qualified = struct
    type prefix = string list

    (** Fully qualified name. Each identifier for global variables / function names / class names
        have a prefix sequence with the module and optional class names. The suffix is usually the
        original "short" name. *)
    type t = {prefix: prefix; name: string; loc: T.Location.t}

    let mk ~prefix name loc = {prefix; name; loc}

    let prefix_to_string prefix = String.concat ~sep:"::" prefix

    let to_string ~sep ?(static = false) {prefix; name} =
      let name = if static then PyCommon.static_companion name else name in
      if List.is_empty prefix then name
      else
        let prefix = prefix_to_string prefix in
        sprintf "%s%s%s" prefix sep name


    let to_qualified_procname {prefix; name; loc} : T.qualified_procname =
      let enclosing_class =
        if List.is_empty prefix then T.TopLevel
        else
          let value = String.concat ~sep:"::" prefix in
          let type_name = type_name ~loc value in
          T.Enclosing type_name
      in
      let name = {T.ProcName.value= name; loc} in
      {T.enclosing_class; name}


    let to_type_name ~is_static {prefix; name; loc} : T.TypeName.t =
      let value = String.concat ~sep:"::" prefix in
      let name = if is_static then PyCommon.static_companion name else name in
      let value = if String.is_empty value then name else sprintf "%s::%s" value name in
      type_name ~loc value


    let to_typ qual : T.Typ.t =
      let typ = to_type_name ~is_static:false qual in
      T.Typ.(Ptr (Struct typ))
  end

  type t =
    | Name of {symbol_name: Qualified.t; is_imported: bool; typ: T.Typ.t}
    | Builtin
    | Code of {code_name: Qualified.t}
    | Class of {class_name: Qualified.t}
    | Import of {import_path: string}

  let to_string ?(code_sep = ".") ?(static = false) = function
    | Class {class_name= qname} | Name {symbol_name= qname} ->
        (* Names and classes are global symobls, without an enclosing class, so we mangle them
           using the "::" separator *)
        Qualified.to_string ~sep:"::" ~static qname
    | Builtin ->
        L.die InternalError "Symbol.to_string called with Builtin"
    | Code {code_name= qname} ->
        (* Functions are used as qualified_procnames so we using "." as a separator.
           However because of how classes are initialized, we might end up
           registering a [Code] but really it's a class. Because we can't
           detect this early, we allow to override "." with "::", using
           [code_sep].
        *)
        Qualified.to_string ~sep:code_sep qname
    | Import {import_path} ->
        import_path


  let to_qualified_procname = function
    | Builtin ->
        L.die InternalError "Symbol.to_qualified_procname called with Builtin"
    | Import _ ->
        L.die InternalError "Symbol.to_qualified_procname called with Import"
    | Name {symbol_name= qname} | Code {code_name= qname} | Class {class_name= qname} ->
        Qualified.to_qualified_procname qname


  let to_type_name ~is_static = function
    | Builtin ->
        L.die InternalError "Symbol.to_type_name called with Builtin"
    | Import _ ->
        L.die InternalError "Symbol.to_type_name called with Import"
    | Name _ ->
        L.die InternalError "Symbol.to_type_name called with Name"
    | Code _ ->
        L.die InternalError "Symbol.to_type_name called with Code"
    | Class {class_name= qname} ->
        Qualified.to_type_name ~is_static qname


  let to_typ = function
    | Class {class_name} ->
        Some (Qualified.to_typ class_name)
    | Builtin | Import _ | Name _ | Code _ ->
        None


  let pp fmt = function
    | Name {symbol_name; typ} ->
        Format.fprintf fmt "Name(%s: %a)" (Qualified.to_string ~sep:"::" symbol_name) T.Typ.pp typ
    | Builtin ->
        Format.pp_print_string fmt "Builtin"
    | Code {code_name} ->
        Format.fprintf fmt "Code(%s)" (Qualified.to_string ~sep:"." code_name)
    | Class {class_name} ->
        Format.fprintf fmt "Class(%s)" (Qualified.to_string ~sep:"::" class_name)
    | Import {import_path} ->
        Format.fprintf fmt "Import(%s)" import_path
end

module Import = struct
  type t = TopLevel of string | Call of T.qualified_procname [@@deriving compare]
end

module ImportSet = Caml.Set.Make (Import)

type class_info = {parent: Symbol.t option}

type label_info =
  { label_name: string
  ; ssa_parameters: T.Typ.t list
  ; prelude: T.Location.t -> t -> t
  ; processed: bool }

(** Part of the environment shared by most structures. It gathers information like which builtin has
    been spotted, or what idents and labels have been generated so far. *)
and shared =
  { idents: T.Ident.Set.t
  ; idents_info: info T.Ident.Map.t
  ; globals: Symbol.t SMap.t
  ; locals: Symbol.t SMap.t
  ; builtins: Builtin.Set.t
        (** All the builtins that have been called, so we only export them in textual to avoid too
            much noise *)
  ; classes: class_info SMap.t  (** All the classes that have been defined *)
  ; imports: ImportSet.t
  ; signatures: Signature.t SMap.t SMap.t
        (** Map from module names to the signature of all of their functions/methods *)
  ; module_name: string
  ; params: string list  (** Name of function / method parameters *)
  ; is_toplevel: bool
  ; is_static: bool (* is the current method a static method or an instance method ? *)
  ; next_label: int
  ; labels: label_info Labels.t }

(** State of the capture while processing a single node: each node has a dedicated data stack, and
    generates its own set of instructions. *)
and node = {stack: DataStack.t; instructions: T.Instr.t list; last_line: int option}

and t = {shared: shared; node: node}

let rec map ~f ~(env : t) = function
  | [] ->
      (env, [])
  | hd :: tl ->
      let env, hd = f env hd in
      let env, tl = map ~f ~env tl in
      (env, hd :: tl)


let mk_fresh_ident ({shared} as env) info =
  let mk_fresh_ident ({idents; idents_info} as env) =
    let fresh = T.Ident.fresh idents in
    let idents = T.Ident.Set.add fresh idents in
    let idents_info = T.Ident.Map.add fresh info idents_info in
    ({env with idents; idents_info}, fresh)
  in
  let shared, fresh = mk_fresh_ident shared in
  ({env with shared}, fresh)


let get_ident_info {shared= {idents_info}} id = T.Ident.Map.find_opt id idents_info

let push ({node} as env) cell =
  let push ({stack} as env) cell =
    let stack = DataStack.push stack cell in
    {env with stack}
  in
  let node = push node cell in
  {env with node}


let pop ({node} as env) =
  let pop ({stack} as env) =
    DataStack.pop stack |> Option.map ~f:(fun (stack, cell) -> ({env with stack}, cell))
  in
  pop node |> Option.map ~f:(fun (node, cell) -> ({env with node}, cell))


let peek {node= {stack}} = DataStack.peek stack

module Label = struct
  type info = label_info

  let mk ?(ssa_parameters = []) ?prelude label_name =
    let default _loc env = env in
    let prelude = Option.value prelude ~default in
    {label_name; ssa_parameters; prelude; processed= false}


  let update_ssa_parameters label_info ssa_parameters = {label_info with ssa_parameters}

  let is_processed {processed} = processed

  let name {label_name} = label_name

  let to_textual env label_loc {label_name; ssa_parameters; prelude} =
    let env, ssa_parameters =
      map ~env ssa_parameters ~f:(fun env typ ->
          let info =
            (* TODO: track code/class for SSA parameters *)
            {typ; is_code= false; is_class= false}
          in
          let env, id = mk_fresh_ident env info in
          (env, (id, typ)) )
    in
    (* Install the prelude before processing the instructions *)
    let env = prelude label_loc env in
    (* If we have ssa_parameters, we need to push them on the stack to restore its right shape.
       Doing a fold_right is important to keep the correct order. *)
    let env =
      List.fold_right ~init:env ssa_parameters ~f:(fun (id, _) env -> push env (DataStack.Temp id))
    in
    (env, label_name, ssa_parameters)
end

let empty_node = {stack= []; instructions= []; last_line= None}

let initial_globals =
  List.fold ~init:SMap.empty
    ~f:(fun acc value ->
      let global_info = Symbol.Builtin in
      SMap.add value global_info acc )
    (Builtin.Set.supported_builtins ())


let empty =
  { idents= T.Ident.Set.empty
  ; idents_info= T.Ident.Map.empty
  ; globals= initial_globals
  ; locals= SMap.empty
  ; builtins= Builtin.Set.empty
  ; classes= SMap.empty
  ; imports= ImportSet.empty
  ; signatures= SMap.empty
  ; module_name= ""
  ; params= []
  ; is_toplevel= true
  ; is_static= false
  ; next_label= 0
  ; labels= Labels.empty }


let empty = {shared= empty; node= empty_node}

let stack {node= {stack}} = stack

let enter_proc ~is_toplevel ~is_static ~module_name ~params {shared} =
  let shared =
    { shared with
      module_name
    ; params
    ; is_toplevel
    ; is_static
    ; idents= T.Ident.Set.empty
    ; next_label= 0
    ; locals= SMap.empty }
  in
  {shared; node= empty_node}


let enter_node ({node} as env) =
  let reset_for_node env = {env with instructions= []} in
  let node = reset_for_node node in
  {env with node}


let reset_stack ({node} as env) = {env with node= {node with stack= []}}

let update_last_line ({node} as env) last_line =
  let update_last_line node last_line =
    if Option.is_some last_line then {node with last_line} else node
  in
  {env with node= update_last_line node last_line}


let loc {node} =
  let loc {last_line} =
    last_line
    |> Option.value_map ~default:T.Location.Unknown ~f:(fun line -> T.Location.known ~line ~col:0)
  in
  loc node


let push_instr ({node} as env) instr =
  let push_instr ({instructions} as env) instr = {env with instructions= instr :: instructions} in
  {env with node= push_instr node instr}


let mk_fresh_label ({shared} as env) =
  let label ({next_label} as env) =
    let fresh_label = sprintf "b%d" next_label in
    let env = {env with next_label= next_label + 1} in
    (env, fresh_label)
  in
  let shared, fresh_label = label shared in
  let env = {env with shared} in
  (env, fresh_label)


let register_label ~offset label_info ({shared} as env) =
  let register_label offset label_info ({labels} as env) =
    let exists = Labels.mem offset labels in
    let exists = if exists then "existing" else "non existing" in
    if label_info.processed then Debug.p "processing %s label at %d\n" exists offset
    else Debug.p "registering %s label at %d\n" exists offset ;
    let labels = Labels.add offset label_info labels in
    {env with labels}
  in
  let shared = register_label offset label_info shared in
  {env with shared}


let process_label ~offset label_info env =
  let label_info = {label_info with processed= true} in
  register_label ~offset label_info env


let label_of_offset {shared} offset =
  let label_of_offset {labels} offset = Labels.find_opt offset labels in
  label_of_offset shared offset


let instructions {node= {instructions}} = List.rev instructions

let register_symbol ({shared} as env) ~global name symbol_info =
  PyDebug.p "[register_symbol] %b %s %a\n" global name Symbol.pp symbol_info ;
  let register map name symbol_info =
    let exists = SMap.mem name map in
    let map = SMap.add name symbol_info map in
    (exists, map)
  in
  let {globals; locals} = shared in
  if global then
    let exists, globals = register globals name symbol_info in
    let shared = {shared with globals} in
    (exists, {env with shared})
  else
    let exists, locals = register locals name symbol_info in
    let shared = {shared with locals} in
    (exists, {env with shared})


let lookup_symbol {shared} ~global name =
  PyDebug.p "[lookup_symbol] %b %s\n" global name ;
  let lookup map name = SMap.find_opt name map in
  let {globals; locals} = shared in
  let res = if global then lookup globals name else lookup locals name in
  PyDebug.p "> %a\n" (Pp.option Symbol.pp) res ;
  res


let globals {shared= {globals}} = globals

let get_used_builtins {shared= {builtins}} = builtins

let register_builtin ({shared} as env) builtin =
  PyDebug.p "[register_builtin] %a\n" T.pp_qualified_procname (Builtin.to_proc_name builtin) ;
  let register_builtin ({builtins} as env) builtin =
    {env with builtins= Builtin.Set.register builtins builtin}
  in
  {env with shared= register_builtin shared builtin}


let mk_builtin_call env builtin args =
  let textual_builtin = Builtin.textual builtin in
  let typ =
    (* Special casing to make the type of new instances more precise *)
    match (builtin, args) with
    | Builtin.PythonClassConstructor, T.Exp.Const (T.Const.Str arg) :: _ ->
        (* TODO: how can we track the loc ? via args ?*)
        let type_name = type_name arg in
        T.Typ.Ptr (T.Typ.Struct type_name)
    | _, _ ->
        Builtin.Set.get_type textual_builtin
  in
  let info = {typ; is_class= false; is_code= false} in
  let env = register_builtin env textual_builtin in
  let env, id = mk_fresh_ident env info in
  let proc = Builtin.to_proc_name textual_builtin in
  let exp = T.Exp.Call {proc; args; kind= T.Exp.NonVirtual} in
  let loc = loc env in
  let instr = T.Instr.Let {id; exp; loc} in
  let env = push_instr env instr in
  (env, id, typ)


let is_builtin {shared= {globals}} fname =
  match SMap.find_opt fname globals with
  | Some Symbol.Builtin ->
      true
  | None | Some (Name _ | Class _ | Code _ | Import _) ->
      false


let register_call env fname =
  PyDebug.p "[register_call] %s\n" fname ;
  match (is_builtin env fname, Builtin.of_string fname) with
  | true, Some builtin ->
      register_builtin env builtin
  | _, _ ->
      env


let register_method ({shared} as env) ~enclosing_class ~method_name annotations =
  PyDebug.p "[register_method] %s.%s" enclosing_class method_name ;
  let {signatures} = shared in
  let class_info = SMap.find_opt enclosing_class signatures |> Option.value ~default:SMap.empty in
  let class_info = SMap.add method_name annotations class_info in
  let signatures = SMap.add enclosing_class class_info signatures in
  let shared = {shared with signatures} in
  {env with shared}


let register_function ({shared} as env) name loc annotations =
  PyDebug.p "[register_function] %s" name ;
  let {module_name} = shared in
  let info = {Signature.is_static= false; annotations} in
  let env = register_method env ~enclosing_class:module_name ~method_name:name info in
  let code_name = Symbol.Qualified.mk ~prefix:[module_name] name loc in
  let symbol_info = Symbol.Code {code_name} in
  snd @@ register_symbol env ~global:true name symbol_info


let lookup_method {shared= {signatures}} ~enclosing_class name =
  let open Option.Let_syntax in
  SMap.find_opt enclosing_class signatures >>= SMap.find_opt name


let register_class ({shared} as env) class_name ({parent} as class_info) =
  PyDebug.p "[register_class] %s" class_name ;
  ( match parent with
  | None ->
      PyDebug.p "\n"
  | Some parent ->
      PyDebug.p " extending %a" Symbol.pp parent ) ;
  let {classes} = shared in
  let classes = SMap.add class_name class_info classes in
  let shared = {shared with classes} in
  {env with shared}


let register_import ({shared} as env) import_name =
  let {imports} = shared in
  let imports = ImportSet.add import_name imports in
  let shared = {shared with imports} in
  {env with shared}


let get_declared_classes {shared= {classes}} = classes

(* TODO: rethink that when adding more support for imports, probably changing it into a
   "lookup_import" version *)
let get_textual_imports {shared= {imports}} =
  let result_type = {T.Typ.typ= PyCommon.pyObject; attributes= []} in
  ImportSet.fold
    (fun import acc ->
      match (import : Import.t) with
      | TopLevel import_name ->
          let enclosing_class = type_name import_name in
          let qualified_name =
            PyCommon.qualified_procname ~enclosing_class
            @@ PyCommon.proc_name PyCommon.toplevel_function
          in
          T.Module.Procdecl {qualified_name; formals_types= Some []; result_type; attributes= []}
          :: acc
      | Call qualified_name ->
          T.Module.Procdecl {qualified_name; formals_types= None; result_type; attributes= []}
          :: acc )
    imports []


let is_toplevel {shared= {is_toplevel}} = is_toplevel

let is_static {shared= {is_static}} = is_static

let get_params {shared= {params}} = params

let module_name {shared= {module_name}} = module_name
