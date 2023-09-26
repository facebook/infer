(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module T = Textual
module Debug = PyDebug
module Builtin = PyBuiltin
module Ident = PyCommon.Ident
module Labels = Caml.Map.Make (Int)
module ISet = Caml.Set.Make (Int)

module Signature = struct
  type t = {annotations: PyCommon.signature; is_static: bool; is_abstract: bool}

  let pp fmt {annotations; is_static} =
    Format.fprintf fmt "MethodSignature%s : %a"
      (if is_static then "(static)" else "")
      PyCommon.pp_signature annotations
end

module SMap = PyCommon.SMap

module Info = struct
  type kind = Code | Class | Other

  type t = {kind: kind; typ: T.Typ.t}

  let default typ = {kind= Other; typ}

  let is_code = function Code -> true | Class | Other -> false
end

module Symbol = struct
  type kind =
    | Name of {is_imported: bool; typ: T.Typ.t}
    | Builtin
    | Code
    | Class
    | ImportCall
    | Import of {more_than_once: bool}

  let pp_kind fmt = function
    | Name {typ} ->
        Format.fprintf fmt "Name(%a)" T.Typ.pp typ
    | Builtin ->
        Format.pp_print_string fmt "Builtin"
    | Code ->
        Format.pp_print_string fmt "Code"
    | Class ->
        Format.pp_print_string fmt "Class"
    | Import {more_than_once} ->
        Format.fprintf fmt "Import(%b)" more_than_once
    | ImportCall ->
        Format.pp_print_string fmt "ImportCall"


  type t = {id: Ident.t; kind: kind; loc: T.Location.t}

  type key = Global of Ident.t | Local of string

  let pp_key fmt = function
    | Global id ->
        Format.fprintf fmt "global %a" Ident.pp id
    | Local local_id ->
        Format.pp_print_string fmt local_id


  let pp fmt {id; kind} = Format.fprintf fmt "%a[%a]" Ident.pp id pp_kind kind
end

module DataStack = struct
  type cell =
    | Const of int
    | Name of {global: bool; ndx: int}
    | VarName of int
    | Temp of T.Ident.t
    | Code of {fun_or_class: bool; code_name: string; code: FFI.Code.t}
    | Map of (T.Exp.t * cell) list
    | BuiltinBuildClass
    | Import of {import_path: Ident.t; symbols: string list}
    | ImportCall of {id: Ident.t; loc: T.Location.t}
    | MethodCall of {receiver: T.Exp.t; name: T.qualified_procname}
    | StaticCall of {call_name: T.qualified_procname; receiver: T.Exp.t option}
    | Super
    | Path of Ident.t
    | WithContext of
        T.Ident.t (* see https://docs.python.org/3.8/reference/compound_stmts.html#with *)
    | NoException

  let pp_cell fmt = function
    | Const n ->
        F.fprintf fmt "Const(%d)" n
    | Name {global; ndx} ->
        F.fprintf fmt "Name(%s, %d)" (if global then "global" else "local") ndx
    | VarName n ->
        F.fprintf fmt "VarName(%d)" n
    | Temp id ->
        F.fprintf fmt "Temp(%a)" T.Ident.pp id
    | Code {fun_or_class; code_name} ->
        F.fprintf fmt "Code(%s, %s)" (if fun_or_class then "fun" else "class") code_name
    | Map _ ->
        F.pp_print_string fmt "Map"
    | BuiltinBuildClass ->
        F.pp_print_string fmt "LOAD_BUILD_CLASS"
    | Import {import_path; symbols= []} ->
        F.fprintf fmt "Import(%a)" Ident.pp import_path
    | Import {import_path; symbols} ->
        F.fprintf fmt "ImportFrom(%a, %a)" Ident.pp import_path
          (Pp.seq ~sep:" " F.pp_print_string)
          symbols
    | ImportCall {id} ->
        F.fprintf fmt "ImportCall(%a)" Ident.pp id
    | MethodCall {receiver; name} ->
        F.fprintf fmt "MethodCall(%a, %a)" T.Exp.pp receiver T.pp_qualified_procname name
    | StaticCall {call_name; receiver} ->
        F.fprintf fmt "StaticCall(%a, %a)" T.pp_qualified_procname call_name (Pp.option T.Exp.pp)
          receiver
    | Super ->
        F.pp_print_string fmt "Super"
    | Path id ->
        F.fprintf fmt "Path(%a)" Ident.pp id
    | WithContext id ->
        F.fprintf fmt "WithContext(%a)" T.Ident.pp id
    | NoException ->
        F.pp_print_string fmt "NoExpception"


  let as_code FFI.Code.{co_consts} = function
    | Const n ->
        let code = co_consts.(n) in
        FFI.Constant.as_code code
    | Code {code} ->
        Some code
    | Path _
    | Name _
    | Temp _
    | VarName _
    | Map _
    | BuiltinBuildClass
    | Import _
    | ImportCall _
    | MethodCall _
    | StaticCall _
    | Super
    | WithContext _
    | NoException ->
        None


  let as_name FFI.Code.{co_varnames; co_names; co_consts} = function
    | Const ndx ->
        let cst = co_consts.(ndx) in
        FFI.Constant.as_name cst
    | Name {ndx} ->
        Some co_names.(ndx)
    | VarName ndx ->
        Some co_varnames.(ndx)
    | Path _
    | Code _
    | Temp _
    | Map _
    | BuiltinBuildClass
    | Import _
    | ImportCall _
    | MethodCall _
    | StaticCall _
    | Super
    | WithContext _
    | NoException ->
        None


  let as_id code cell =
    match cell with
    | Const _ ->
        Option.map ~f:Ident.mk (as_name code cell)
    | Name {global} ->
        Option.map ~f:(Ident.mk ~global) (as_name code cell)
    | VarName _ ->
        Option.map ~f:(Ident.mk ~global:false) (as_name code cell)
    | Path id ->
        Some id
    | Code _
    | Temp _
    | Map _
    | BuiltinBuildClass
    | Import _
    | ImportCall _
    | MethodCall _
    | StaticCall _
    | Super
    | WithContext _
    | NoException ->
        None


  let is_path = function Path _ -> true | _ -> false

  let is_no_exception = function NoException -> true | _ -> false

  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)

  let peek = function [] -> None | hd :: _ -> Some hd
end

(* TODO: try to merge this in a [globals]/[Symbol]. However be careful not to override the
   original parent info since it might not be locally available when we executer [register_symbol]
   after [register_class] *)
type class_info = {parents: Ident.t list}

type label_info =
  {label_name: string; ssa_parameters: T.Typ.t list; prelude: prelude option; processed: bool}

and prelude = T.Location.t -> t -> t

(** Part of the environment shared by most structures. It gathers information like which builtin has
    been spotted, or what idents and labels have been generated so far. *)
and shared =
  { local_idents: T.Ident.Set.t
  ; local_idents_info: Info.t T.Ident.Map.t
  ; globals: Symbol.t Ident.Map.t
        (** Information about global symbols (function, classes, imports, global variables, ... *)
  ; locals: Symbol.t SMap.t
  ; classes: class_info SMap.t  (** Class level info *)
  ; builtins: Builtin.Set.t
        (** All the builtins that have been called, so we only export them in textual to avoid too
            much noise *)
  ; imported_values: Ident.Set.t
  ; signatures: Signature.t SMap.t Ident.Map.t
        (** Map from module names to the signature of all of their functions/methods *)
  ; fields: PyCommon.signature T.TypeName.Map.t
        (** Map from fully qualified class name to the list of known fields and their types *)
  ; module_name: Ident.t
  ; params: string list  (** Name of function / method parameters *)
  ; is_toplevel: bool
  ; is_static: bool (* is the current method a static method or an instance method ? *)
  ; next_label: int
  ; with_targets: ISet.t (* targets for clean up code of [with] statements or [finally] blocks *)
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


let map_result ~f ~(env : t) l =
  let open IResult.Let_syntax in
  let rec aux acc l =
    match (acc, l) with
    | Error s, _ ->
        Error s
    | Ok env, [] ->
        Ok (env, [])
    | Ok env, hd :: tl ->
        let* env, hd = f env hd in
        let* env, tl = aux (Ok env) tl in
        Ok (env, hd :: tl)
  in
  aux (Ok env) l


let mk_fresh_ident ({shared} as env) info =
  let mk_fresh_ident ({local_idents; local_idents_info} as env) =
    let fresh = T.Ident.fresh local_idents in
    let local_idents = T.Ident.Set.add fresh local_idents in
    let local_idents_info = T.Ident.Map.add fresh info local_idents_info in
    ({env with local_idents; local_idents_info}, fresh)
  in
  let shared, fresh = mk_fresh_ident shared in
  ({env with shared}, fresh)


let get_ident_info {shared= {local_idents_info}} id = T.Ident.Map.find_opt id local_idents_info

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
    {label_name; ssa_parameters; prelude; processed= false}


  let update_ssa_parameters label_info ssa_parameters = {label_info with ssa_parameters}

  let is_processed {processed} = processed

  let name {label_name} = label_name

  let to_textual env label_loc {label_name; ssa_parameters; prelude} =
    let env, ssa_parameters =
      map ~env ssa_parameters ~f:(fun env typ ->
          let info =
            (* TODO: track code/class for SSA parameters *)
            {Info.kind= Other; typ}
          in
          let env, id = mk_fresh_ident env info in
          (env, (id, typ)) )
    in
    (* Install the prelude before processing the instructions *)
    let env = match prelude with Some action -> action label_loc env | None -> env in
    (* If we have ssa_parameters, we need to push them on the stack to restore its right shape.
       Doing a fold_right is important to keep the correct order. *)
    let env =
      List.fold_right ~init:env ssa_parameters ~f:(fun (id, _) env -> push env (DataStack.Temp id))
    in
    (env, label_name, ssa_parameters)
end

let empty_node = {stack= []; instructions= []; last_line= None}

let initial_globals =
  let loc = T.Location.Unknown in
  List.fold ~init:Ident.Map.empty
    ~f:(fun acc value ->
      let key = Ident.mk value in
      let id = Ident.mk_builtin value in
      let global_info = {Symbol.id; loc; kind= Builtin} in
      Ident.Map.add key global_info acc )
    (Builtin.Set.supported_builtins ())


let empty module_name =
  { local_idents= T.Ident.Set.empty
  ; local_idents_info= T.Ident.Map.empty
  ; globals= initial_globals
  ; locals= SMap.empty
  ; classes= SMap.empty
  ; builtins= Builtin.Set.empty
  ; imported_values= Ident.Set.empty
  ; signatures= Ident.Map.empty
  ; fields= T.TypeName.Map.empty
  ; module_name
  ; params= []
  ; is_toplevel= true
  ; is_static= false
  ; next_label= 0
  ; with_targets= ISet.empty
  ; labels= Labels.empty }


let empty module_name = {shared= empty module_name; node= empty_node}

let stack {node= {stack}} = stack

let enter_proc ~is_toplevel ~is_static ~module_name ~params {shared} =
  Debug.p "[enter_proc] %a\n" Ident.pp module_name ;
  let shared =
    { shared with
      module_name
    ; params
    ; is_toplevel
    ; is_static
    ; local_idents= T.Ident.Set.empty
    ; next_label= 0
    ; labels= Labels.empty
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


let register_label ~offset ({label_name; ssa_parameters} as label_info) ({shared} as env) =
  Debug.p "[register_label] %s at offset %d (arity %d)\n" label_name offset
    (List.length ssa_parameters) ;
  let register_label offset label_info ({labels} as env) =
    let labels =
      match Labels.find_opt offset labels with
      | None ->
          Labels.add offset label_info labels
      | Some old_info ->
          (* Sanity check: at the moment we *never* have to combine preludes, we do this using
             the Textual [If] terminator *)
          let {prelude= old_prelude} = old_info in
          let {prelude= new_prelude} = label_info in
          if Option.is_some old_prelude && Option.is_some new_prelude then
            L.die InternalError "register_label: failure to combine preludes@\n" ;
          labels
    in
    {env with labels}
  in
  let shared = register_label offset label_info shared in
  {env with shared}


let register_with_target ~offset ({shared} as env) =
  Debug.p "[register_with_target] at offset %d\n" offset ;
  let {with_targets} = shared in
  let with_targets = ISet.add offset with_targets in
  let shared = {shared with with_targets} in
  {env with shared}


let is_with_target ~offset {shared} =
  Debug.p "[is_with_target] %d ?\n" offset ;
  let {with_targets} = shared in
  ISet.mem offset with_targets


let process_label ~offset ({label_name} as label_info) ({shared} as env) =
  Debug.p "[process_label] %s at %d\n" label_name offset ;
  let label_info = {label_info with processed= true} in
  let process_label ({labels} as env) =
    let labels = Labels.add offset label_info labels in
    {env with labels}
  in
  let shared = process_label shared in
  {env with shared}


let label_of_offset {shared} offset =
  let label_of_offset {labels} offset = Labels.find_opt offset labels in
  label_of_offset shared offset


let instructions {node= {instructions}} = List.rev instructions

let register_symbol ({shared} as env) key symbol_info =
  PyDebug.p "[register_symbol] %a %a\n" Symbol.pp_key key Symbol.pp symbol_info ;
  let {globals; locals} = shared in
  match key with
  | Symbol.Global id ->
      let globals = Ident.Map.add id symbol_info globals in
      let shared = {shared with globals} in
      {env with shared}
  | Symbol.Local local_id ->
      let locals = SMap.add local_id symbol_info locals in
      let shared = {shared with locals} in
      {env with shared}


let lookup_symbol {shared} key =
  PyDebug.p "[lookup_symbol] %a\n" Symbol.pp_key key ;
  let {globals; locals} = shared in
  let res =
    match key with
    | Symbol.Global id ->
        Ident.Map.find_opt id globals
    | Symbol.Local local_id ->
        SMap.find_opt local_id locals
  in
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
  let typ = Builtin.Set.get_type textual_builtin in
  let info = {Info.kind= Other; typ} in
  let env = register_builtin env textual_builtin in
  let env, id = mk_fresh_ident env info in
  let proc = Builtin.to_proc_name textual_builtin in
  let exp = T.Exp.Call {proc; args; kind= T.Exp.NonVirtual} in
  let loc = loc env in
  let instr = T.Instr.Let {id; exp; loc} in
  let env = push_instr env instr in
  (env, id, typ)


let is_builtin {shared= {globals}} fid =
  match Ident.Map.find_opt fid globals with
  | Some {Symbol.kind= Builtin} ->
      Some (Ident.to_string ~sep:"." fid)
  | _ ->
      None


let register_call env fid =
  PyDebug.p "[register_call] %a\n" Ident.pp fid ;
  match is_builtin env fid with
  | Some fname -> (
    match Builtin.of_string fname with Some builtin -> register_builtin env builtin | None -> env )
  | None ->
      env


let register_method ({shared} as env) ~enclosing_class ~method_name annotations =
  PyDebug.p "[register_method] %a.%s\n" Ident.pp enclosing_class method_name ;
  PyDebug.p "                  %a\n" Signature.pp annotations ;
  let {signatures} = shared in
  let class_info =
    Ident.Map.find_opt enclosing_class signatures |> Option.value ~default:SMap.empty
  in
  let class_info = SMap.add method_name annotations class_info in
  let signatures = Ident.Map.add enclosing_class class_info signatures in
  let shared = {shared with signatures} in
  {env with shared}


(* We keep the fields sorted to be able to use List.merge *)
let register_fields ({shared} as env) class_name class_fields =
  (* FYI doc says:
     If several elements compare equal, the elements of l1 will be before the elements of l2.

     TODO(vsiles)
     Right now we only register once, but we might try to capture more field
     when an explicit [__init__] is present, and then the order will matter.
  *)
  let compare : PyCommon.annotated_name -> PyCommon.annotated_name -> int =
   fun {PyCommon.name= n0} {PyCommon.name= n1} -> String.compare n0 n1
  in
  let {fields} = shared in
  let old_fields = T.TypeName.Map.find_opt class_name fields |> Option.value ~default:[] in
  let new_fields = List.sort ~compare class_fields in
  let new_fields = List.merge ~compare old_fields new_fields in
  let new_fields = List.dedup_and_sort ~compare new_fields in
  let fields = T.TypeName.Map.add class_name new_fields fields in
  let shared = {shared with fields} in
  {env with shared}


let register_function ({shared} as env) fname loc annotations =
  PyDebug.p "[register_function] %s\n" fname ;
  let {module_name} = shared in
  let info = {Signature.is_static= false; is_abstract= false; annotations} in
  let env = register_method env ~enclosing_class:module_name ~method_name:fname info in
  let key = Ident.mk ~loc fname in
  let id = Ident.extend ~prefix:module_name fname in
  let symbol_info = {Symbol.kind= Code; id; loc} in
  register_symbol env (Symbol.Global key) symbol_info


let lookup_method {shared= {signatures}} ~enclosing_class name =
  let open Option.Let_syntax in
  Ident.Map.find_opt enclosing_class signatures >>= SMap.find_opt name


let lookup_fields {shared= {fields}} class_name = T.TypeName.Map.find_opt class_name fields

let register_class ({shared} as env) class_name parents =
  PyDebug.p "[register_class] %s\n" class_name ;
  ( match parents with
  | [] ->
      PyDebug.p "\n"
  | _ :: _ ->
      PyDebug.p " extending %a\n" (Pp.seq ~sep:", " Ident.pp) parents ) ;
  let {classes} = shared in
  let classes = SMap.add class_name {parents} classes in
  let shared = {shared with classes} in
  {env with shared}


let get_declared_classes {shared= {classes}} = classes

let register_imported_value ({shared} as env) value =
  let {imported_values} = shared in
  let imported_values = Ident.Set.add value imported_values in
  let shared = {shared with imported_values} in
  {env with shared}


(* TODO: rethink that when adding more support for imports, probably changing it into a
   "lookup_import" version *)
let get_textual_imports {shared= {globals; imported_values}} =
  let result_type = {T.Typ.typ= PyCommon.pyObject; attributes= []} in
  let calls =
    Ident.Map.fold
      (fun _key {Symbol.id; kind} acc ->
        match kind with
        | Symbol.ImportCall ->
            let qualified_name = Ident.to_qualified_procname id in
            let procdecl =
              {T.ProcDecl.qualified_name; formals_types= None; result_type; attributes= []}
            in
            T.Module.Procdecl procdecl :: acc
        | Symbol.Import _ ->
            let enclosing_class = Ident.to_type_name id in
            let qualified_name =
              PyCommon.qualified_procname ~enclosing_class
              @@ PyCommon.proc_name PyCommon.toplevel_function
            in
            let procdecl =
              {T.ProcDecl.qualified_name; formals_types= Some []; result_type; attributes= []}
            in
            T.Module.Procdecl procdecl :: acc
        | _ ->
            acc )
      globals []
  in
  Ident.Set.fold
    (fun id acc ->
      let var_name = Ident.to_var_name id in
      let global_var = {T.Global.name= var_name; typ= PyCommon.pyObject; attributes= []} in
      T.Module.Global global_var :: acc )
    imported_values calls


let is_toplevel {shared= {is_toplevel}} = is_toplevel

let is_static {shared= {is_static}} = is_static

let get_params {shared= {params}} = params

let module_name {shared= {module_name}} = module_name
