(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module Debug = PyDebug

module Builtin = struct
  type primitive = PythonInt | PythonBool | PythonString | PythonTuple [@@deriving compare]

  type textual =
    | IsTrue
    | BinaryAdd
    | PythonCall
    | PythonClass
    | PythonClassConstructor
    | PythonCode
    | PythonIter
    | PythonIterItem
    | PythonIterNext
  [@@deriving compare]

  type python = Print | Range [@@deriving compare]

  type t = Primitive of primitive | Textual of textual | Python of python [@@deriving compare]

  let to_proc_name = function
    | Primitive primitive -> (
      match primitive with
      | PythonInt ->
          PyCommon.python_int
      | PythonBool ->
          PyCommon.python_bool
      | PythonString ->
          PyCommon.python_string
      | PythonTuple ->
          PyCommon.python_tuple )
    | Textual textual ->
        let str =
          match textual with
          | IsTrue ->
              "python_is_true"
          | BinaryAdd ->
              "binary_add"
          | PythonCall ->
              "python_call"
          | PythonClass ->
              "python_class"
          | PythonClassConstructor ->
              "python_class_constructor"
          | PythonCode ->
              "python_code"
          | PythonIter ->
              "python_iter"
          | PythonIterItem ->
              "python_iter_item"
          | PythonIterNext ->
              "python_iter_next"
        in
        PyCommon.builtin_name str
    | Python python ->
        let str = match python with Print -> "print" | Range -> "range" in
        PyCommon.builtin_name str


  (** Lookup a [Python] builtin from its name *)
  let of_string name =
    match name with "print" -> Some (Python Print) | "range" -> Some (Python Range) | _ -> None
end

module BuiltinSet = struct
  let type_name value = T.TypeName.{value; loc= T.Location.Unknown}

  let string_ = T.Typ.(Ptr (Struct (type_name "String")))

  type elt = {formals_types: T.Typ.annotated list option; result_type: T.Typ.annotated}

  module Info = Caml.Map.Make (Builtin)
  module Set = Caml.Set.Make (Builtin)

  let mk_builtin {formals_types; result_type} builtin =
    let qualified_name = Builtin.to_proc_name builtin in
    T.Module.Procdecl T.ProcDecl.{qualified_name; formals_types; result_type; attributes= []}


  let annot typ = T.Typ.{typ; attributes= []}

  type t = Set.t

  let primitive_builtins =
    let builtins =
      [ ( Builtin.PythonInt
        , {formals_types= Some [annot T.Typ.Int]; result_type= annot PyCommon.pyInt} )
      ; ( Builtin.PythonBool
        , {formals_types= Some [annot T.Typ.Int]; result_type= annot PyCommon.pyBool} )
      ; ( Builtin.PythonString
        , {formals_types= Some [annot string_]; result_type= annot PyCommon.pyString} )
      ; (Builtin.PythonTuple, {formals_types= None; result_type= annot PyCommon.pyObject}) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Primitive builtin) elt acc)
      ~init:Info.empty builtins


  let textual_builtins =
    let builtins =
      [ ( Builtin.IsTrue
        , {formals_types= Some [annot PyCommon.pyObject]; result_type= annot T.Typ.Int} )
      ; ( Builtin.BinaryAdd
        , { formals_types= Some [annot PyCommon.pyObject; annot PyCommon.pyObject]
          ; result_type= annot PyCommon.pyObject } )
      ; (Builtin.PythonCall, {formals_types= None; result_type= annot PyCommon.pyObject})
      ; ( Builtin.PythonClass
        , {formals_types= Some [annot string_]; result_type= annot PyCommon.pyClass} )
      ; ( Builtin.PythonClassConstructor
          (* Class constructors can be implicitly inherited, so we are never sure of their
             arity. Also, we'll override their return type when we setup the call, to make it
             more precise. *)
        , {formals_types= None; result_type= annot PyCommon.pyObject} )
      ; ( Builtin.PythonCode
        , {formals_types= Some [annot string_]; result_type= annot PyCommon.pyCode} )
        (* TODO: should we introduce a Textual type for iterators ? *)
      ; ( Builtin.PythonIter
        , {formals_types= Some [annot PyCommon.pyObject]; result_type= annot PyCommon.pyObject} )
      ; ( Builtin.PythonIterItem
        , {formals_types= Some [annot PyCommon.pyObject]; result_type= annot PyCommon.pyObject} )
      ; ( Builtin.PythonIterNext
        , {formals_types= Some [annot PyCommon.pyObject]; result_type= annot T.Typ.Int} ) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Textual builtin) elt acc)
      ~init:primitive_builtins builtins


  let supported_builtins =
    let builtins =
      [ (Builtin.Print, {formals_types= None; result_type= annot PyCommon.pyObject})
      ; (Builtin.Range, {formals_types= None; result_type= annot PyCommon.pyObject}) ]
    in
    List.fold_left
      ~f:(fun acc (builtin, elt) -> Info.add (Builtin.Python builtin) elt acc)
      ~init:textual_builtins builtins


  let to_textual spotted =
    let init = Info.fold (fun key elt l -> mk_builtin elt key :: l) primitive_builtins [] in
    Info.fold
      (fun key elt l -> if Set.mem key spotted then mk_builtin elt key :: l else l)
      supported_builtins init


  let register spotted name = Set.add name spotted

  let get_type builtin =
    let info = Info.find_opt builtin supported_builtins in
    Option.map info ~f:(fun b -> b.result_type.typ) |> Option.value ~default:PyCommon.pyObject


  let empty = Set.empty
end

module DataStack = struct
  type cell =
    | Const of int
    | Name of int
    | VarName of int
    | Temp of T.Ident.t
    | Code of {fun_or_class: bool; qualified_name: string; code: FFI.Code.t}
    | Map of (string * cell) list
    | BuiltinBuildClass
  [@@deriving show]

  let as_code FFI.Code.{co_consts} = function
    | Const n ->
        let code = co_consts.(n) in
        FFI.Constant.as_code code
    | Code {code} ->
        Some code
    | Name _ | Temp _ | VarName _ | Map _ | BuiltinBuildClass ->
        None


  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)
end

module Labels = Caml.Map.Make (Int)

module Signature = struct
  type t = (string * string) list

  module Map = Caml.Map.Make (String)
end

type info = {is_code: bool; is_class: bool; typ: T.Typ.t}

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
  ; globals: info T.VarName.Map.t
  ; shadowed_builtins: BuiltinSet.t
  ; builtins: BuiltinSet.t
  ; classes: string list
  ; toplevel_signatures: Signature.t Signature.Map.t
  ; is_toplevel: bool
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

let empty =
  { idents= T.Ident.Set.empty
  ; idents_info= T.Ident.Map.empty
  ; globals= T.VarName.Map.empty
  ; shadowed_builtins= BuiltinSet.empty
  ; builtins= BuiltinSet.empty
  ; classes= []
  ; toplevel_signatures= Signature.Map.empty
  ; is_toplevel= true
  ; next_label= 0
  ; labels= Labels.empty }


let empty = {shared= empty; node= empty_node}

let stack {node= {stack}} = stack

let enter_proc ~is_toplevel {shared} =
  let shared = {shared with is_toplevel; idents= T.Ident.Set.empty; next_label= 0} in
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
    |> Option.map ~f:(fun line -> T.Location.known ~line ~col:0)
    |> Option.value ~default:T.Location.Unknown
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

let register_global ({shared} as env) name is_code =
  let register_global ({globals} as env) name is_code =
    {env with globals= T.VarName.Map.add name is_code globals}
  in
  {env with shared= register_global shared name is_code}


let globals {shared= {globals}} = globals

let builtins {shared= {builtins}} = builtins

let register_builtin ({shared} as env) builtin =
  let register_builtin ({builtins} as env) builtin =
    {env with builtins= BuiltinSet.register builtins builtin}
  in
  {env with shared= register_builtin shared builtin}


let mk_builtin_call env builtin args =
  let textual_builtin = Builtin.Textual builtin in
  let typ =
    (* Special casing to make the type of new instances more precise *)
    match (builtin, args) with
    | Builtin.PythonClassConstructor, T.Exp.Const (T.Const.Str arg) :: _ ->
        (* TODO: how can we track the loc ? via args ?*)
        let type_name = {T.TypeName.value= arg; loc= T.Location.Unknown} in
        T.Typ.Ptr (T.Typ.Struct type_name)
    | _, _ ->
        BuiltinSet.get_type textual_builtin
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


let is_builtin_shadowed {shared= {shadowed_builtins}} python_builtin =
  BuiltinSet.Set.mem python_builtin shadowed_builtins


let get_as_builtin env fname =
  let open Option.Monad_infix in
  Builtin.of_string fname
  >>= fun python_builtin ->
  Option.some_if (not @@ is_builtin_shadowed env python_builtin) python_builtin


let is_builtin env fname = get_as_builtin env fname |> Option.is_some

let register_call env fname =
  match get_as_builtin env fname with None -> env | Some builtin -> register_builtin env builtin


let register_toplevel ({shared} as env) fname annotations =
  let builtin = Builtin.of_string fname in
  match builtin with
  | None ->
      let fname = PyCommon.global fname in
      let {toplevel_signatures} = shared in
      let toplevel_signatures = Signature.Map.add fname annotations toplevel_signatures in
      let shared = {shared with toplevel_signatures} in
      {env with shared}
  | Some builtin ->
      let {shadowed_builtins} = shared in
      let shadowed_builtins = BuiltinSet.Set.add builtin shadowed_builtins in
      let shared = {shared with shadowed_builtins} in
      {env with shared}


let lookup_signature {shared= {toplevel_signatures}} {T.ProcName.value} =
  Signature.Map.find_opt value toplevel_signatures


let register_class ({shared} as env) class_name =
  let {classes} = shared in
  let classes = class_name :: classes in
  let shared = {shared with classes} in
  {env with shared}


let get_classes {shared= {classes}} = classes

let is_toplevel {shared= {is_toplevel}} = is_toplevel
