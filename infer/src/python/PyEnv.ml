(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module PyBuiltins = PyCommon.Builtins
module Debug = PyDebug

module DataStack = struct
  type cell =
    | Const of int
    | Name of int
    | VarName of int
    | Temp of T.Ident.t
    | Fun of (string * FFI.Code.t)
  [@@deriving show]

  let as_code FFI.Code.{co_consts} = function
    | Const n ->
        let code = co_consts.(n) in
        FFI.Constant.as_code code
    | Fun (_, code) ->
        Some code
    | Name _ | Temp _ | VarName _ ->
        None


  type t = cell list

  let push stack cell = cell :: stack

  let pop = function [] -> None | hd :: stack -> Some (stack, hd)
end

module Labels = Caml.Map.Make (Int)

type global_info = {is_code: bool}

type label_info =
  { label_name: string
  ; ssa_parameters: T.Typ.t list
  ; prelude: T.Location.t -> t -> t
  ; processed: bool }

and shared =
  { idents: T.Ident.Set.t
  ; globals: global_info T.VarName.Map.t
  ; builtins: PyBuiltins.t
  ; next_label: int
  ; labels: label_info Labels.t }
(* TODO(vsiles): revisit the data stack status once generators are in the mix *)

and node = {stack: DataStack.t; instructions: T.Instr.t list; last_line: int option}

and t = {shared: shared; node: node}

let empty_node = {stack= []; instructions= []; last_line= None}

let empty =
  { idents= T.Ident.Set.empty
  ; globals= T.VarName.Map.empty
  ; builtins= PyBuiltins.empty
  ; next_label= 0
  ; labels= Labels.empty }


let mk_label_info ?(ssa_parameters = []) ?prelude label_name =
  let default _loc env = env in
  let prelude = Option.value prelude ~default in
  {label_name; ssa_parameters; prelude; processed= false}


let stack {node= {stack}} = stack

let rec map ~f ~(env : t) = function
  | [] ->
      (env, [])
  | hd :: tl ->
      let env, hd = f env hd in
      let env, tl = map ~f ~env tl in
      (env, hd :: tl)


let reset_for_proc shared =
  let shared = {shared with idents= T.Ident.Set.empty; next_label= 0} in
  {shared; node= empty_node}


let reset_for_node ({node} as env) =
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


let mk_fresh_ident ({shared} as env) =
  let mk_fresh_ident ({idents} as env) =
    let fresh = T.Ident.fresh idents in
    let idents = T.Ident.Set.add fresh idents in
    ({env with idents}, fresh)
  in
  let shared, fresh = mk_fresh_ident shared in
  ({env with shared}, fresh)


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


let register_label offset label_info ({shared} as env) =
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


(** Check if the instruction is a possible jump location, and return the label information found
    there, if any. *)
let label_of_offset {shared} offset =
  let label_of_offset {labels} offset = Labels.find_opt offset labels in
  label_of_offset shared offset


(** Returns the list of all instructions recorded for the current code unit *)
let get_instructions {node= {instructions}} = List.rev instructions

(** Register a global name (function, variable, ...). Since Python allows "toplevel" code, they are
    encoded within a specially named function that behaves as a toplevel scope, and global
    identifiers are scope accordingly. That way, there is no mixing them with locals with the same
    name. *)
let register_global ({shared} as env) name is_code =
  let register_global ({globals} as env) name is_code =
    {env with globals= T.VarName.Map.add name is_code globals}
  in
  {env with shared= register_global shared name is_code}


(** Return the [globals] map *)
let globals {shared= {globals}} = globals

(** Register a known builtin, so they are correctly scoped, and add the relevant Textual
    declarations for them. *)
let register_builtin ({shared} as env) name =
  let register_builtin ({builtins} as env) name =
    {env with builtins= PyBuiltins.register builtins name}
  in
  {env with shared= register_builtin shared name}
