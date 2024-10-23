(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(*module L = Logging *)
open PyIR

module Parameter = struct
  let locals = Textual.VarName.of_string "locals"

  let globals = Textual.VarName.of_string "globals"
end

let location_from_opt_line = function
  | None ->
      Textual.Location.Unknown
  | Some line ->
      Textual.Location.known ~line ~col:(-1)


let of_location loc = Location.line loc |> location_from_opt_line

module Typ = struct
  let locals = Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string "PyLocals")))

  let globals = Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string "PyGlobals")))

  let value = Textual.(Typ.Ptr (Typ.Struct (TypeName.of_string "PyObject")))
end

type proc_kind = ModuleBody of Ident.t | RegularFunction of QualName.t

let is_module_body = function ModuleBody _ -> true | _ -> false

let mk_procdecl kind =
  let qual_name_str =
    match kind with
    | ModuleBody name ->
        F.asprintf "%a.__module_body__" Ident.pp name
    | RegularFunction qual_name ->
        F.asprintf "%a" QualName.pp qual_name
  in
  let qualified_name =
    { Textual.QualifiedProcName.enclosing_class= TopLevel
    ; name= Textual.ProcName.of_string qual_name_str }
  in
  let formals_types =
    if is_module_body kind then Some []
    else
      Some
        [Textual.Typ.mk_without_attributes Typ.globals; Textual.Typ.mk_without_attributes Typ.locals]
  in
  let result_type = Textual.Typ.mk_without_attributes Typ.value in
  let attributes = [] in
  {Textual.ProcDecl.qualified_name; formals_types; result_type; attributes}


let mk_node_name node_name = F.asprintf "%a" NodeName.pp node_name |> Textual.NodeName.of_string

let mk_jump {Terminator.label; ssa_args} =
  let null = Textual.Exp.null (* TODO *) in
  Textual.Terminator.(
    Jump [{label= mk_node_name label; ssa_args= List.map ssa_args ~f:(fun _ -> null)}] )


let of_terminator terminator : Textual.Terminator.t =
  let null = Textual.Exp.null (* TODO *) in
  match (terminator : Terminator.t) with
  | Return _ ->
      Ret null
  | Throw _ ->
      Throw null
  | Jump node_call ->
      mk_jump node_call
  | If {exp= _; then_; else_} ->
      let then_ = mk_jump then_ in
      let else_ = mk_jump else_ in
      If {bexp= Exp null; then_; else_}


let of_node {Node.name; first_loc; last_loc; ssa_parameters; stmts= _; last} =
  let label = mk_node_name name in
  let label_loc = of_location first_loc in
  let last_loc = of_location last_loc in
  let last = of_terminator last in
  let instrs = [] (* TODO *) in
  let exn_succs = [] (* TODO *) in
  let ssa_parameters =
    List.map ssa_parameters ~f:(fun ssa ->
        let ident = Textual.Ident.of_int (SSA.id ssa) in
        (ident, Typ.value) )
  in
  {Textual.Node.label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc}


let mk_procdesc proc_kind {CFG.entry; nodes; code_info= _} =
  let procdecl = mk_procdecl proc_kind in
  let nodes_bindings = NodeName.Map.bindings nodes in
  let nodes = List.map nodes_bindings ~f:(fun (_node_name, node) -> of_node node) in
  let start = mk_node_name entry in
  let params = if is_module_body proc_kind then [] else [Parameter.globals; Parameter.locals] in
  let locals = [] in
  let exit_loc =
    let last_loc =
      List.fold nodes_bindings ~init:None ~f:(fun acc (_, {Node.last_loc}) ->
          match Location.line last_loc with
          | None ->
              acc
          | Some line ->
              Some (Option.value_map acc ~default:line ~f:(fun acc_line -> Int.max acc_line line)) )
    in
    location_from_opt_line last_loc
  in
  {Textual.ProcDesc.procdecl; nodes; start; params; locals; exit_loc}


let mk_module {Module.name; toplevel; functions} =
  let filename = F.asprintf "%a.py" Ident.pp name (* TODO: may not work with nested paths *) in
  let sourcefile = Textual.SourceFile.create filename in
  let decls =
    List.map (QualName.Map.bindings functions) ~f:(fun (qual_name, cfg) ->
        Textual.Module.Proc (mk_procdesc (RegularFunction qual_name) cfg) )
  in
  let decls = Textual.Module.Proc (mk_procdesc (ModuleBody name) toplevel) :: decls in
  {Textual.Module.attrs= []; decls; sourcefile}
