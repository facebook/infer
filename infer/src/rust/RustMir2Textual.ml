(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let location_from_span (span : Charon.Generated_Meta.span) : Textual.Location.t =
  let line = span.span.beg_loc.line in
  let col = span.span.beg_loc.col in
  Textual.Location.known ~line ~col


let location_from_span_end (span : Charon.Generated_Meta.span) : Textual.Location.t =
  let line = span.span.end_loc.line in
  let col = span.span.end_loc.col in
  Textual.Location.known ~line ~col


let name_of_path_element (path_element : Charon.Generated_Types.path_elem) : string =
  match path_element with
  | PeIdent (name, _) ->
      name
  | _ ->
      L.die UserError "Unsupported path element %a" Charon.Generated_Types.pp_path_elem path_element


let mk_name (name : Charon.Generated_Types.name) : Textual.ProcName.t =
  let names = List.map name ~f:name_of_path_element in
  let name_str = Stdlib.String.concat "::" names in
  Textual.ProcName.of_string name_str


let mk_qualified_proc_name (item_meta : Charon.Generated_Types.item_meta) :
    Textual.QualifiedProcName.t =
  let enclosing_class = Textual.QualifiedProcName.TopLevel in
  let name = mk_name item_meta.name in
  {Textual.QualifiedProcName.enclosing_class; name}


let mk_procdecl (proc : Charon.UllbcAst.fun_decl) : Textual.ProcDecl.t =
  let qualified_name = mk_qualified_proc_name proc.item_meta in
  let result_type = Textual.Typ.mk_without_attributes Textual.Typ.Void in
  let formals_types = Some [] in
  let attributes = [] in
  {Textual.ProcDecl.qualified_name; formals_types; result_type; attributes}


let mk_terminator (terminator : Charon.Generated_UllbcAst.terminator) : Textual.Terminator.t =
  match terminator.content with
  | Charon.Generated_UllbcAst.Goto id ->
      let label =
        "node_" ^ string_of_int (Charon.Generated_UllbcAst.BlockId.to_int id)
        |> Textual.NodeName.of_string
      in
      let ssa_args = [] in
      let node_call : Textual.Terminator.node_call = {label; ssa_args} in
      Textual.Terminator.Jump [node_call]
  | Charon.Generated_UllbcAst.Return ->
      Textual.Terminator.Ret (Textual.Exp.Lvar (Textual.VarName.of_string "var_0"))
  | t ->
      L.die UserError "Not yet supported %a" Charon.Generated_UllbcAst.pp_raw_terminator t


let mk_instr (statement : Charon.Generated_UllbcAst.statement) : Textual.Instr.t =
  let loc = location_from_span statement.span in
  let exp1 = Textual.Exp.Lvar (Textual.VarName.of_string "var_0") in
  let exp2 = Textual.Exp.Const (Textual.Const.Int (Z.of_int 10)) in
  let typ = None in
  Textual.Instr.Store {exp1; typ; exp2; loc}


let mk_node (idx : int) (block : Charon.Generated_UllbcAst.block) : Textual.Node.t =
  let label = "node_" ^ string_of_int idx |> Textual.NodeName.of_string in
  (*TODO Should be retrieved from Γ *)
  let ssa_parameters = [] in
  let exn_succs = [] in
  let last = mk_terminator block.terminator in
  let instrs = block.statements |> List.map ~f:mk_instr in
  let last_loc = location_from_span block.terminator.span in
  let label_loc = Textual.Location.Unknown in
  {Textual.Node.label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc}


let mk_procdesc (proc : Charon.GAst.fun_decl_id * Charon.UllbcAst.blocks Charon.GAst.gfun_decl) :
    Textual.ProcDesc.t =
  let _, fun_decl = proc in
  let procdecl = mk_procdecl fun_decl in
  let blocks, _locals =
    match fun_decl.body with
    | Some {span= _; locals; body} ->
        (body, locals.locals)
    | None ->
        ([], [])
  in
  let nodes = List.mapi blocks ~f:mk_node in
  let start = Textual.NodeName.of_string "node_0" in
  (* TODO *)
  let params = [] in
  (* TODO *)
  let locals =
    [(Textual.VarName.of_string "var_0", Textual.Typ.mk_without_attributes Textual.Typ.Int)]
    (* TODO *)
  in
  let exit_loc = location_from_span_end fun_decl.item_meta.span in
  {Textual.ProcDesc.procdecl; nodes; start; params; locals; exit_loc}


let mk_module (crate : Charon.UllbcAst.crate) json_file : Textual.Module.t =
  let fun_decls = crate.fun_decls in
  let attrs = [Textual.Attr.mk_source_language Rust] in
  let decls =
    Charon.Generated_Types.FunDeclId.Map.bindings fun_decls
    |> List.map ~f:mk_procdesc
    |> List.map ~f:(fun p -> Textual.Module.Proc p)
  in
  let sourcefile = Textual.SourceFile.create json_file in
  {Textual.Module.attrs; decls; sourcefile}
