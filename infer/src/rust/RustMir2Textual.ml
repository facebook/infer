(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let ident_set = ref Textual.Ident.Set.empty


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


let proc_name_from_unop (op : Charon.Generated_Expressions.unop) (typ : Textual.Typ.t) :
    Textual.ProcName.t =
  (* TODO: Handle bool, Textual.Typ does not have bool *)
  match (op, typ) with
  | Charon.Generated_Expressions.Neg _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_neg"
  | Charon.Generated_Expressions.Neg _, Textual.Typ.Float ->
      Textual.ProcName.of_string "__sil_neg"
  | Charon.Generated_Expressions.Not, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_bnot"
  | _ ->
      L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_unop op


let proc_name_from_binop (op : Charon.Generated_Expressions.binop) (typ : Textual.Typ.t) :
    Textual.ProcName.t =
  match (op, typ) with
  | Charon.Generated_Expressions.Add _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_plusa_int"
  | Charon.Generated_Expressions.Sub _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_minusa_int"
  | Charon.Generated_Expressions.Mul _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_mult_int"
  | Charon.Generated_Expressions.Div _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_divi"
  | Charon.Generated_Expressions.Rem _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_mod"
  | Charon.Generated_Expressions.BitXor, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_bxor"
  | Charon.Generated_Expressions.BitAnd, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_band"
  | Charon.Generated_Expressions.BitOr, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_bor"
  | Charon.Generated_Expressions.Eq, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_eq"
  | Charon.Generated_Expressions.Lt, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_lt"
  | Charon.Generated_Expressions.Le, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_le"
  | Charon.Generated_Expressions.Ne, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_ne"
  | Charon.Generated_Expressions.Ge, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_ge"
  | Charon.Generated_Expressions.Gt, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_gt"
  | _ ->
      L.die UserError "Not yet supported %a " Charon.Generated_Expressions.pp_binop op


let get_textual_typ (rust_ty: Charon.Generated_Types.ty) : Textual.Typ.t = 
  match rust_ty with 
   | Charon.Generated_Types.TLiteral (Charon.Generated_Values.TInt _) -> Textual.Typ.Int
   | Charon.Generated_Types.TLiteral (Charon.Generated_Values.TFloat _) -> Textual.Typ.Float
   | _ -> L.die UserError "Not yet supported %a" Charon.Generated_Types.pp_ty rust_ty


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


let mk_exp_from_operand (operand : Charon.Generated_Expressions.operand) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match operand with
  | Copy place | Move place ->
      let temp_id = Textual.Ident.fresh !ident_set in
      let temp_exp = Textual.Exp.Lvar (Textual.VarName.of_string ("n" ^ string_of_int (Textual.Ident.to_int temp_id))) in
      let temp_typ = get_textual_typ place.ty in
      let temp_loc = Textual.Location.Unknown in
      let load_instr = Textual.Instr.Load {id = temp_id; exp = temp_exp; typ = Some temp_typ; loc = temp_loc} in
      ident_set := Textual.Ident.Set.add temp_id !ident_set;
      ([load_instr], temp_exp, temp_typ)
  | Constant const_operand -> (
      (* TODO: Add more types *)
      let value = const_operand.value in
      let ty = const_operand.ty in
      match ty with
      | Charon.Generated_Types.TLiteral literal_kind ->
          let exp, typ =
            match literal_kind with
            | Charon.Generated_Values.TInt _ -> (
              match value with
              | Charon.Generated_Expressions.CLiteral (VScalar (UnsignedScalar (_, n)))
              | Charon.Generated_Expressions.CLiteral (VScalar (SignedScalar (_, n))) ->
                  (Textual.Exp.Const (Textual.Const.Int n), Textual.Typ.Int)
              | _ ->
                  L.die UserError "This int literal kind is not yet supported" )
            | Charon.Generated_Values.TFloat _ ->
                (* TODO: Add support for float *)
                L.die UserError "Float type not yet supported"
            | _ ->
                L.die UserError "This literal kind is not yet supported"
          in
          ([], exp, typ)
      | _ ->
          L.die UserError "Not yet supported %a" Charon.Generated_Types.pp_ty ty )


let mk_exp_from_rvalue (rvalue : Charon.Generated_Expressions.rvalue) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match rvalue with
  | UnaryOp (op, operand) ->
      let instrs, exp, typ = mk_exp_from_operand operand in
      let proc_name = proc_name_from_unop op typ in
      let qualified_proc_name =
        {Textual.QualifiedProcName.enclosing_class= TopLevel; name= proc_name}
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp] in
      (instrs, call, typ)
  | BinaryOp (op, operand1, operand2) ->
      let instrs1, exp1, typ1 = mk_exp_from_operand operand1 in
      let instrs2, exp2, _ = mk_exp_from_operand operand2 in
      let proc_name = proc_name_from_binop op typ1 in
      let qualified_proc_name =
        {Textual.QualifiedProcName.enclosing_class= TopLevel; name= proc_name}
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp1; exp2] in
      (instrs1 @ instrs2, call, typ1)
  | Aggregate (kind, ops) -> (
      let _, exps, _ =
        List.fold_right
          (List.map ~f:mk_exp_from_operand ops)
          ~f:(fun (instrs, exp, typ) (all_instrs, all_exps, all_typs) -> (instrs @ all_instrs, exp :: all_exps, typ :: all_typs))
          ~init:([], [], [])
      in
      match (kind, exps) with
      | AggregatedAdt (_, None, None), [] ->
          (* unit () *)
          ([], Textual.Exp.Const Textual.Const.Null, Textual.Typ.Void)
      | _ ->
          L.die UserError "Aggregates other than unit() are not yet supported" )
  | Use op ->
      mk_exp_from_operand op
  | _ ->
      L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_rvalue rvalue


let mk_instr (statement : Charon.Generated_UllbcAst.statement) : Textual.Instr.t list =
  let loc = location_from_span statement.span in
  match statement.content with
  | Charon.Generated_UllbcAst.Assign (lhs, rhs) -> (
    match lhs.kind with
    | Charon.Generated_Expressions.PlaceLocal var_id ->
        let id = Charon.Generated_Expressions.LocalId.to_string var_id in
        let typ = None in
        let exp1 = Textual.Exp.Lvar (Textual.VarName.of_string ("var_" ^ id)) in
        let instrs, exp2, _ = mk_exp_from_rvalue rhs in
        let store_instr = Textual.Instr.Store {exp1; typ; exp2; loc} in
        instrs @ [store_instr]
    | _ ->
        L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_place lhs )
  | Charon.Generated_UllbcAst.StorageDead _ ->
      []
  | Charon.Generated_UllbcAst.StorageLive _ ->
      []
  | s ->
      L.die UserError "Not yet supported %a" Charon.Generated_UllbcAst.pp_raw_statement s


let mk_node (idx : int) (block : Charon.Generated_UllbcAst.block) : Textual.Node.t =
  let label = "node_" ^ string_of_int idx |> Textual.NodeName.of_string in
  (*TODO Should be retrieved from Γ *)
  let ssa_parameters = [] in
  let exn_succs = [] in
  let last = mk_terminator block.terminator in
  let instrs = block.statements |> List.concat_map ~f:mk_instr in
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
