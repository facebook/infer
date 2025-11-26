(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module PlaceMap = Stdlib.Map.Make (Int)

(* A map from place ids to (place name, type) *)
type place_map_ty = (Textual.VarName.t * Charon.Generated_Types.ty) PlaceMap.t

(* Holds the identifiers of temporary variables (n0, n1, etc.) to avoid reusing them within the same function *)
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


let mk_name (name : Charon.Generated_Types.name) : Textual.ProcName.t =
  let names = List.map name ~f:name_of_path_element in
  let name_str = Stdlib.String.concat "::" names in
  Textual.ProcName.of_string name_str


let mk_qualified_proc_name (item_meta : Charon.Generated_Types.item_meta) :
    Textual.QualifiedProcName.t =
  let enclosing_class = Textual.QualifiedProcName.TopLevel in
  let name = mk_name item_meta.name in
  {Textual.QualifiedProcName.enclosing_class; name}


let mk_varname (local : Charon.Generated_GAst.local) (index : int) : Textual.VarName.t =
  match local.name with
  | Some name ->
      Textual.VarName.of_string (name ^ "_" ^ string_of_int index)
  | None ->
      Textual.VarName.of_string ("var_" ^ string_of_int index)


let mk_label (id : int) : Textual.NodeName.t =
  "node_" ^ string_of_int id |> Textual.NodeName.of_string


let params_from_fun_decl (fun_decl : Charon.UllbcAst.blocks Charon.GAst.gfun_decl) (arg_count : int)
    : Textual.VarName.t list =
  match fun_decl.body with
  | Some {locals} -> (
      let locals_list = List.tl locals.locals in
      (* TODO: favor one match with nested patterns instead of nested matchs *)
      match locals_list with
      | Some locals_list ->
          List.take locals_list arg_count
          (* TODO: avoid 3x shadowing of locals_list *)
          |> List.mapi ~f:(fun i (local : Charon.Generated_GAst.local) -> mk_varname local (i + 1))
      | None ->
          [] )
  | None ->
      []


let adt_ty_to_textual_typ (type_decl_ref : Charon.Generated_Types.type_decl_ref) : Textual.Typ.t =
  (* TODO: Implement non-empty tuple and other adt types *)
  match type_decl_ref.id with
  | TTuple ->
      if List.is_empty type_decl_ref.generics.types then Textual.Typ.Void
      else
        L.die UserError "Unsupported tuple type: %a" Charon.Generated_Types.pp_type_decl_ref
          type_decl_ref
  | _ ->
      L.die UserError "Unsupported adt type: %a" Charon.Generated_Types.pp_type_decl_ref
        type_decl_ref


let rec ty_to_textual_typ (rust_ty : Charon.Generated_Types.ty) : Textual.Typ.t =
  (* Bool and char are mapped to int since Textual does not have bool type *)
  match rust_ty with
  | TLiteral (TInt _) | TLiteral (TUInt _) | TLiteral TBool | TLiteral TChar ->
      Textual.Typ.Int
  | TLiteral (TFloat _) ->
      Textual.Typ.Float
  | TRawPtr (ty, _) | TRef (_, ty, _) ->
      Textual.Typ.Ptr (ty_to_textual_typ ty)
  | TAdt type_decl_ref ->
      adt_ty_to_textual_typ type_decl_ref
  | _ ->
      L.die UserError "Unsupported type: %a" Charon.Generated_Types.pp_ty rust_ty


let mk_place_map (locals : Charon.Generated_GAst.local list) : place_map_ty =
  List.foldi locals ~init:PlaceMap.empty ~f:(fun i acc (local : Charon.Generated_GAst.local) ->
      let id = local.index in
      let name = mk_varname local i in
      let ty = local.var_ty in
      PlaceMap.add (Charon.Generated_Expressions.LocalId.to_int id) (name, ty) acc )


let mk_place_from_id (id : int) (ty : Charon.Generated_Types.ty) :
    Charon.Generated_Expressions.place =
  {kind= PlaceLocal (Charon.Generated_Expressions.LocalId.of_int id); ty}


let mk_locals (locals : Charon.Generated_GAst.local list) (arg_count : int)
    (place_map : place_map_ty) : (Textual.VarName.t * Textual.Typ.annotated) list =
  (* Extracts the local variable names from locals list, excluding the return value and the arguments *)
  locals
  |> fun lst ->
  (* TODO: could be simplified *)
  List.take lst 1 @ List.drop lst (1 + arg_count)
  |> List.map ~f:(fun (l : Charon.Generated_GAst.local) ->
         let id = l.index in
         let varname, _ =
           PlaceMap.find (Charon.Generated_Expressions.LocalId.to_int id) place_map
         in
         (varname, Textual.Typ.mk_without_attributes (ty_to_textual_typ l.var_ty)) )


let mk_const_exp (rust_ty : Charon.Generated_Types.ty)
    (value : Charon.Generated_Expressions.raw_constant_expr) : Textual.Exp.t =
  (* TODO: Add support for more types *)
  match rust_ty with
  | TLiteral literal_ty -> (
    (* TODO: favor one match with nested patterns instead of nested matchs *)
    match literal_ty with
    | TInt _ | TUInt _ -> (
      match value with
      | CLiteral (VScalar (UnsignedScalar (_, n))) | CLiteral (VScalar (SignedScalar (_, n))) ->
          Textual.Exp.Const (Textual.Const.Int n)
      | _ ->
          L.die UserError "Unsupported int type: %a" Charon.Generated_Types.pp_ty rust_ty )
    | TFloat _ -> (
      match value with
      | CLiteral (VFloat {float_value= f; float_ty= _}) ->
          Textual.Exp.Const (Textual.Const.Float (float_of_string f))
      | _ ->
          L.die UserError "Unsupported float type: %a" Charon.Generated_Types.pp_ty rust_ty )
    | TBool -> (
      match value with
      | CLiteral (VBool b) ->
          Textual.Exp.Const (Textual.Const.Int (if b then Z.one else Z.zero))
      | _ ->
          L.die UserError "Unsupported bool type: %a" Charon.Generated_Types.pp_ty rust_ty )
    | TChar -> (
      match value with
      | CLiteral (VChar c) -> (
        match Uchar.to_char c with
        | Some ch ->
            Textual.Exp.Const (Textual.Const.Int (Z.of_int (int_of_char ch)))
        | None ->
            L.die UserError "Cannot convert Unicode character to char: %a"
              Charon.Generated_Types.pp_ty rust_ty )
      | _ ->
          L.die UserError "Unsupported char type: %a" Charon.Generated_Types.pp_ty rust_ty ) )
  | _ ->
      L.die UserError "Unsupported literal type: %a" Charon.Generated_Types.pp_ty rust_ty


let mk_exp_from_place (place_map : place_map_ty) (place : Charon.Generated_Expressions.place) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  let temp_id = Textual.Ident.fresh !ident_set in
  let temp_exp = Textual.Exp.Var temp_id in
  let temp_typ = ty_to_textual_typ place.ty in
  let temp_loc = Textual.Location.Unknown in
  ident_set := Textual.Ident.Set.add temp_id !ident_set ;
  match place.kind with
  | PlaceLocal var_id ->
      let id = Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id) in
      let exp = Textual.Exp.Lvar (fst (PlaceMap.find (Textual.Ident.to_int id) place_map)) in
      let load_instr = Textual.Instr.Load {id= temp_id; exp; typ= Some temp_typ; loc= temp_loc} in
      (* TODO: generate a Load expression and avoid the ref [ident_set] *)
      ([load_instr], temp_exp, temp_typ)
  | _ ->
      L.die UserError "Unsupported place: %a" Charon.Generated_Expressions.pp_place place


let mk_exp_from_operand (place_map : place_map_ty) (operand : Charon.Generated_Expressions.operand)
    : Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match operand with
  | Copy place | Move place ->
      mk_exp_from_place place_map place
  | Constant const_operand ->
      let value = const_operand.value in
      let rust_ty = const_operand.ty in
      let textual_typ = ty_to_textual_typ rust_ty in
      let exp = mk_const_exp rust_ty value in
      ([], exp, textual_typ)


let mk_exp_from_rvalue (rvalue : Charon.Generated_Expressions.rvalue) (place_map : place_map_ty) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match rvalue with
  | Aggregate (kind, ops) -> (
      (* TODO: Handle non-empty aggregates as well *)
      let _, exps, _ =
        List.fold_right
          (List.map ~f:(mk_exp_from_operand place_map) ops)
          ~f:(fun (instrs, exp, typ) (all_instrs, all_exps, all_typs) ->
            (instrs @ all_instrs, exp :: all_exps, typ :: all_typs) )
          ~init:([], [], [])
      in
      match (kind, exps) with
      | AggregatedAdt (_, None, None), [] ->
          ([], Textual.Exp.Const Textual.Const.Null, Textual.Typ.Void)
      | _ ->
          L.die UserError "Unsupported aggregate type: %a" Charon.Generated_Expressions.pp_rvalue
            rvalue )
  | Use op ->
      mk_exp_from_operand place_map op
  | _ ->
      L.die UserError "Unsupported rvalue: %a" Charon.Generated_Expressions.pp_rvalue rvalue


let mk_terminator (_crate : Charon.UllbcAst.crate) (place_map : place_map_ty)
    (terminator : Charon.Generated_UllbcAst.terminator) :
    Textual.Instr.t list * Textual.Terminator.t =
  match terminator.content with
  | Charon.Generated_UllbcAst.Goto block_id ->
      let label = mk_label (Charon.Generated_UllbcAst.BlockId.to_int block_id) in
      let ssa_args = [] in
      let node_call : Textual.Terminator.node_call = {label; ssa_args} in
      ([], Textual.Terminator.Jump [node_call])
  | Charon.Generated_UllbcAst.Return ->
      (* TODO: give a meaningfull name to constant 0 *)
      let _, ty = PlaceMap.find 0 place_map in
      let place = mk_place_from_id 0 ty in
      let instrs, exp, _ = mk_exp_from_place place_map place in
      (instrs, Textual.Terminator.Ret exp)
  | Charon.Generated_UllbcAst.UnwindResume ->
      (* TODO: To be updated when error handling is being implemented *)
      ([], Textual.Terminator.Unreachable)
  | t ->
      L.die UserError "Unsupported terminator: %a" Charon.Generated_UllbcAst.pp_raw_terminator t


let mk_instr (place_map : place_map_ty) (statement : Charon.Generated_UllbcAst.statement) :
    Textual.Instr.t list =
  let loc = location_from_span statement.span in
  match statement.content with
  | Assign (lhs, rhs) -> (
    (* TODO: favor one match with nested patterns instead of nested matchs *)
    match lhs.kind with
    | PlaceLocal var_id ->
        let id = Charon.Generated_Expressions.LocalId.to_int var_id in
        let varname, _ = PlaceMap.find id place_map in
        let exp1 = Textual.Exp.Lvar varname in
        let instrs, exp2, typ = mk_exp_from_rvalue rhs place_map in
        let store_instr = Textual.Instr.Store {exp1; typ= Some typ; exp2; loc} in
        instrs @ [store_instr]
    | _ ->
        L.die UserError "Unsupported place: %a" Charon.Generated_Expressions.pp_place lhs )
  | StorageDead _ ->
      []
  | StorageLive _ ->
      []
  | s ->
      L.die UserError "Unsupported statement: %a" Charon.Generated_UllbcAst.pp_raw_statement s


let mk_procdecl (proc : Charon.UllbcAst.fun_decl) : Textual.ProcDecl.t =
  let qualified_name = mk_qualified_proc_name proc.item_meta in
  let result_type = Textual.Typ.mk_without_attributes (ty_to_textual_typ proc.signature.output) in
  let param_types = List.map proc.signature.inputs ~f:ty_to_textual_typ in
  let formals_types = Some (List.map param_types ~f:Textual.Typ.mk_without_attributes) in
  let attributes = [] in
  {Textual.ProcDecl.qualified_name; formals_types; result_type; attributes}


let mk_node (crate : Charon.UllbcAst.crate) (idx : int) (block : Charon.Generated_UllbcAst.block)
    (place_map : place_map_ty) : Textual.Node.t =
  let label = mk_label idx in
  let ssa_parameters = [] in
  let exn_succs = [] in
  let instrs1 = block.statements |> List.concat_map ~f:(mk_instr place_map) in
  let instrs2, last = mk_terminator crate place_map block.terminator in
  let instrs = instrs1 @ instrs2 in
  let last_loc = location_from_span block.terminator.span in
  let label_loc = Textual.Location.Unknown in
  {Textual.Node.label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc}


let mk_procdesc (crate : Charon.UllbcAst.crate)
    (proc : Charon.GAst.fun_decl_id * Charon.UllbcAst.blocks Charon.GAst.gfun_decl) :
    Textual.ProcDesc.t =
  (* TODO: use setter or avoid this ref *)
  ident_set := Textual.Ident.Set.empty ;
  let _, fun_decl = proc in
  let blocks, locals, arg_count =
    match fun_decl.body with
    | Some {span= _; locals; body} ->
        (body, locals.locals, locals.arg_count)
    | None ->
        ([], [], 0)
  in
  let place_map = mk_place_map locals in
  let fresh_ident = None in
  let procdecl = mk_procdecl fun_decl in
  let nodes = List.mapi blocks ~f:(fun i block -> mk_node crate i block place_map) in
  let start = mk_label 0 in
  let params = params_from_fun_decl fun_decl arg_count in
  let locals = mk_locals locals arg_count place_map in
  let exit_loc = location_from_span_end fun_decl.item_meta.span in
  {Textual.ProcDesc.procdecl; fresh_ident; nodes; start; params; locals; exit_loc}


let mk_module (crate : Charon.UllbcAst.crate) ~json_filename : Textual.Module.t =
  let fun_decls = crate.fun_decls in
  let attrs = [Textual.Attr.mk_source_language Rust] in
  let decls =
    Charon.Generated_Types.FunDeclId.Map.bindings fun_decls
    |> List.map ~f:(fun proc -> Textual.Module.Proc (mk_procdesc crate proc))
  in
  let sourcefile = Textual.SourceFile.create json_filename in
  {Textual.Module.attrs; decls; sourcefile}
