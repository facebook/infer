(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(* TODO: See if there is a better way to access function names *)
module FunMap = Stdlib.Map.Make(String)
let fun_map : string FunMap.t ref = ref FunMap.empty

module PlaceMap = Stdlib.Map.Make(Int)

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
  | Add _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_plusa_int"
  | Sub _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_minusa_int"
  | Mul _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_mult_int"
  | Div _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_divi"
  | Rem _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_mod"
  | BitXor, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_bxor"
  | BitAnd, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_band"
  | BitOr, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_bor"
  | Eq, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_eq"
  | Lt, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_lt"
  | Le, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_le"
  | Ne, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_ne"
  | Ge, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_ge"
  | Gt, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_gt"
  | _ ->
      L.die UserError "Not yet supported %a " Charon.Generated_Expressions.pp_binop op


let get_textual_typ (rust_ty : Charon.Generated_Types.ty) : Textual.Typ.t =
  (* TODO: Add support for types other than TLiteral *)
  (* TODO: Consider how to handle bool *)
  (* TODO: Check the implementation for unit type - empty tuple *)
  match rust_ty with
  | Charon.Generated_Types.TLiteral (Charon.Generated_Values.TInt _) ->
      Textual.Typ.Int
  | Charon.Generated_Types.TLiteral (Charon.Generated_Values.TUInt _) ->
      Textual.Typ.Int
  | Charon.Generated_Types.TLiteral (Charon.Generated_Values.TFloat _) ->
      Textual.Typ.Float
  | Charon.Generated_Types.TLiteral Charon.Generated_Values.TBool ->
      Textual.Typ.Int
  | Charon.Generated_Types.TLiteral Charon.Generated_Values.TChar ->
      Textual.Typ.Int
  | Charon.Generated_Types.TAdt _ ->
      Textual.Typ.Void
  | _ ->
      L.die UserError "Not yet supported %a" Charon.Generated_Types.pp_ty rust_ty


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
  let result_type = Textual.Typ.mk_without_attributes (get_textual_typ proc.signature.output) in
  let param_types = List.map proc.signature.inputs ~f:get_textual_typ in
  let formals_types = Some (List.map param_types ~f:Textual.Typ.mk_without_attributes) in
  let attributes = [] in
  {Textual.ProcDecl.qualified_name; formals_types; result_type; attributes}


let mk_const_exp (rust_ty : Charon.Generated_Types.ty)
    (value : Charon.Generated_Expressions.raw_constant_expr) : Textual.Exp.t =
  (* TODO: Add support for more types *)
  match rust_ty with
  | TLiteral literal_ty -> (
    match literal_ty with
    | TInt _ | TUInt _ -> (
      match value with
      | CLiteral (VScalar (UnsignedScalar (_, n))) | CLiteral (VScalar (SignedScalar (_, n))) ->
          Textual.Exp.Const (Textual.Const.Int n)
      | _ ->
          L.die UserError "This int type is not yet supported" )
    | TFloat _ -> (
      match value with
      | CLiteral (VFloat {float_value= f; float_ty= _}) ->
          Textual.Exp.Const (Textual.Const.Float (float_of_string f))
      | _ ->
          L.die UserError "This float type is not yet supported" )
    | TBool -> (
      match value with
      | CLiteral (VBool b) ->
          Textual.Exp.Const (Textual.Const.Int (if b then Z.one else Z.zero))
      | _ ->
          L.die UserError "This bool type is not yet supported" )
    | TChar -> (
      match value with
      | CLiteral (VChar c) -> (
        match Uchar.to_char c with
        | Some ch ->
            Textual.Exp.Const (Textual.Const.Int (Z.of_int (int_of_char ch)))
        | None ->
            L.die UserError "Cannot convert Unicode character to char" )
      | _ ->
          L.die UserError "This char type is not yet supported" ) )
  | _ ->
      L.die UserError "Not yet supported %a" Charon.Generated_Types.pp_ty rust_ty


let mk_exp_from_operand (place_map : string PlaceMap.t) (operand : Charon.Generated_Expressions.operand):
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match operand with
  | Copy place | Move place ->
      let temp_id = Textual.Ident.fresh !ident_set in
      let temp_exp = Textual.Exp.Var(temp_id) in
      let temp_typ = get_textual_typ place.ty in
      let temp_loc = Textual.Location.Unknown in
      let place_id = match place.kind with
        | PlaceLocal var_id -> Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id)
        | _ -> L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_place place
      in
      let place_exp = Textual.Exp.Lvar (Textual.VarName.of_string (PlaceMap.find (Textual.Ident.to_int place_id) place_map)) in
      let load_instr =
        Textual.Instr.Load {id= temp_id; exp= place_exp; typ= Some temp_typ; loc= temp_loc}
      in
      ident_set := Textual.Ident.Set.add temp_id !ident_set ;
      ([load_instr], temp_exp, temp_typ)
  | Constant const_operand ->
      let value = const_operand.value in
      let rust_ty = const_operand.ty in
      let exp = mk_const_exp rust_ty value in
      let textual_ty = get_textual_typ rust_ty in
      ([], exp, textual_ty)


let mk_exp_from_rvalue (rvalue : Charon.Generated_Expressions.rvalue) (place_map : string PlaceMap.t) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match rvalue with
  | UnaryOp (op, operand) ->
      let instrs, exp, typ = mk_exp_from_operand place_map operand in
      let proc_name = proc_name_from_unop op typ in
      let qualified_proc_name =
        {Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel; name= proc_name}
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp] in
      (instrs, call, typ)
  | BinaryOp (op, operand1, operand2) ->
      let instrs1, exp1, typ1 = mk_exp_from_operand place_map operand1 in
      let instrs2, exp2, _ = mk_exp_from_operand place_map operand2 in
      let proc_name = proc_name_from_binop op typ1 in
      let qualified_proc_name =
        {Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel; name= proc_name}
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp1; exp2] in
      (instrs1 @ instrs2, call, typ1)
  | Aggregate (kind, ops) -> (
      let _, exps, _ =
        List.fold_right
          (List.map ~f:(mk_exp_from_operand place_map) ops)
          ~f:(fun (instrs, exp, typ) (all_instrs, all_exps, all_typs) ->
            (instrs @ all_instrs, exp :: all_exps, typ :: all_typs) )
          ~init:([], [], [])
      in
      match (kind, exps) with
      (* Handle other aggregate types other than unit() *)
      | AggregatedAdt (_, None, None), [] ->
          ([], Textual.Exp.Const Textual.Const.Null, Textual.Typ.Void)
      | _ ->
          L.die UserError "Aggregates other than unit() are not yet supported" )
  | Use op ->
      mk_exp_from_operand place_map op
  | _ ->
      L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_rvalue rvalue


let func_name_from_operand (operand: Charon.Generated_GAst.fn_operand) : string =
  let func_name = match operand with
      | FnOpRegular fn_ptr -> 
          (match fn_ptr.func with
            | FunId id ->
             (match id with 
               | FRegular fun_decl_id -> 
                   (match FunMap.find_opt (Charon.GAst.FunDeclId.to_string fun_decl_id) !fun_map with
                   | Some name -> name
                   | None -> L.die UserError "Function not found in fun_map: %s" (Charon.GAst.FunDeclId.to_string fun_decl_id))
               | _ -> L.die UserError "Not yet supported"
              )
            | TraitMethod (_) -> L.die UserError "Not yet supported %a" Charon.Generated_GAst.pp_fn_operand operand
          )
      | FnOpMove (_) -> L.die UserError "Not yet supported %a" Charon.Generated_GAst.pp_fn_operand operand
    in
    func_name


let mk_terminator (terminator : Charon.Generated_UllbcAst.terminator) (place_map : string PlaceMap.t) : Textual.Instr.t list * Textual.Terminator.t =
  (* TODO: Handle the switch_int and unwind_resume *)
  match terminator.content with
  | Charon.Generated_UllbcAst.Call (call, block_id_1, _) ->
    (* Extract arguments from call*)
    let arg_evals = List.map call.args ~f:(mk_exp_from_operand place_map) in
    let arg_instrs = List.concat_map arg_evals ~f:(fun (instrs, _, _) -> instrs) in
    let arg_exps = List.map arg_evals ~f:(fun (_, exp, _) -> exp) in

    (* Get the function name *)
    let name = func_name_from_operand call.func in
    let proc_name = Textual.ProcName.of_string name in
    let qualified_proc_name = {Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel; name = proc_name} in
    
    (* Get the destination of the call *)
    let dest = match call.dest.kind with
      | PlaceLocal var_id -> Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id)
      | _ -> L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_place call.dest
    in

    (* Create the expressions *)
    let exp1 = Textual.Exp.Lvar (Textual.VarName.of_string (PlaceMap.find (Textual.Ident.to_int dest) place_map)) in
    let exp2 = Textual.Exp.Call {proc= qualified_proc_name; args= arg_exps; kind= Textual.Exp.NonVirtual} in

    (* Create a call instruction like n0 = call temp_func(args) *)
    let temp_id = Textual.Ident.fresh !ident_set in
    let call_instr = Textual.Instr.Let {id= Some temp_id; exp= exp2; loc= Textual.Location.Unknown} in

    (* Create the store instruction *)
    let temp_exp = Textual.Exp.Var temp_id in
    let store_instr = Textual.Instr.Store {exp1 = exp1; typ= Some (get_textual_typ call.dest.ty); exp2= temp_exp; loc= Textual.Location.Unknown} in
    
    (* Create the jump instruction *)
    let label =
        "node_" ^ string_of_int (Charon.Generated_UllbcAst.BlockId.to_int block_id_1)
        |> Textual.NodeName.of_string
      in
    let ssa_args = [] in
    let node_call : Textual.Terminator.node_call = {label; ssa_args} in
    
    (* Update the ident set *)
    ident_set := Textual.Ident.Set.add temp_id !ident_set ;

    (* Return the instructions *)
    (arg_instrs @ [call_instr] @ [store_instr], Textual.Terminator.Jump [node_call])
  | Charon.Generated_UllbcAst.Goto id ->
      let label =
        "node_" ^ string_of_int (Charon.Generated_UllbcAst.BlockId.to_int id)
        |> Textual.NodeName.of_string
      in
      let ssa_args = [] in
      let node_call : Textual.Terminator.node_call = {label; ssa_args} in
      ([], Textual.Terminator.Jump [node_call])
  | Charon.Generated_UllbcAst.Return ->
      ([], Textual.Terminator.Ret (Textual.Exp.Lvar (Textual.VarName.of_string "var_0")))
  | Charon.Generated_UllbcAst.UnwindResume -> ([], Textual.Terminator.Throw (Textual.Exp.Lvar (Textual.VarName.of_string "var_0")))
  | t ->
      L.die UserError "Not yet supported %a" Charon.Generated_UllbcAst.pp_raw_terminator t


let mk_instr (place_map : string PlaceMap.t) (statement : Charon.Generated_UllbcAst.statement) : Textual.Instr.t list =
  let loc = location_from_span statement.span in
  match statement.content with
  | Assign (lhs, rhs) -> (
    match lhs.kind with
    | PlaceLocal var_id ->
        let id = Charon.Generated_Expressions.LocalId.to_int var_id in
        let typ = get_textual_typ lhs.ty in
        let exp1 =
          Textual.Exp.Lvar
            (Textual.VarName.of_string
               (PlaceMap.find id place_map) )
        in
        let instrs, exp2, _ = mk_exp_from_rvalue rhs place_map in
        let store_instr = Textual.Instr.Store {exp1; typ= Some typ; exp2; loc} in
        instrs @ [store_instr]
    | _ ->
        L.die UserError "Not yet supported %a" Charon.Generated_Expressions.pp_place lhs )
  | StorageDead _ ->
      []
  | StorageLive _ ->
      []
  | s ->
      L.die UserError "Not yet supported %a" Charon.Generated_UllbcAst.pp_raw_statement s


let mk_node (idx : int) (block : Charon.Generated_UllbcAst.block) (place_map : string PlaceMap.t): Textual.Node.t =
  ident_set := Textual.Ident.Set.empty;
  let label = "node_" ^ string_of_int idx |> Textual.NodeName.of_string in
  (*TODO Should be retrieved from Γ *)
  let ssa_parameters = [] in
  let exn_succs = [] in
  let instrs1 = block.statements |> List.concat_map ~f:(mk_instr place_map) in
  let instrs2, last = mk_terminator block.terminator place_map in
  let instrs = instrs1 @ instrs2 in
  let last_loc = location_from_span block.terminator.span in
  let label_loc = Textual.Location.Unknown in
  {Textual.Node.label; ssa_parameters; exn_succs; last; instrs; last_loc; label_loc}


let mk_place_map (locals : Charon.Generated_GAst.local list) : string PlaceMap.t =
  List.foldi locals ~init:PlaceMap.empty ~f:(fun i acc (local : Charon.Generated_GAst.local) ->
      let id = local.index in
      let name =
        match local.name with
        | Some name -> name
        | None -> "var_" ^ string_of_int i
      in
      PlaceMap.add (Charon.Generated_Expressions.LocalId.to_int id) name acc)

    
let mk_procdesc (proc : Charon.GAst.fun_decl_id * Charon.UllbcAst.blocks Charon.GAst.gfun_decl) :
    Textual.ProcDesc.t =
  let _, fun_decl = proc in
  let blocks, locals, arg_count =
    match fun_decl.body with
    | Some {span= _; locals; body} ->
        (body, locals.locals, locals.arg_count)
    | None ->
        ([], [], 0)
  in
  let place_map = mk_place_map locals in
  let procdecl = mk_procdecl fun_decl in
  let nodes = List.mapi blocks ~f:(fun i block -> mk_node i block place_map) in  
  let start = Textual.NodeName.of_string "node_0" in
  let params = 
    match fun_decl.body with
    | Some {locals} -> let locals_list = List.tl locals.locals in
        (match locals_list with 
        | Some locals_list -> 
          List.take locals_list arg_count
          |> List.mapi ~f:(fun i (local: Charon.Generated_GAst.local) -> 
              (match local.name with 
              | Some name -> Textual.VarName.of_string name
              | None -> Textual.VarName.of_string ("param_" ^ string_of_int i))
              )
        | None -> [])
    | None -> []
  in
  let locals =
      locals
        |> fun lst -> List.take lst 1 @ List.drop lst (1 + arg_count)
        |> List.map ~f:(fun (l: Charon.Generated_GAst.local) ->
          let id = l.index in
          (Textual.VarName.of_string (PlaceMap.find (Charon.Generated_Expressions.LocalId.to_int id) place_map), Textual.Typ.mk_without_attributes (get_textual_typ l.var_ty)))
  in
  let exit_loc = location_from_span_end fun_decl.item_meta.span in
  {Textual.ProcDesc.procdecl; nodes; start; params; locals; exit_loc}


let fun_name_of_decl (fun_decl : Charon.UllbcAst.blocks Charon.GAst.gfun_decl) : string =
  let open Charon in
  let name = fun_decl.item_meta.name in
  match List.tl name with
  | Some name_list -> (
      match List.hd name_list with
      | Some pe -> name_of_path_element pe
      | None -> L.die UserError "Not yet supported %a" Generated_Types.pp_name name)
  | None -> L.die UserError "Not yet supported %a" Generated_Types.pp_name name


let fun_map_of_decls fun_decls (init : string FunMap.t) : string FunMap.t =
  let fun_decl_list = Charon.Generated_Types.FunDeclId.Map.bindings fun_decls in
  List.fold_left fun_decl_list ~init ~f:(fun acc (fun_decl_id, fun_decl) ->
      let fun_name = fun_name_of_decl fun_decl in
      FunMap.add (Charon.Generated_Types.FunDeclId.to_string fun_decl_id) fun_name acc)


let mk_module (crate : Charon.UllbcAst.crate) json_file : Textual.Module.t =
  let fun_decls = crate.fun_decls in
  fun_map := fun_map_of_decls fun_decls FunMap.empty;
  let attrs = [Textual.Attr.mk_source_language Rust] in
  let decls =
    Charon.Generated_Types.FunDeclId.Map.bindings fun_decls
    |> List.map ~f:mk_procdesc
    |> List.map ~f:(fun p -> Textual.Module.Proc p)
  in
  let sourcefile = Textual.SourceFile.create json_file in
  {Textual.Module.attrs; decls; sourcefile}
