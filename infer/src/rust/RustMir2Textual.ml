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
type place_map_ty = (string * Charon.Generated_Types.ty) PlaceMap.t

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


let item_meta_to_string (item_meta : Charon.Generated_Types.item_meta) : string =
  let names = List.map item_meta.name ~f:name_of_path_element in
  let name_str = Stdlib.String.concat "::" names in
  name_str


let fun_name_from_fun_operand (crate : Charon.UllbcAst.crate)
    (operand : Charon.Generated_GAst.fn_operand) : string =
  match operand with
  | FnOpRegular fn_ptr -> (
    match fn_ptr.func with
    | FunId id -> (
      match id with
      | FRegular fun_decl_id ->
          let d = Charon.Types.FunDeclId.Map.find fun_decl_id crate.fun_decls in
          item_meta_to_string d.item_meta
      | FBuiltin _ ->
          L.die UserError "Unsupported function type: FBuiltin" )
    | TraitMethod _ ->
        L.die UserError "Unsupported function type: TraitMethod" )
  | FnOpMove _ ->
      L.die UserError "Unsupported function operand type: FnOpMove"


let params_from_fun_decl (fun_decl : Charon.UllbcAst.blocks Charon.GAst.gfun_decl) (arg_count : int)
    : Textual.VarName.t list =
  match fun_decl.body with
  | Some {locals} -> (
      let locals_list = List.tl locals.locals in
      match locals_list with
      | Some locals_list ->
          List.take locals_list arg_count
          |> List.mapi ~f:(fun i (local : Charon.Generated_GAst.local) ->
                 match local.name with
                 | Some name ->
                     Textual.VarName.of_string (name ^ "_" ^ string_of_int (i + 1))
                 | None ->
                     L.die UserError "Local variable name not found in %a"
                       Charon.Generated_GAst.pp_local local )
      | None ->
          [] )
  | None ->
      []


let proc_name_from_unop (op : Charon.Generated_Expressions.unop) (typ : Textual.Typ.t) :
    Textual.ProcName.t =
  match (op, typ) with
  | Neg _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_neg"
  | Neg _, Textual.Typ.Float ->
      Textual.ProcName.of_string "__sil_neg"
  | Not, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_lnot"
  | _ ->
      L.die UserError "Unsupported unary operator: %a" Charon.Generated_Expressions.pp_unop op


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
  | Shl _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_shiftlt"
  | Shr _, Textual.Typ.Int ->
      Textual.ProcName.of_string "__sil_shiftrt"
  | _ ->
      L.die UserError "Unsupported binary operator: %a" Charon.Generated_Expressions.pp_binop op


let adt_ty_to_textual_typ (ty_decl_ref : Charon.Generated_Types.type_decl_ref) : Textual.Typ.t =
  (* TODO: Implement non-empty tuple and other adt types *)
  match ty_decl_ref.id with
  | TTuple ->
      if List.is_empty ty_decl_ref.generics.types then Textual.Typ.Void
      else
        L.die UserError "Unsupported tuple type: %a" Charon.Generated_Types.pp_type_decl_ref
          ty_decl_ref
  | TBuiltin builtin_ty -> (
    match builtin_ty with
    (* TODO: Charon hardcodes builtin type for now, update this when Charon is updated *)
    | TArray ->
        Textual.Typ.Array Textual.Typ.Int
    | _ ->
        L.die UserError "Unsupported builtin type: %a" Charon.Generated_Types.pp_type_decl_ref
          ty_decl_ref )
  | _ ->
      L.die UserError "Unsupported adt type: %a" Charon.Generated_Types.pp_type_decl_ref ty_decl_ref


let rec ty_to_textual_typ (rust_ty : Charon.Generated_Types.ty) : Textual.Typ.t =
  (* TODO: Bool and char are mapped to int since Textual does not have bool type, see how to handle this *)
  match rust_ty with
  | TLiteral (TInt _) | TLiteral (TUInt _) | TLiteral TBool | TLiteral TChar ->
      Textual.Typ.Int
  | TLiteral (TFloat _) ->
      Textual.Typ.Float
  | TRawPtr (ty, _) | TRef (_, ty, _) ->
      Textual.Typ.Ptr (ty_to_textual_typ ty)
  | TAdt ty_decl_ref ->
      adt_ty_to_textual_typ ty_decl_ref
  | _ ->
      L.die UserError "Unsupported type: %a" Charon.Generated_Types.pp_ty rust_ty


let mk_place_map (locals : Charon.Generated_GAst.local list) : place_map_ty =
  List.foldi locals ~init:PlaceMap.empty ~f:(fun i acc (local : Charon.Generated_GAst.local) ->
      let id = local.index in
      let name =
        match local.name with
        | Some name ->
            name ^ "_" ^ string_of_int i
        | None ->
            "var_" ^ string_of_int i
      in
      let ty = local.var_ty in
      PlaceMap.add (Charon.Generated_Expressions.LocalId.to_int id) (name, ty) acc )


let mk_label (id : int) : Textual.NodeName.t =
  "node_" ^ string_of_int id |> Textual.NodeName.of_string


let mk_locals (locals : Charon.Generated_GAst.local list) (arg_count : int)
    (place_map : place_map_ty) : (Textual.VarName.t * Textual.Typ.annotated) list =
  (* Extracts the local variable names from locals list, excluding the return value and the arguments *)
  locals
  |> fun lst ->
  List.take lst 1 @ List.drop lst (1 + arg_count)
  |> List.map ~f:(fun (l : Charon.Generated_GAst.local) ->
         let id = l.index in
         let name, _ = PlaceMap.find (Charon.Generated_Expressions.LocalId.to_int id) place_map in
         ( Textual.VarName.of_string name
         , Textual.Typ.mk_without_attributes (ty_to_textual_typ l.var_ty) ) )


let mk_procdecl (proc : Charon.UllbcAst.fun_decl) : Textual.ProcDecl.t =
  let qualified_name = mk_qualified_proc_name proc.item_meta in
  let result_type = Textual.Typ.mk_without_attributes (ty_to_textual_typ proc.signature.output) in
  let param_types = List.map proc.signature.inputs ~f:ty_to_textual_typ in
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


let mk_place_from_id (id : int) (ty : Charon.Generated_Types.ty) :
    Charon.Generated_Expressions.place =
  {kind= PlaceLocal (Charon.Generated_Expressions.LocalId.of_int id); ty}


let rec mk_exp_from_place (place_map : place_map_ty) (place : Charon.Generated_Expressions.place) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  let temp_id = Textual.Ident.fresh !ident_set in
  let temp_exp = Textual.Exp.Var temp_id in
  let temp_typ = ty_to_textual_typ place.ty in
  let temp_loc = Textual.Location.Unknown in
  ident_set := Textual.Ident.Set.add temp_id !ident_set ;
  match place.kind with
  | PlaceLocal var_id ->
      let id = Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id) in
      let exp =
        Textual.Exp.Lvar
          (Textual.VarName.of_string (fst (PlaceMap.find (Textual.Ident.to_int id) place_map)))
      in
      let load_instr = Textual.Instr.Load {id= temp_id; exp; typ= Some temp_typ; loc= temp_loc} in
      ([load_instr], temp_exp, temp_typ)
  | PlaceProjection (projection_place, projection_elem) -> (
    match projection_elem with
    | Deref ->
        let instrs, exp, _ = mk_exp_from_place place_map projection_place in
        let load_instr = Textual.Instr.Load {id= temp_id; exp; typ= Some temp_typ; loc= temp_loc} in
        (instrs @ [load_instr], temp_exp, temp_typ)
    | _ ->
        L.die UserError "Unsupported projection element: %a"
          Charon.Generated_Expressions.pp_projection_elem projection_elem )
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
      let textual_ty = ty_to_textual_typ rust_ty in
      let exp = mk_const_exp rust_ty value in
      ([], exp, textual_ty)


let mk_exp_from_rvalue (rvalue : Charon.Generated_Expressions.rvalue) (place_map : place_map_ty) :
    Textual.Instr.t list * Textual.Exp.t * Textual.Typ.t =
  match rvalue with
  | UnaryOp (op, operand) ->
      let instrs, exp, typ = mk_exp_from_operand place_map operand in
      let proc_name = proc_name_from_unop op typ in
      let qualified_proc_name =
        { Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel
        ; name= proc_name }
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp] in
      (instrs, call, typ)
  | BinaryOp (op, operand1, operand2) ->
      let instrs1, exp1, typ1 = mk_exp_from_operand place_map operand1 in
      let instrs2, exp2, _ = mk_exp_from_operand place_map operand2 in
      let proc_name = proc_name_from_binop op typ1 in
      let qualified_proc_name =
        { Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel
        ; name= proc_name }
      in
      let call = Textual.Exp.call_non_virtual qualified_proc_name [exp1; exp2] in
      (instrs1 @ instrs2, call, typ1)
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
  | RawPtr (place, _) | RvRef (place, _) -> (
    match place.kind with
    | PlaceLocal var_id ->
        let id = Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id) in
        let exp =
          Textual.Exp.Lvar
            (Textual.VarName.of_string (fst (PlaceMap.find (Textual.Ident.to_int id) place_map)))
        in
        let typ = Textual.Typ.Ptr (ty_to_textual_typ place.ty) in
        ([], exp, typ)
    | PlaceProjection (projection_place, projection_elem) -> (
      match projection_elem with
      | Deref ->
          mk_exp_from_place place_map projection_place
      | _ ->
          L.die UserError "Unsupported projection element: %a"
            Charon.Generated_Expressions.pp_projection_elem projection_elem )
    | _ ->
        L.die UserError "Unsupported place: %a" Charon.Generated_Expressions.pp_place place )
  | _ ->
      L.die UserError "Unsupported rvalue: %a" Charon.Generated_Expressions.pp_rvalue rvalue


let mk_terminator (crate : Charon.UllbcAst.crate) (place_map : place_map_ty)
    (terminator : Charon.Generated_UllbcAst.terminator) :
    Textual.Instr.t list * Textual.Terminator.t =
  match terminator.content with
  | Charon.Generated_UllbcAst.Goto block_id ->
      let label = mk_label (Charon.Generated_UllbcAst.BlockId.to_int block_id) in
      let ssa_args = [] in
      let node_call : Textual.Terminator.node_call = {label; ssa_args} in
      ([], Textual.Terminator.Jump [node_call])
  | Charon.Generated_UllbcAst.Return ->
      let _, ty = PlaceMap.find 0 place_map in
      let place = mk_place_from_id 0 ty in
      let instrs, exp, _ = mk_exp_from_place place_map place in
      (instrs, Textual.Terminator.Ret exp)
  | Charon.Generated_UllbcAst.Switch (operand, switch) -> (
    match switch with
    | SwitchInt (_, _, _) ->
        L.die UserError "Unsupported switch type: SwitchInt"
    | If (then_block_id, else_block_id) ->
        let instrs, exp, _ = mk_exp_from_operand place_map operand in
        let then_label = mk_label (Charon.Generated_UllbcAst.BlockId.to_int then_block_id) in
        let else_label = mk_label (Charon.Generated_UllbcAst.BlockId.to_int else_block_id) in
        let bexp = Textual.BoolExp.Exp exp in
        let then_node_call : Textual.Terminator.node_call = {label= then_label; ssa_args= []} in
        let else_node_call : Textual.Terminator.node_call = {label= else_label; ssa_args= []} in
        let then_ = Textual.Terminator.Jump [then_node_call] in
        let else_ = Textual.Terminator.Jump [else_node_call] in
        (instrs, Textual.Terminator.If {bexp; then_; else_}) )
  | Charon.Generated_UllbcAst.Call (call, block_id_1, _) ->
      let arg_evals = List.map call.args ~f:(mk_exp_from_operand place_map) in
      let arg_instrs = List.concat_map arg_evals ~f:(fun (instrs, _, _) -> instrs) in
      let arg_exps = List.map arg_evals ~f:(fun (_, exp, _) -> exp) in
      let proc_name = Textual.ProcName.of_string (fun_name_from_fun_operand crate call.func) in
      let qualified_proc_name =
        { Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel
        ; name= proc_name }
      in
      let dest =
        match call.dest.kind with
        | PlaceLocal var_id ->
            Textual.Ident.of_int (Charon.Generated_Expressions.LocalId.to_int var_id)
        | _ ->
            L.die UserError "Unsupported place: %a" Charon.Generated_Expressions.pp_place call.dest
      in
      let exp1 =
        Textual.Exp.Lvar
          (Textual.VarName.of_string (fst (PlaceMap.find (Textual.Ident.to_int dest) place_map)))
      in
      let exp2 =
        Textual.Exp.Call {proc= qualified_proc_name; args= arg_exps; kind= Textual.Exp.NonVirtual}
      in
      let temp_id = Textual.Ident.fresh !ident_set in
      let temp_exp = Textual.Exp.Var temp_id in
      let call_instr =
        Textual.Instr.Let {id= Some temp_id; exp= exp2; loc= Textual.Location.Unknown}
      in
      let store_instr =
        Textual.Instr.Store
          { exp1
          ; typ= Some (ty_to_textual_typ call.dest.ty)
          ; exp2= temp_exp
          ; loc= Textual.Location.Unknown }
      in
      let label = mk_label (Charon.Generated_UllbcAst.BlockId.to_int block_id_1) in
      let ssa_args = [] in
      let node_call : Textual.Terminator.node_call = {label; ssa_args} in
      ident_set := Textual.Ident.Set.add temp_id !ident_set ;
      (arg_instrs @ [call_instr] @ [store_instr], Textual.Terminator.Jump [node_call])
  | Charon.Generated_UllbcAst.UnwindResume ->
      (* TODO:To be updated when error handling is being implemented *)
      ([], Textual.Terminator.Unreachable)
  | t ->
      L.die UserError "Unsupported terminator: %a" Charon.Generated_UllbcAst.pp_raw_terminator t


let mk_instr (place_map : place_map_ty) (statement : Charon.Generated_UllbcAst.statement) :
    Textual.Instr.t list =
  let loc = location_from_span statement.span in
  match statement.content with
  | Assign (lhs, rhs) -> (
    match lhs.kind with
    | PlaceLocal var_id ->
        let id = Charon.Generated_Expressions.LocalId.to_int var_id in
        let name, _ = PlaceMap.find id place_map in
        let exp1 = Textual.Exp.Lvar (Textual.VarName.of_string name) in
        let instrs, exp2, typ = mk_exp_from_rvalue rhs place_map in
        let store_instr = Textual.Instr.Store {exp1; typ= Some typ; exp2; loc} in
        instrs @ [store_instr]
    | _ ->
        L.die UserError "Unsupported place: %a" Charon.Generated_Expressions.pp_place lhs )
  | StorageDead _ ->
      []
  | StorageLive _ ->
      []
  | Drop (place, _) ->
      let instrs, exp, typ = mk_exp_from_place place_map place in
      let temp_id = Textual.Ident.fresh !ident_set in
      let temp_exp = Textual.Exp.Var temp_id in
      let assign_instr = Textual.Instr.Store {exp1= temp_exp; typ= Some typ; exp2= exp; loc} in
      let free_proc_name = Textual.ProcName.of_string "__sil_free" in
      let qualified_free_name =
        { Textual.QualifiedProcName.enclosing_class= Textual.QualifiedProcName.TopLevel
        ; name= free_proc_name }
      in
      let free_call = Textual.Exp.call_non_virtual qualified_free_name [temp_exp] in
      let free_instr = Textual.Instr.Let {id= None; exp= free_call; loc} in
      ident_set := Textual.Ident.Set.add temp_id !ident_set ;
      instrs @ [assign_instr; free_instr]
  | s ->
      L.die UserError "Unsupported statement: %a" Charon.Generated_UllbcAst.pp_raw_statement s


let mk_node (crate : Charon.UllbcAst.crate) (idx : int) (block : Charon.Generated_UllbcAst.block)
    (place_map : place_map_ty) : Textual.Node.t =
  let label = mk_label idx in
  (*TODO Should be retrieved from Γ *)
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
  let procdecl = mk_procdecl fun_decl in
  let nodes = List.mapi blocks ~f:(fun i block -> mk_node crate i block place_map) in
  let start = mk_label 0 in
  let params = params_from_fun_decl fun_decl arg_count in
  let locals = mk_locals locals arg_count place_map in
  let exit_loc = location_from_span_end fun_decl.item_meta.span in
  {Textual.ProcDesc.procdecl; nodes; start; params; locals; exit_loc}


let mk_module (crate : Charon.UllbcAst.crate) json_file : Textual.Module.t =
  let fun_decls = crate.fun_decls in
  let attrs = [Textual.Attr.mk_source_language Rust] in
  let decls =
    Charon.Generated_Types.FunDeclId.Map.bindings fun_decls
    |> List.map ~f:(mk_procdesc crate)
    |> List.map ~f:(fun p -> Textual.Module.Proc p)
  in
  let sourcefile = Textual.SourceFile.create json_file in
  {Textual.Module.attrs; decls; sourcefile}
