(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module F = Format
module Type = Llair2TextualType
module L = Logging
module ProcState = Llair2TextualProcState
module VarMap = Textual.VarName.Map
module IdentMap = Textual.Ident.Map

let swift_weak_assign = Textual.ProcName.of_string "swift_weakAssign"

let llvm_dynamic_call = Textual.ProcName.of_string "llvm_dynamic_call"

let get_alloc_class_name =
  let alloc_object = Textual.ProcName.of_string "swift_allocObject" in
  fun ~proc_state proc_name ->
    if Textual.ProcName.equal proc_name alloc_object then
      Textual.QualifiedProcName.get_class_name proc_state.ProcState.qualified_name
    else None


let builtin_qual_proc_name name : Textual.QualifiedProcName.t =
  { enclosing_class= Enclosing (Textual.TypeName.of_string "$builtins")
  ; name= Textual.ProcName.of_string name }


module Var = struct
  let string_name_of_reg reg =
    let name = Reg.name reg in
    match Int.of_string_opt name with Some i -> Format.sprintf "var%d" (i + 1) | None -> name


  let reg_to_var_name reg = Textual.VarName.of_string (string_name_of_reg reg)

  let reg_to_id ~proc_state reg =
    let id = ProcState.mk_fresh_id ~reg proc_state in
    let reg_typ =
      Type.to_textual_typ proc_state.ProcState.lang ~struct_map:proc_state.ProcState.struct_map
        (Reg.typ reg)
    in
    (id, reg_typ)


  let add_fresh_id ~proc_state () = ProcState.mk_fresh_id proc_state

  let find_formal_type ~proc_state reg_var_name =
    VarMap.fold
      (fun _ (annot_typ, local) acc ->
        match local with
        | Some local when Textual.VarName.equal local reg_var_name ->
            Some annot_typ.Textual.Typ.typ
        | _ ->
            acc )
      proc_state.ProcState.formals None


  let reg_to_textual_var ~(proc_state : ProcState.t) reg =
    let reg_var_name = reg_to_var_name reg in
    match VarMap.find_opt reg_var_name proc_state.formals with
    | Some (annot_typ, Some local) ->
        (Textual.Exp.Lvar local, Some annot_typ.Textual.Typ.typ)
    | Some (annot_typ, None) ->
        (Textual.Exp.Lvar reg_var_name, Some annot_typ.Textual.Typ.typ)
    | None ->
        if VarMap.mem reg_var_name proc_state.locals then
          (Textual.Exp.Lvar reg_var_name, find_formal_type ~proc_state reg_var_name)
        else (Textual.Exp.Var (reg_to_id ~proc_state reg |> fst), None)


  let reg_to_annot_typ lang ~struct_map reg =
    Type.to_annotated_textual_typ lang ~struct_map (Reg.typ reg)
end

let to_textual_loc ?proc_state {Loc.line; col} =
  if Int.equal line 0 && Int.equal col 0 then
    let line =
      if Config.frontend_tests then ProcState.get_fresh_fake_line ()
      else
        match proc_state with
        | Some proc_state -> (
          match proc_state.ProcState.loc with
          | Textual.Location.Known {line= proc_line; _} ->
              proc_line
          | _ ->
              line )
        | None ->
            line
    in
    Textual.Location.Known {line; col}
  else Textual.Location.Known {line; col}


let is_loc_default loc =
  match loc with
  | Textual.Location.Known {line; col} ->
      Int.equal line 0 && Int.equal col 0
  | _ ->
      false


let to_textual_loc_instr ~(proc_state : ProcState.t) loc =
  let loc = to_textual_loc ~proc_state loc in
  if is_loc_default loc then proc_state.loc else loc


let build_globals_map globals =
  let add_global map global =
    let name = global.GlobalDefn.name |> Global.name |> Textual.VarName.of_string in
    Textual.VarName.Map.add name global map
  in
  let globals = StdUtils.iarray_to_list globals in
  List.fold ~f:add_global globals ~init:Textual.VarName.Map.empty


let to_qualified_proc_name ?loc func_name =
  let proc_name = Textual.ProcName.of_string ?loc func_name in
  let enclosing_class =
    match Textual.ProcName.Hashtbl.find_opt ProcState.method_class_index proc_name with
    | Some class_name ->
        Textual.QualifiedProcName.Enclosing class_name
    | None ->
        Textual.QualifiedProcName.TopLevel
  in
  Textual.QualifiedProcName.{enclosing_class; name= proc_name}


let to_name_attr func_name =
  Option.map ~f:Textual.Attr.mk_plain_name (FuncName.unmangled_name func_name)


let to_formal_types lang ~struct_map func =
  let to_textual_formal_type formal_type = Var.reg_to_annot_typ lang ~struct_map formal_type in
  let to_textual_formal_signature_type formal formal_type =
    let typ = Type.signature_type_to_textual_typ lang formal_type in
    let typ = Option.map ~f:Textual.Typ.mk_without_attributes typ in
    match typ with Some typ -> typ | None -> to_textual_formal_type formal
  in
  let llair_formals = StdUtils.iarray_to_list func.Llair.formals in
  let llair_formals_types = StdUtils.iarray_to_list func.Llair.formals_types in
  let formals_signature_types =
    List.map2 ~f:to_textual_formal_signature_type llair_formals llair_formals_types
  in
  (* We try using the signature types, but sometimes they don't match the number of arguments,
     in that case we revert to the internal types *)
  match formals_signature_types with
  | List.Or_unequal_lengths.Unequal_lengths ->
      List.map ~f:to_textual_formal_type llair_formals
  | List.Or_unequal_lengths.Ok formals_ ->
      formals_


let update_signature_types lang ~struct_map formal_types return_type =
  let update_signature_type typ =
    let typ =
      Type.update_type
        ~update_struct_name:(Type.update_signature_type lang struct_map)
        typ.Textual.Typ.typ
    in
    Textual.Typ.mk_without_attributes typ
  in
  let update formal_types = List.map ~f:update_signature_type formal_types in
  let formal_types = Option.map ~f:update formal_types in
  let return_type = update_signature_type return_type in
  (formal_types, return_type)


let block_to_node_name block =
  let name = block.Llair.lbl in
  Textual.NodeName.of_string name


let undef_proc_name = builtin_qual_proc_name "llvm_nondet"

let undef_exp ~sourcefile ~loc ?typ ~proc exp =
  let pp_typ fmt typ = Option.iter typ ~f:(fun typ -> F.fprintf fmt ":%a" Textual.Typ.pp typ) in
  L.internal_error "Llair2Textual: unsupported exp: %a%a in proc %a in %a at %a@\n" Llair.Exp.pp exp
    pp_typ typ Textual.QualifiedProcName.pp proc SourceFile.pp sourcefile Textual.Location.pp loc ;
  (* TODO: should include the arguments here too *)
  (Textual.Exp.Call {proc= undef_proc_name; args= []; kind= NonVirtual}, typ, [])


let to_textual_arith_exp_builtin ~loc ~sourcefile (op : Llair.Exp.op2) (typ : Llair.Typ.t) =
  let sil_binop : Binop.t option =
    match (op, typ) with
    | Add, Integer _ ->
        Some (PlusA (Some IInt))
    | Add, Pointer _ ->
        Some PlusPI
    | Sub, Integer _ ->
        Some (MinusA (Some IInt))
    | Mul, Integer _ ->
        Some (Mult (Some IInt))
    | Div, Integer _ ->
        Some DivI
    | Div, Float _ ->
        Some DivF
    | Rem, Integer _ ->
        Some Mod
    | _ ->
        None
  in
  match sil_binop with
  | Some binop ->
      Textual.ProcDecl.of_binop binop
  | None ->
      L.internal_error "Llair2Textual: unsupported op2: %a in %a at %a@\n" Llair.Exp.pp_op2 op
        SourceFile.pp sourcefile Textual.Location.pp loc ;
      undef_proc_name


let to_textual_bool_exp_builtin (op : Llair.Exp.op2) =
  let sil_bin_op =
    match op with
    | Eq ->
        Binop.Eq
    | Dq ->
        Binop.Ne
    | Gt ->
        Binop.Gt
    | Ge ->
        Binop.Ge
    | Le ->
        Binop.Le
    | Lt ->
        Binop.Lt
    | And ->
        Binop.LAnd
    | Or ->
        Binop.LOr
    | Xor ->
        Binop.BXor
    | Shl ->
        Binop.Shiftlt
    | Lshr ->
        Binop.Shiftrt
    | Ashr ->
        Binop.Shiftrt
    | _ ->
        assert false
  in
  Textual.ProcDecl.of_binop sil_bin_op


let add_deref ~proc_state ?from_call exp loc =
  let id = Var.add_fresh_id ~proc_state () in
  let add_load_instr =
    let instr = Textual.Instr.Load {id; exp; typ= None; loc} in
    ([instr], Textual.Exp.Var id)
  in
  match exp with
  | Textual.Exp.Lvar var_name ->
      ( if Option.is_some from_call then
          let typ_opt = Var.find_formal_type ~proc_state var_name in
          match typ_opt with
          | Some typ ->
              ProcState.update_ids_types ~proc_state id (Textual.Typ.mk_without_attributes typ)
          | None ->
              () ) ;
      add_load_instr
  | Textual.Exp.Field _ ->
      add_load_instr
  | Textual.Exp.Var id -> (
      let typ = IdentMap.find_opt id proc_state.ProcState.ids_move in
      match typ with Some {typ= Textual.Typ.Ptr _} -> add_load_instr | _ -> ([], exp) )
  | _ ->
      ([], exp)


let update_id_return_type ~proc_state proc id =
  let proc_decl_opt = Textual.QualifiedProcName.Map.find_opt proc proc_state.ProcState.proc_map in
  match (id, proc_decl_opt) with
  | Some id, Some proc_decl ->
      let return_typ = proc_decl.Textual.ProcDecl.result_type in
      ProcState.update_ids_types ~proc_state id return_typ
  | _ ->
      ()


let rec to_textual_exp ~(proc_state : ProcState.t) loc ?generate_typ_exp (exp : Llair.Exp.t) :
    Textual.Exp.t * Textual.Typ.t option * Textual.Instr.t list =
  let struct_map = proc_state.ProcState.struct_map in
  match exp with
  | Integer {data; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then Textual.Exp.Typ textual_typ
        else if NS.Z.is_false data && not (Llair.Typ.is_int typ) then Textual.Exp.Const Null
        else Textual.Exp.Const (Int data)
      in
      (textual_exp, Some textual_typ, [])
  | Float {data; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then
          Textual.Exp.Typ (Type.to_textual_typ proc_state.lang ~struct_map typ)
        else Textual.Exp.Const (Float (Float.of_string data))
      in
      (textual_exp, Some textual_typ, [])
  | Nondet {typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name
        ~typ:textual_typ exp
  | FuncName {name} ->
      (Textual.Exp.Const (Str name), None, [])
  | Reg {id; name; typ} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp, var_typ = Var.reg_to_textual_var ~proc_state (Reg.mk typ id name) in
      let typ =
        match var_typ with
        | Some var_typ
          when (Textual.Typ.is_pointer textual_typ && not (Textual.Typ.is_pointer var_typ))
               || not (Textual.Typ.is_pointer textual_typ && Textual.Typ.is_pointer var_typ) ->
            textual_typ
        | Some var_typ ->
            var_typ
        | _ ->
            textual_typ
      in
      (textual_exp, Some typ, [])
  | Global {name; typ; is_constant} ->
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let textual_exp =
        match textual_typ with
        | Textual.Typ.Ptr _ when is_constant -> (
            let string_opt =
              match
                VarMap.find_opt (Textual.VarName.of_string name) proc_state.ProcState.globals
              with
              | Some global ->
                  Option.bind ~f:Llair.Exp.string_of_exp global.Llair.GlobalDefn.init
              | None ->
                  None
            in
            match string_opt with
            | Some s ->
                Textual.Exp.Const (Str s)
            | None ->
                Textual.Exp.Lvar (Textual.VarName.of_string name) )
        | _ ->
            Textual.Exp.Lvar (Textual.VarName.of_string name)
      in
      (textual_exp, Some textual_typ, [])
  | Ap1 (Select n, typ, llair_exp) -> (
      let typ_name =
        match typ with
        | Struct {name} ->
            Some (Type.struct_name_of_mangled_name proc_state.lang struct_map name)
        | Tuple _ -> (
          match Type.to_textual_typ proc_state.lang ~struct_map typ with
          | Textual.Typ.(Ptr (Struct name)) ->
              Some name
          | _ ->
              None )
        | _ ->
            None
      in
      match typ_name with
      | None ->
          undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name exp
      | Some typ_name ->
          let exp, _, exp_instrs = to_textual_exp loc ~proc_state llair_exp in
          let field =
            if Llair.Typ.is_tuple typ then Type.tuple_field_of_pos typ_name n
            else Type.field_of_pos typ_name n
          in
          let field_instrs, exp = add_deref ~proc_state exp loc in
          let instrs = List.append field_instrs exp_instrs in
          let exp = Textual.Exp.Field {exp; field} in
          let typ = Type.lookup_field_type ~struct_map typ_name field in
          (exp, typ, instrs) )
  | Ap1 (GetElementPtr n, _typ, _exp) ->
      let n_arg = Llair.Exp.integer (Llair.Typ.integer ~bits:32 ~byts:4) (Z.of_int n) in
      let exp, _, instrs = to_textual_exp loc ~proc_state n_arg in
      let var_name =
        ProcState.mk_fresh_tmp_var ProcState.get_element_ptr_offset_prefix proc_state
      in
      let new_var = Textual.Exp.Lvar var_name in
      ProcState.update_locals ~proc_state var_name (Textual.Typ.mk_without_attributes (Ptr Void)) ;
      ProcState.update_var_offset ~proc_state var_name n ;
      let store_instr = Textual.Instr.Store {exp1= new_var; exp2= exp; typ= None; loc} in
      (new_var, None, store_instr :: instrs)
  | Ap1 ((Convert _ | Signed _ | Unsigned _), dst_typ, exp) ->
      (* Signed is the translation of llvm's trunc and SExt and Unsigned is the translation of ZExt, all different types of cast,
         and convert translates other types of cast *)
      let exp, _, instrs = to_textual_exp loc ~proc_state exp in
      let deref_instrs, exp = add_deref ~proc_state exp loc in
      let textual_dst_typ = Type.to_textual_typ proc_state.lang ~struct_map dst_typ in
      let proc = Textual.ProcDecl.cast_name in
      let instrs = List.append instrs deref_instrs in
      ( Call {proc; args= [Textual.Exp.Typ textual_dst_typ; exp]; kind= Textual.Exp.NonVirtual}
      , None
      , instrs )
  | Ap1 (Splat, _, _) ->
      (* [splat exp] initialises every element of an array with the element exp, so to be precise it
         needs to be translated as a loop. We translate here to a non-deterministic value for the array *)
      let proc = undef_proc_name in
      (Call {proc; args= []; kind= Textual.Exp.NonVirtual}, None, [])
  | Ap2 (((Add | Sub | Mul | Div | Rem) as op), typ, e1, e2) ->
      let proc = to_textual_arith_exp_builtin ~loc ~sourcefile:proc_state.sourcefile op typ in
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let deref_instrs1, exp1 = add_deref ~proc_state exp1 loc in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      let deref_instrs2, exp2 = add_deref ~proc_state exp2 loc in
      ( Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
      , typ1
      , deref_instrs1 @ exp1_instrs @ deref_instrs2 @ exp2_instrs )
  | Ap2 (((Eq | Dq | Gt | Ge | Le | Lt | And | Or | Xor | Shl | Lshr | Ashr) as op), _, e1, e2) ->
      let proc = to_textual_bool_exp_builtin op in
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let deref_instrs1, exp1 = add_deref ~proc_state exp1 loc in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      let deref_instrs2, exp2 = add_deref ~proc_state exp2 loc in
      ( Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
      , typ1
      , deref_instrs1 @ exp1_instrs @ deref_instrs2 @ exp2_instrs )
  | Ap2 (Update idx, typ, rcd, elt) ->
      let rcd_exp, _, rcd_instrs = to_textual_exp loc ~proc_state rcd in
      let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state elt in
      let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
      let elt_instrs = List.append elt_instrs elt_deref_instrs in
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let type_name =
        match textual_typ with
        | Textual.Typ.(Ptr (Struct name)) ->
            name
        | _ ->
            L.die InternalError "Llair2Textual: unexpected type %a in %a at %a" Textual.Typ.pp
              textual_typ SourceFile.pp proc_state.sourcefile Textual.Location.pp loc
      in
      let index_exp =
        Textual.Exp.Field {exp= rcd_exp; field= Type.tuple_field_of_pos type_name idx}
      in
      let store_instr =
        Textual.Instr.Store {exp1= index_exp; exp2= elt_exp_deref; typ= None; loc}
      in
      (rcd_exp, Some textual_typ, List.append (List.append rcd_instrs elt_instrs) [store_instr])
  | Ap3 (Conditional, _typ, cond_exp, then_exp, else_exp) ->
      let cond_exp, _, cond_instrs = to_textual_bool_exp loc ~proc_state cond_exp in
      let then_exp, _, then_instrs = to_textual_exp loc ~proc_state then_exp in
      let else_exp, _, else_instrs = to_textual_exp loc ~proc_state else_exp in
      let exp = Textual.Exp.If {cond= cond_exp; then_= then_exp; else_= else_exp} in
      (exp, None, cond_instrs @ then_instrs @ else_instrs)
  | ApN (Record, typ, _elements) -> (
      let textual_typ = Type.to_textual_typ proc_state.lang ~struct_map typ in
      let type_name_opt =
        match textual_typ with
        | Textual.Typ.(Ptr (Struct name)) | Textual.Typ.Struct name ->
            Some name
        | _ ->
            (* skipping arrays for now *)
            None
      in
      match type_name_opt with
      | None ->
          undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name exp
      | Some type_name ->
          let elements = StdUtils.iarray_to_list _elements in
          let id = Var.add_fresh_id ~proc_state () in
          let rcd_exp = Textual.Exp.Var id in
          let undef_exp =
            let proc = builtin_qual_proc_name "llvm_init_tuple" in
            Textual.Exp.Call {proc; args= []; kind= NonVirtual}
          in
          let rcd_store_instr = Textual.Instr.Let {id= Some id; exp= undef_exp; loc} in
          (* for each element in the record we set the value to a field of the record variable. *)
          let to_textual_exp_index idx acc_instrs element =
            let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state element in
            let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
            let elt_instrs = List.append elt_instrs elt_deref_instrs in
            let field =
              if Llair.Typ.is_tuple typ then Type.tuple_field_of_pos type_name idx
              else Type.field_of_pos type_name idx
            in
            let index_exp = Textual.Exp.Field {exp= rcd_exp; field} in
            let store_instr =
              Textual.Instr.Store {exp1= index_exp; exp2= elt_exp_deref; typ= None; loc}
            in
            List.append acc_instrs (List.append elt_instrs [store_instr])
          in
          let instrs = rcd_store_instr :: List.foldi ~f:to_textual_exp_index ~init:[] elements in
          (rcd_exp, Some textual_typ, instrs) )
  | _ ->
      undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name exp


and to_textual_bool_exp ~proc_state loc exp =
  let textual_exp, textual_typ_opt, instrs = to_textual_exp loc ~proc_state exp in
  (Textual.BoolExp.Exp textual_exp, textual_typ_opt, instrs)


and resolve_method_call ~proc_state proc args =
  match args with
  | Textual.Exp.Var id_offset :: args -> (
    match (proc_state.ProcState.id_offset, List.hd (List.rev args)) with
    | Some (proc_state_id_offset, offset), Some (Textual.Exp.Var self_id)
      when Textual.Ident.equal proc_state_id_offset id_offset
           && Textual.ProcName.equal (Textual.QualifiedProcName.name proc) llvm_dynamic_call -> (
      match ProcState.IdentMap.find_opt self_id proc_state.ProcState.ids_types with
      | Some {typ= Textual.Typ.Ptr (Textual.Typ.Struct struct_name as inner_typ)}
        when not (Textual.Typ.equal Textual.Typ.any_type_swift inner_typ) -> (
        match ProcState.find_method_with_offset ~proc_state struct_name offset with
        | Some proc_name ->
            ProcState.reset_offsets ~proc_state ;
            Some (Textual.Exp.Call {proc= proc_name; args; kind= Textual.Exp.Virtual})
        | None ->
            None )
      | _ ->
          None )
    | _ ->
        None )
  | _ ->
      None


and to_textual_call_aux ~proc_state ~kind proc return ?generate_typ_exp (args : Llair.Exp.t list)
    (loc : Textual.Location.t) =
  let args_instrs, args =
    List.fold_map
      ~f:(fun acc_instrs exp ->
        let exp, _, instrs = to_textual_exp loc ~proc_state ?generate_typ_exp exp in
        let deref_instrs, deref_exp =
          (* So far it looks like for C the load operations are already there when needed. *)
          if
            Textual.Lang.is_swift proc_state.ProcState.lang
            && not (Textual.ProcName.equal proc.Textual.QualifiedProcName.name swift_weak_assign)
          then add_deref ~proc_state ~from_call:true exp loc
          else ([], exp)
        in
        (List.append deref_instrs (List.append acc_instrs instrs), deref_exp) )
      args ~init:[]
  in
  let args, args_instrs =
    if Textual.QualifiedProcName.equal proc Textual.ProcDecl.assert_fail_name then
      ([Textual.Exp.Const Null], [])
    else (args, args_instrs)
  in
  let return_id =
    Option.map return ~f:(fun reg ->
        let id = Var.reg_to_id ~proc_state reg |> fst in
        id )
  in
  let functions_to_skip = ["swift_unknownObjectRetain"; "swift_weakLoadStrong"] in
  let call_exp =
    if
      (* skip calls to the elements in functions_to_skip  and returns its first argument instead. *)
      List.exists
        ~f:(fun f ->
          Textual.ProcName.equal proc.Textual.QualifiedProcName.name (Textual.ProcName.of_string f) )
        functions_to_skip
    then List.hd_exn args
    else
      match resolve_method_call ~proc_state proc args with
      | Some call_instrs ->
          call_instrs
      | None ->
          Textual.Exp.Call {proc; args; kind}
  in
  update_id_return_type ~proc_state proc return_id ;
  (return_id, call_exp, args_instrs)


and to_textual_call_instrs ~proc_state return proc args loc =
  let id, call_exp, args_instrs =
    to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc return args loc
  in
  let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
  let_instr :: args_instrs


and to_textual_builtin ~proc_state return name args loc =
  let proc = builtin_qual_proc_name name in
  to_textual_call_instrs ~proc_state return proc args loc


and to_textual_call ~proc_state (call : 'a Llair.call) =
  let args = StdUtils.iarray_to_list call.actuals in
  let proc, kind, exp_opt =
    match call.callee with
    | Direct {func} ->
        let proc =
          if
            String.equal (FuncName.name func.Llair.name)
              (Procname.get_method BuiltinDecl.__assert_fail)
            || String.is_substring ~substring:"assertionFailure" (FuncName.name func.Llair.name)
          then Textual.ProcDecl.assert_fail_name
          else to_qualified_proc_name (FuncName.name func.Llair.name)
        in
        (proc, Textual.Exp.NonVirtual, None)
    | Indirect {ptr} ->
        let proc = builtin_qual_proc_name (Textual.ProcName.to_string llvm_dynamic_call) in
        (proc, Textual.Exp.NonVirtual, Some ptr)
    | Intrinsic intrinsic ->
        let proc = builtin_qual_proc_name (Llair.Intrinsic.to_name intrinsic) in
        (proc, Textual.Exp.NonVirtual, None)
  in
  let loc = to_textual_loc_instr ~proc_state call.loc in
  let args = Option.to_list exp_opt @ args in
  let id, call_exp, args_instrs =
    to_textual_call_aux ~proc_state ~kind proc call.areturn args loc
  in
  let call_exp =
    match call_exp with
    | Textual.Exp.Call {proc} -> (
      match get_alloc_class_name ~proc_state (Textual.QualifiedProcName.name proc) with
      | Some class_name ->
          let args = [Textual.Exp.Typ (Textual.Typ.Struct class_name)] in
          Textual.Exp.Call
            {proc= Textual.ProcDecl.swift_alloc_name; args; kind= Textual.Exp.NonVirtual}
      | None ->
          call_exp )
    | _ ->
        call_exp
  in
  (* Replace swift_weakAssign with a store instruction. We do not add dereference to the first argument
  because we are flattenning the structure of weak pointers to be just like normal pointers in infer,
  whilst in llvm the structures is field_2: *swift::weak}, type swift::weak = {field_0: *ptr_elt} *)
  let instrs =
    match call_exp with
    | Textual.Exp.Call {proc; args= [arg1; arg2]}
      when Textual.ProcName.equal proc.Textual.QualifiedProcName.name swift_weak_assign ->
        let instrs2, arg2 = add_deref ~proc_state arg2 loc in
        Textual.Instr.Store {exp1= arg1; typ= None; exp2= arg2; loc} :: instrs2
    | _ ->
        [Textual.Instr.Let {id; exp= call_exp; loc}]
  in
  instrs @ args_instrs


let remove_store_zero_in_class typ_exp1 exp2 =
  let any_type_llvm_name =
    match Textual.Typ.any_type_llvm with Textual.Typ.Struct name -> name | _ -> assert false
  in
  match typ_exp1 with
  | Some (Textual.Typ.Ptr (Struct type_name))
    when Textual.Exp.is_zero_exp exp2 && not (Textual.TypeName.equal type_name any_type_llvm_name)
    ->
      true
  | _ ->
      false


let translate_move ~move_phi ~proc_state loc textual_instrs reg_exps =
  let reg_exps = StdUtils.iarray_to_list reg_exps in
  let loc = to_textual_loc_instr ~proc_state loc in
  let instrs =
    List.fold
      ~f:(fun instrs (reg, exp) ->
        let id = Some (Var.reg_to_id ~proc_state reg |> fst) in
        let exp, exp_typ, exp_instrs = to_textual_exp loc ~proc_state exp in
        ( match (id, exp_typ) with
        | Some id, Some exp_typ ->
            ProcState.update_ids_move ~proc_state id (Textual.Typ.mk_without_attributes exp_typ)
        | _ ->
            () ) ;
        let deref_instrs, exp = if move_phi then add_deref ~proc_state exp loc else ([], exp) in
        let instrs = List.append instrs (List.append deref_instrs exp_instrs) in
        Textual.Instr.Let {id; exp; loc} :: instrs )
      ~init:[] reg_exps
  in
  List.append instrs textual_instrs


let is_store_formal_to_local ~(proc_state : ProcState.t) exp1 exp2 =
  match (exp1, exp2) with
  | Llair.Exp.Reg {name= name1; id= id1; typ= typ1}, Llair.Exp.Reg {name= name2; id= id2; typ= typ2}
    ->
      let reg1 = Reg.mk typ1 id1 name1 in
      let reg2 = Reg.mk typ2 id2 name2 in
      let name1 = Var.reg_to_var_name reg1 in
      let name2 = Var.reg_to_var_name reg2 in
      if
        Textual.VarName.Map.mem name2 proc_state.ProcState.formals
        && Textual.VarName.Map.mem name1 proc_state.ProcState.locals
      then (
        let typ1 =
          Type.to_annotated_textual_typ proc_state.lang ~struct_map:proc_state.struct_map typ1
        in
        ProcState.subst_formal_local ~proc_state ~formal:name2 ~local:(name1, typ1) ;
        true )
      else false
  | _ ->
      false


let cmnd_to_instrs ~(proc_state : ProcState.t) block =
  let struct_map = proc_state.ProcState.struct_map in
  let to_instr textual_instrs inst =
    match inst with
    | Load {reg; ptr; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let id, _ = Var.reg_to_id ~proc_state reg in
        let exp, _, ptr_instrs = to_textual_exp loc ~proc_state ptr in
        ProcState.update_id_offset ~proc_state id exp ;
        let textual_instr = Textual.Instr.Load {id; exp; typ= None; loc} in
        textual_instr :: List.append ptr_instrs textual_instrs
    | Store {ptr; exp}
      when Textual.Lang.is_swift proc_state.lang && is_store_formal_to_local ~proc_state ptr exp ->
        []
    | Store {ptr; exp; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state exp in
        let exp2_deref_instrs, exp2 = add_deref ~proc_state exp2 loc in
        let exp1, typ_exp1, exp1_instrs = to_textual_exp loc ~proc_state ptr in
        let textual_instr_opt =
          if remove_store_zero_in_class typ_exp1 exp2 then None
          else Some (Textual.Instr.Store {exp1; typ= None; exp2; loc})
        in
        (Option.to_list textual_instr_opt @ exp2_deref_instrs @ exp2_instrs)
        @ exp1_instrs @ textual_instrs
    | Alloc {reg} ->
        let reg_var_name = Var.reg_to_var_name reg in
        let ptr_typ = Type.to_annotated_textual_typ proc_state.lang ~struct_map (Reg.typ reg) in
        ProcState.update_locals ~proc_state reg_var_name ptr_typ ;
        textual_instrs
    | Free _ when Textual.Lang.is_swift proc_state.lang ->
        (* ignore [free] in Swift for now until we know if/where it's needed *)
        []
    | Free {ptr; loc} ->
        let proc = Textual.ProcDecl.free_name in
        let loc = to_textual_loc ~proc_state loc in
        let id, call_exp, call_textual_instrs =
          to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc None [ptr] loc
        in
        let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
        List.append (let_instr :: call_textual_instrs) textual_instrs
    | Nondet {reg; loc} ->
        (* llvm_init_tuple is also a nondet builtin but we return the type for tuples in the Textual to Sil translation *)
        let builtin_name_opt =
          match reg with
          | Some reg when Llair.Typ.is_tuple (Reg.typ reg) ->
              Some "llvm_init_tuple"
          | Some _ ->
              Some "llvm_nondet"
          | None ->
              None
        in
        let loc = to_textual_loc ~proc_state loc in
        let call_textual_instrs =
          match builtin_name_opt with
          | Some builtin_name ->
              to_textual_builtin ~proc_state reg builtin_name [] loc
          | None ->
              []
        in
        List.append call_textual_instrs textual_instrs
    | Builtin {reg; name; args; loc} when Llair.Builtin.equal name `malloc -> (
        let proc = Textual.ProcDecl.malloc_name in
        let loc = to_textual_loc ~proc_state loc in
        match StdUtils.iarray_to_list args with
        | [((Llair.Exp.Integer _ | Llair.Exp.Float _) as exp)] ->
            let id, call_exp, args_instrs =
              to_textual_call_aux ~proc_state ~generate_typ_exp:(Some true)
                ~kind:Textual.Exp.NonVirtual proc reg [exp] loc
            in
            let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
            List.append (List.append [let_instr] args_instrs) textual_instrs
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc}
      when Textual.Lang.is_swift proc_state.ProcState.lang && Llair.Builtin.equal name `memset -> (
        let args = StdUtils.iarray_to_list args in
        let loc = to_textual_loc ~proc_state loc in
        match args with
        | [_; arg2; _; _] when Textual.Exp.is_zero_exp (fst3 (to_textual_exp loc ~proc_state arg2))
          ->
            textual_instrs
        | _ ->
            let name = Llair.Builtin.to_name name in
            let call_textual_instrs = to_textual_builtin ~proc_state reg name args loc in
            List.append call_textual_instrs textual_instrs )
    | Builtin {reg; name; args; loc= loc_} when Llair.Builtin.equal name `expect -> (
        let args = StdUtils.iarray_to_list args in
        let loc = to_textual_loc ~proc_state loc_ in
        match args with
        | [arg1; _] ->
            let exp, _, exp_instrs = to_textual_exp loc ~proc_state arg1 in
            let id =
              match reg with
              | Some reg ->
                  Some (Var.reg_to_id ~proc_state reg |> fst)
              | None ->
                  Some (Var.add_fresh_id ~proc_state ())
            in
            Textual.Instr.Let {id; exp; loc} :: exp_instrs
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc} ->
        let loc = to_textual_loc ~proc_state loc in
        let name = Llair.Builtin.to_name name in
        let args = StdUtils.iarray_to_list args in
        let call_textual_instrs = to_textual_builtin ~proc_state reg name args loc in
        List.append call_textual_instrs textual_instrs
    | Move {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        translate_move ~move_phi:false ~proc_state loc textual_instrs reg_exps
    | MovePhi {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        translate_move ~move_phi:true ~proc_state loc textual_instrs reg_exps
    | AtomicRMW {reg; ptr; exp; loc} ->
        let loc = to_textual_loc ~proc_state loc in
        let call_textual_instrs =
          to_textual_builtin ~proc_state (Some reg) "llvm_atomicRMW" [ptr; exp] loc
        in
        List.append call_textual_instrs textual_instrs
    | AtomicCmpXchg {reg; ptr; cmp; exp; loc} ->
        let loc = to_textual_loc ~proc_state loc in
        let call_textual_instrs =
          to_textual_builtin ~proc_state (Some reg) "llvm_atomicCmpXchg" [ptr; cmp; exp] loc
        in
        List.append call_textual_instrs textual_instrs
  in
  let rev_instrs = List.fold ~init:[] ~f:to_instr (StdUtils.iarray_to_list block.cmnd) in
  let call_instrs =
    match block.term with Call call -> to_textual_call ~proc_state call | _ -> []
  in
  let rev_instrs = List.append call_instrs rev_instrs in
  let instrs = List.rev rev_instrs in
  let first_loc, last_loc =
    match (instrs, rev_instrs) with
    | first :: _, last :: _ ->
        (Some (Textual.Instr.loc first), Some (Textual.Instr.loc last))
    | _ ->
        (None, None)
  in
  (instrs, first_loc, last_loc)


let rec to_textual_jump_and_succs ~proc_state ~seen_nodes jump =
  let block = jump.dst in
  let node_label = block_to_node_name block in
  let node_label, typ_opt, succs =
    (* If we've seen this node, stop the recursion *)
    if Textual.NodeName.HashSet.mem seen_nodes node_label then
      (node_label, None, Textual.Node.Set.empty)
    else
      let (node : Textual.Node.t), typ_opt, nodes =
        block_to_node_and_succs ~proc_state ~seen_nodes block
      in
      (node.label, typ_opt, nodes)
  in
  let node_call = Textual.Terminator.{label= node_label; ssa_args= []} in
  (Textual.Terminator.Jump [node_call], typ_opt, succs)


and to_terminator_and_succs ~proc_state ~seen_nodes term =
  let no_succs = Textual.Node.Set.empty in
  match term with
  | Call {return; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      (to_textual_jump_and_succs ~proc_state ~seen_nodes return, Some loc, [])
  | Return {exp= Some exp; loc= loc_} ->
      let loc = to_textual_loc_instr ~proc_state loc_ in
      let textual_exp, textual_typ_opt, instrs = to_textual_exp loc ~proc_state exp in
      let exp_deref_instrs, textual_exp = add_deref ~proc_state textual_exp loc in
      let instrs = List.append instrs exp_deref_instrs in
      ((Textual.Terminator.Ret textual_exp, textual_typ_opt, no_succs), Some loc, instrs)
  | Return {exp= None; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ((Textual.Terminator.Ret (Textual.Exp.Const Null), None, no_succs), Some loc, [])
  | Throw {exc; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ( (Textual.Terminator.Throw (to_textual_exp loc ~proc_state exc |> fst3), None, no_succs)
      , Some loc
      , [] )
  | Switch {key; tbl; els; loc} -> (
      let loc = to_textual_loc_instr ~proc_state loc in
      match StdUtils.iarray_to_list tbl with
      | [(exp, zero_jump)] when Exp.equal exp Exp.false_ ->
          (* if then else *)
          let bexp, _, instrs = to_textual_bool_exp loc ~proc_state key in
          let else_, else_typ, zero_nodes =
            to_textual_jump_and_succs ~proc_state ~seen_nodes zero_jump
          in
          let then_, if_typ, els_nodes = to_textual_jump_and_succs ~proc_state ~seen_nodes els in
          let term = Textual.Terminator.If {bexp; then_; else_} in
          let nodes = Textual.Node.Set.union zero_nodes els_nodes in
          let typ_opt = Type.join_typ if_typ else_typ in
          ((term, typ_opt, nodes), Some loc, instrs)
      | [] when Exp.equal key Exp.false_ ->
          (* goto *)
          (to_textual_jump_and_succs ~proc_state ~seen_nodes els, Some loc, [])
      | _ ->
          ((Textual.Terminator.Unreachable, None, no_succs), None, [] (* TODO translate Switch *)) )
  | Iswitch {loc} | Abort {loc} | Unreachable {loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ((Textual.Terminator.Unreachable, None, no_succs), Some loc, [])


(* TODO still various parts of the node left to be translated *)
and block_to_node_and_succs ~proc_state ~seen_nodes (block : Llair.block) =
  let node_name = block_to_node_name block in
  let instrs, first_loc, last_loc = cmnd_to_instrs ~proc_state block in
  Textual.NodeName.HashSet.add node_name seen_nodes ;
  let (terminator, typ_opt, succs), term_loc_opt, term_instrs =
    to_terminator_and_succs ~proc_state ~seen_nodes block.term
  in
  let instrs = List.append instrs term_instrs in
  let last_loc =
    match term_loc_opt with
    | Some loc ->
        loc
    | None ->
        Option.value last_loc ~default:Textual.Location.Unknown
  in
  let first_loc =
    match first_loc with
    | Some loc ->
        loc
    | None when List.is_empty instrs ->
        last_loc
    | _ ->
        Textual.Location.Unknown
  in
  let node =
    Textual.Node.
      { label= node_name
      ; ssa_parameters= []
      ; exn_succs= []
      ; last= terminator
      ; instrs
      ; last_loc
      ; label_loc= first_loc }
  in
  (* We add the nodes here to make sure they always get added even in the case of recursive jumps *)
  (node, typ_opt, Textual.Node.Set.add node succs)


let func_to_nodes ~proc_state func =
  let seen_nodes = Textual.NodeName.HashSet.create 16 in
  let _, typ_opt, nodes = block_to_node_and_succs ~proc_state ~seen_nodes func.Llair.entry in
  (typ_opt, Textual.Node.Set.to_list nodes)


let is_undefined func =
  let entry = func.Llair.entry in
  match entry.term with
  | Unreachable _ ->
      String.equal entry.lbl "undefined" && List.is_empty (StdUtils.iarray_to_list entry.cmnd)
  | _ ->
      false


type textual_proc = ProcDecl of Textual.ProcDecl.t | ProcDesc of Textual.ProcDesc.t

let should_translate plain_name lang source_file (loc : Llair.Loc.t) =
  let file =
    if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
    else loc.Llair.Loc.file
  in
  let source_file_loc = SourceFile.create file in
  SourceFile.equal source_file source_file_loc
  || Option.exists plain_name ~f:(fun plain_name ->
         (* the loc in these methods is empty but these are getters and setters and we need to translate them *)
         String.is_substring ~substring:".get" plain_name
         || String.is_substring ~substring:".set" plain_name )


let add_method_to_class_method_index class_method_index class_name proc_name index =
  let methods =
    Textual.TypeName.Hashtbl.find_opt class_method_index class_name |> Option.value ~default:[]
  in
  Textual.TypeName.Hashtbl.replace class_method_index class_name ((proc_name, index) :: methods)


let class_from_global lang struct_map global_name =
  let name = String.substr_replace_first global_name ~pattern:"$s" ~with_:"T" in
  let name = String.chop_suffix_exn name ~suffix:"Mf" in
  Type.struct_name_of_mangled_name lang struct_map name


let process_globals lang class_method_index ~struct_map globals_map =
  let process_exp global (last_offset, carry) exp typ =
    match typ with
    | Llair.Typ.Integer {bits} when Int.equal bits 64 ->
        (last_offset + 1, 0)
    | Llair.Typ.Integer {bits} when Int.equal bits 32 ->
        let carry = carry + 2 in
        if Int.equal carry 4 then (last_offset + 1, 0) else (last_offset, carry)
    | Llair.Typ.Integer {bits} when Int.equal bits 16 ->
        let carry = carry + 1 in
        if Int.equal carry 4 then (last_offset + 1, 0) else (last_offset, carry)
    | Llair.Typ.Pointer _ ->
        let offset = last_offset + 1 in
        ( match exp with
        | Llair.Exp.FuncName {name} ->
            let class_name = class_from_global lang struct_map global in
            let proc_name = Textual.ProcName.of_string name in
            let proc =
              Textual.QualifiedProcName.{enclosing_class= Enclosing class_name; name= proc_name}
            in
            add_method_to_class_method_index class_method_index class_name proc (offset - 3) ;
            Textual.ProcName.Hashtbl.replace ProcState.method_class_index proc_name class_name
        | _ ->
            () ) ;
        (offset, 0)
    | _ ->
        (last_offset, carry)
  in
  let collect_class_method_indices global_name exp =
    match exp with
    | Llair.Exp.ApN (Record, Llair.Typ.Tuple {elts}, elements) ->
        let elements = StdUtils.iarray_to_list elements in
        let _, types = StdUtils.iarray_to_list elts |> List.unzip in
        ignore (List.fold2_exn ~f:(process_exp global_name) ~init:(-1, 0) elements types)
    | _ ->
        ()
  in
  let process_global _var global =
    match global with
    | GlobalDefn.{name; init= Some exp} ->
        let global_name = Global.name name in
        if String.is_suffix global_name ~suffix:"CMf" then
          collect_class_method_indices global_name exp
    | _ ->
        ()
  in
  Textual.VarName.Map.iter process_global globals_map


let to_textual_global lang ~struct_map sourcefile global =
  let global_ = global.GlobalDefn.name in
  let global_name = Global.name global_ in
  let name = Textual.VarName.of_string global_name in
  let typ = Type.to_textual_typ lang ~struct_map (Global.typ global_) in
  let loc = to_textual_loc global.GlobalDefn.loc in
  let global_proc_state = ProcState.global_proc_state lang sourcefile loc global_name in
  let proc_desc_opt =
    match global.GlobalDefn.init with
    | Some exp when Config.llvm_translate_global_init ->
        let init_exp, _, instrs = to_textual_exp ~proc_state:global_proc_state loc exp in
        let procdecl =
          Textual.ProcDecl.
            { qualified_name= global_proc_state.qualified_name
            ; formals_types= Some []
            ; result_type= Textual.Typ.mk_without_attributes Textual.Typ.Void
            ; attributes= [] }
        in
        let start_node =
          Textual.Node.
            { label= Textual.NodeName.of_string "start"
            ; ssa_parameters= []
            ; exn_succs= []
            ; last= Textual.Terminator.Ret init_exp
            ; instrs
            ; last_loc= Textual.Location.Unknown
            ; label_loc= Textual.Location.Unknown }
        in
        let proc_desc =
          Textual.ProcDesc.
            { procdecl
            ; nodes= [start_node]
            ; fresh_ident= None
            ; start= start_node.label
            ; params= []
            ; locals= []
            ; exit_loc= Textual.Location.Unknown }
        in
        Some proc_desc
    | _ ->
        None
  in
  (* The init_exp would need to be a call to the initialiser, to add later. *)
  let global = Textual.Global.{name; typ; attributes= []; init_exp= None} in
  (global, proc_desc_opt)


type attr_map = Textual.Attr.t Textual.QualifiedProcName.Map.t

let pp_attr_map fmt attr_map =
  let pp qualified_name attr =
    Format.fprintf fmt "%a: %a@." Textual.QualifiedProcName.pp qualified_name Textual.Attr.pp attr
  in
  Textual.QualifiedProcName.Map.iter pp attr_map
[@@warning "-unused-value-declaration"]


let create_offset_attributes class_method_index : attr_map =
  let process_class _ method_offsets attr_map =
    let process_method attr_map (method_name, index) =
      let offset_attr = Textual.Attr.mk_method_offset index in
      Textual.QualifiedProcName.Map.add method_name offset_attr attr_map
    in
    List.fold ~init:attr_map ~f:process_method method_offsets
  in
  Textual.TypeName.Hashtbl.fold process_class class_method_index Textual.QualifiedProcName.Map.empty


let function_to_proc_decl lang ~struct_map (func_name, func) =
  let formal_types = to_formal_types lang ~struct_map func in
  let loc = to_textual_loc func.Llair.loc in
  let qualified_name = to_qualified_proc_name ~loc (FuncName.name func_name) in
  let plain_name = match lang with Textual.Lang.Swift -> to_name_attr func_name | _ -> None in
  let fun_result_typ =
    Textual.Typ.mk_without_attributes
      (Type.to_textual_typ lang ~struct_map (FuncName.typ func_name))
  in
  let result_type =
    match func.Llair.freturn_type with
    | Some typ ->
        let typ =
          Option.map ~f:Textual.Typ.mk_without_attributes
            (Type.signature_type_to_textual_typ lang typ)
        in
        Option.value typ ~default:fun_result_typ
    | None ->
        fun_result_typ
  in
  let procdecl =
    Textual.ProcDecl.
      { qualified_name
      ; result_type
      ; attributes= Option.to_list plain_name
      ; formals_types= Some formal_types }
  in
  procdecl


let update_formals_list formals_list formals_map =
  let update_formal varname =
    match Textual.VarName.Map.find_opt varname formals_map with
    | Some (typ, Some local) ->
        (Some typ, local)
    | Some (_, None) | None ->
        (None, varname)
  in
  List.map ~f:update_formal formals_list


let translate_code proc_map lang source_file struct_map globals class_name_offset_map proc_descs
    (procdecl : Textual.ProcDecl.t) (func_name, func) =
  let should_translate =
    should_translate (FuncName.unmangled_name func_name) lang source_file func.Llair.loc
  in
  let formals_list = List.map ~f:Var.reg_to_var_name (StdUtils.iarray_to_list func.Llair.formals) in
  let formals_map =
    match procdecl.Textual.ProcDecl.formals_types with
    | None ->
        Textual.VarName.Map.empty
    | Some formals_types ->
        List.fold2_exn
          ~f:(fun formals varname typ -> Textual.VarName.Map.add varname (typ, None) formals)
          formals_list formals_types ~init:Textual.VarName.Map.empty
  in
  let loc = procdecl.qualified_name.Textual.QualifiedProcName.name.Textual.ProcName.loc in
  let proc_state =
    ProcState.init_state ~qualified_name:procdecl.qualified_name ~sourcefile:source_file ~loc
      ~formals:formals_map ~struct_map ~globals ~lang ~proc_map ~class_name_offset_map
  in
  let ret_typ, nodes = if should_translate then func_to_nodes ~proc_state func else (None, []) in
  let result_type =
    match ret_typ with
    | Some typ ->
        Textual.Typ.mk_without_attributes typ
    | None ->
        procdecl.result_type
  in
  let params_types = update_formals_list formals_list proc_state.formals in
  let formals_types =
    let update_formals_types params formal_types =
      List.map2_exn params formal_types ~f:(fun (typ_opt, _) signature_type ->
          match typ_opt with Some formal_typ -> formal_typ | None -> signature_type )
    in
    Option.map procdecl.formals_types ~f:(fun formals_types ->
        update_formals_types params_types formals_types )
  in
  let procdecl = {procdecl with result_type; formals_types} in
  let is_deinit () = Option.exists (FuncName.unmangled_name func_name) ~f:(String.equal "deinit") in
  if is_undefined func || (not should_translate) || (Textual.Lang.is_swift lang && is_deinit ())
  then ProcDecl procdecl :: proc_descs
  else
    let locals = ProcState.compute_locals ~proc_state in
    ProcDesc
      Textual.ProcDesc.
        { params= List.map ~f:(fun (_, param) -> param) params_types
        ; locals
        ; procdecl
        ; start= block_to_node_name func.Llair.entry
        ; nodes
        ; fresh_ident= None
        ; exit_loc= Unknown (* TODO: get this location *) }
    :: proc_descs


let translate_llair_function_signatures lang struct_map functions =
  let proc_decls = List.map functions ~f:(function_to_proc_decl lang ~struct_map) in
  let struct_map = Type.update_struct_map struct_map in
  (proc_decls, struct_map)


let update_function_signatures lang class_method_index ~struct_map functions =
  let update_proc_decl offset_attributes (procdecl : Textual.ProcDecl.t) =
    let procname = procdecl.qualified_name.Textual.QualifiedProcName.name in
    let loc = procname.Textual.ProcName.loc in
    let qualified_name = to_qualified_proc_name ~loc (Textual.ProcName.to_string procname) in
    let offset_attribute =
      Textual.QualifiedProcName.Map.find_opt qualified_name offset_attributes
    in
    let formals_types, result_type =
      if Textual.Lang.is_swift lang then
        update_signature_types lang ~struct_map procdecl.formals_types procdecl.result_type
      else (procdecl.formals_types, procdecl.result_type)
    in
    let procdecl =
      Textual.ProcDecl.
        { qualified_name
        ; formals_types
        ; result_type
        ; attributes= procdecl.attributes @ Option.to_list offset_attribute }
    in
    procdecl
  in
  let offset_attributes = create_offset_attributes class_method_index in
  List.map functions ~f:(update_proc_decl offset_attributes)


let translate_llair_functions source_file lang struct_map globals proc_decls class_name_offset_map
    values =
  let proc_map =
    List.fold
      ~f:(fun proc_map proc_decl ->
        Textual.QualifiedProcName.Map.add proc_decl.Textual.ProcDecl.qualified_name proc_decl
          proc_map )
      ~init:Textual.QualifiedProcName.Map.empty proc_decls
  in
  List.fold2_exn proc_decls values
    ~f:(translate_code proc_map lang source_file struct_map globals class_name_offset_map)
    ~init:[]


let reset_global_state () =
  Hash_set.clear Type.signature_structs ;
  Textual.ProcName.Hashtbl.clear ProcState.method_class_index


module ClassMethodIndex = struct
  type t = (Textual.QualifiedProcName.t * int) list Textual.TypeName.Hashtbl.t

  let pp_ fmt class_method_index =
    let pp (class_name, index) =
      Format.fprintf fmt "%a: %a@." Textual.TypeName.pp class_name
        (Pp.comma_seq (Pp.pair ~fst:Textual.QualifiedProcName.pp ~snd:Int.pp))
        index
    in
    Textual.TypeName.Hashtbl.to_seq class_method_index
    |> Stdlib.List.of_seq
    |> List.sort ~compare:[%compare: Textual.TypeName.t * _]
    |> List.iter ~f:pp
  [@@warning "-unused-value-declaration"]


  let fill_class_name_offset_map class_method_index =
    let class_name_offset_map = ProcState.ClassNameOffsetMap.create 16 in
    let process_map class_name (proc, offset) =
      let key = ProcState.ClassNameOffset.{class_name; offset} in
      ProcState.ClassNameOffsetMap.replace class_name_offset_map key proc
    in
    let process_class class_name procs = List.iter procs ~f:(process_map class_name) in
    Textual.TypeName.Hashtbl.iter process_class class_method_index ;
    class_name_offset_map
end

type module_state =
  { functions: (FuncName.t * func) list
  ; struct_map: Textual.Struct.t Textual.TypeName.Map.t
  ; proc_decls: Textual.ProcDecl.t list
  ; globals_map: GlobalDefn.t VarMap.t
  ; lang: Textual.Lang.t
  ; class_method_index: ClassMethodIndex.t }

let init_module_state llair_program lang =
  reset_global_state () ;
  let functions = FuncName.Map.to_list llair_program.Llair.functions in
  let globals_map = build_globals_map llair_program.Llair.globals in
  let struct_map = Llair2TextualType.translate_types_env lang llair_program.typ_defns in
  let proc_decls, struct_map = translate_llair_function_signatures lang struct_map functions in
  let class_method_index = Textual.TypeName.Hashtbl.create 16 in
  process_globals lang class_method_index ~struct_map globals_map ;
  {functions; struct_map; proc_decls; globals_map; lang; class_method_index}


let translate ~source_file (module_state : module_state) : Textual.Module.t =
  let {functions; struct_map; proc_decls; globals_map; lang; class_method_index} = module_state in
  let source_file_ = SourceFile.create source_file in
  let globals, proc_descs =
    Textual.VarName.Map.fold
      (fun _ global (globals, proc_descs) ->
        let global, proc_desc_opt = to_textual_global lang ~struct_map source_file_ global in
        let proc_desc_opt =
          Option.map ~f:(fun proc_desc -> Textual.Module.Proc proc_desc) proc_desc_opt
        in
        (Textual.Module.Global global :: globals, Option.to_list proc_desc_opt @ proc_descs) )
      globals_map ([], [])
  in
  let proc_decls = update_function_signatures lang class_method_index ~struct_map proc_decls in
  let class_name_offset_map = ClassMethodIndex.fill_class_name_offset_map class_method_index in
  let procs =
    translate_llair_functions source_file_ lang struct_map globals_map proc_decls
      class_name_offset_map functions
  in
  let procs =
    List.fold
      ~f:(fun procs proc ->
        match proc with
        | ProcDecl proc_decl ->
            Textual.Module.Procdecl proc_decl :: procs
        | ProcDesc proc_desc ->
            Textual.Module.Proc proc_desc :: procs )
      procs ~init:[]
  in
  let structs =
    List.map
      ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
      (Textual.TypeName.Map.bindings struct_map)
  in
  let decls = procs @ proc_descs @ globals @ structs in
  let attrs = [Textual.Attr.mk_source_language lang] in
  let sourcefile = Textual.SourceFile.create source_file in
  Textual.Module.{attrs; decls; sourcefile}
