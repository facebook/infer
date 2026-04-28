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
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module L = Logging
module State = Llair2TextualState
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState
module Globals = Llair2TextualGlobals
module Var = Llair2TextualVar
module Proc = Llair2TextualProc
module Models = Llair2TextualModels
module IdentMap = Textual.Ident.Map
module Utils = Llair2TextualUtils

type module_state = ModuleState.t

let undef_proc_name = Proc.builtin_qual_proc_name "llvm_nondet"

let is_closure lang s = Textual.Lang.is_swift lang && String.is_substring ~substring:"fU" s

let is_init_in_swift_overlay mangled_name =
  String.is_prefix mangled_name ~prefix:"$sSo"
  && (String.is_suffix mangled_name ~suffix:"fC" || String.is_suffix mangled_name ~suffix:"TO")


let is_objc_thunk_not_init mangled_name =
  let is_initializer mangled =
    String.is_suffix mangled ~suffix:"fCTo" || String.is_suffix mangled ~suffix:"fcTo"
  in
  String.is_suffix mangled_name ~suffix:"To" && not (is_initializer mangled_name)


let to_proc_name_closure_name s =
  Textual.QualifiedProcName.
    {enclosing_class= TopLevel; name= Textual.ProcName.of_string s; metadata= None}


let to_textual_loc ?proc_state {Loc.line; col} =
  if Int.equal line 0 && Int.equal col 0 then
    let line =
      if Config.llvm_debug_loc then State.get_fresh_fake_line ()
      else
        match proc_state with
        | Some ProcState.{loc= Known {line= proc_line; _}} ->
            proc_line
        | _ ->
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


let to_name_attr func_name =
  Option.map ~f:Textual.Attr.mk_plain_name (FuncName.unmangled_name func_name)


let to_formal_types lang signature_structs ~struct_map func =
  let to_textual_formal_type formal_type = Var.reg_to_annot_typ lang ~struct_map formal_type in
  let to_textual_formal_signature_type formal formal_type =
    let signature_type = Type.signature_type_to_textual_typ signature_structs lang formal_type in
    let signature_type = Option.map ~f:Textual.Typ.mk_without_attributes signature_type in
    let internal_typ = to_textual_formal_type formal in
    (* we need this check because sometimes llvm changes the types of the parameters to be primitive
    types instead of objects *)
    match signature_type with
    | Some signature_type when Type.is_compatible internal_typ.typ signature_type.typ ->
        signature_type
    | _ ->
        internal_typ
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


let block_to_node_name block =
  let name = block.Llair.lbl in
  Textual.NodeName.of_string name


let undef_exp ?(reason = "unsupported exp") ~sourcefile ~loc ?typ ~proc exp =
  let pp_typ fmt typ = Option.iter typ ~f:(fun typ -> F.fprintf fmt ":%a" Textual.Typ.pp typ) in
  L.internal_error "Llair2Textual: %s: %a%a in proc %a in %a at %a@\n" reason Llair.Exp.pp exp
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
    | Lt | Ult ->
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
    (* Check for Metadata Taint (handles both Field access and Pointer deref) *)
    Models.Metadata.propagate ~proc_state id exp ~is_load:true ;
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
      ProcState.update_ids_move ~proc_state id None ~loaded_var:true ~deref_needed:false ;
      add_load_instr
  | Textual.Exp.Field _ ->
      add_load_instr
  | Textual.Exp.Var id -> (
      let id_data = IdentMap.find_opt id proc_state.ProcState.ids_move in
      match id_data with
      | Some {loaded_var= true} | Some {deref_needed= false} ->
          ([], exp)
      | Some {deref_needed= true} ->
          add_load_instr
      | _ ->
          ([], exp) )
  | _ ->
      ([], exp)


let should_add_deref ~proc_state exp =
  match exp with
  | Textual.Exp.Lvar _ ->
      true
  | Textual.Exp.Field _ ->
      true
  | Textual.Exp.Var id -> (
      let id_data = IdentMap.find_opt id proc_state.ProcState.ids_move in
      match id_data with
      | Some {loaded_var= true} ->
          false
      | Some {deref_needed= true} ->
          true
      | _ ->
          false )
  | _ ->
      false


let update_id_return_type ~(proc_state : ProcState.t) (proc : Textual.QualifiedProcName.t) id =
  let module_state = proc_state.module_state in
  let proc_decl_opt = Textual.QualifiedProcName.Map.find_opt proc module_state.proc_map in
  match (id, proc_decl_opt) with
  | Some id, Some proc_decl ->
      let return_typ = proc_decl.Textual.ProcDecl.result_type in
      let refined_typ =
        (* If we have a generic Swift type, try to "look inside" the mangled name *)
        if Textual.Typ.equal return_typ.Textual.Typ.typ Textual.Typ.any_type_swift then
          let proc_name_str = Textual.ProcName.to_string proc.name in
          match Utils.extract_type_from_mangled_proc proc_name_str with
          | Some class_name ->
              (* Use the central struct_name_of_mangled_name to reconcile with the module *)
              let type_name =
                TypeName.struct_name_of_mangled_name module_state.lang
                  ~mangled_map:(Some module_state.mangled_map) module_state.struct_map class_name
              in
              let struct_typ = Textual.Typ.Struct type_name in
              Textual.Typ.mk_without_attributes (Textual.Typ.mk_ptr struct_typ)
          | None ->
              return_typ
        else return_typ
      in
      ProcState.update_ids_types ~proc_state id refined_typ
  | _ ->
      ()


let to_textual_not_exp op exp1 exp2 =
  match op with
  | Llair.Exp.Xor when Textual.Exp.is_one_exp exp1 ->
      let proc = Textual.ProcDecl.of_unop Unop.LNot in
      Some Textual.Exp.(Call {proc; args= [exp2]; kind= Textual.Exp.NonVirtual})
  | Llair.Exp.Xor when Textual.Exp.is_one_exp exp2 ->
      let proc = Textual.ProcDecl.of_unop Unop.LNot in
      Some Textual.Exp.(Call {proc; args= [exp1]; kind= Textual.Exp.NonVirtual})
  | _ ->
      None


let extract_struct_name_opt module_state llair_typ =
  let ModuleState.{lang; mangled_map; struct_map; _} = module_state in
  match llair_typ with
  | Llair.Typ.Struct {name} ->
      Some
        (TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map name)
  | Llair.Typ.Tuple _ -> (
    match Type.to_textual_typ lang ~mangled_map ~struct_map llair_typ with
    | Textual.Typ.(Ptr (Struct name, _)) ->
        Some name
    | _ ->
        None )
  | _ ->
      None


let resolve_real_type proc_state llair_exp llair_typ original_typ_name =
  let inferred_llair_typ =
    match llair_exp with
    | Llair.Exp.Reg {id; _} ->
        Hashtbl.find proc_state.ProcState.inferred_types id
    | _ ->
        None
  in
  match inferred_llair_typ with
  | Some (Textual.Typ.Ptr (Struct name, _)) when Llair.Typ.is_class_bound_tuple llair_typ ->
      (name, true)
  | _ ->
      (original_typ_name, false)


let inject_load_type field_instrs typ_name =
  let expected_typ = Textual.Typ.mk_ptr (Struct typ_name) in
  List.map field_instrs ~f:(fun instr ->
      match instr with
      | Textual.Instr.Load {id; exp= load_exp; typ= _; loc= load_loc} ->
          Textual.Instr.Load {id; exp= load_exp; typ= Some expected_typ; loc= load_loc}
      | _ ->
          instr )


let rec to_textual_exp ~(proc_state : ProcState.t) loc ?generate_typ_exp (exp : Llair.Exp.t) :
    Textual.Exp.t * Textual.Typ.t option * Textual.Instr.t list =
  let ModuleState.{struct_map; mangled_map; lang; _} = proc_state.module_state in
  match exp with
  | Integer {data; typ} ->
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then Textual.Exp.Typ textual_typ
        else if NS.Z.is_false data && not (Llair.Typ.is_int typ) then Textual.Exp.Const Null
        else Textual.Exp.Const (Int data)
      in
      (textual_exp, Some textual_typ, [])
  | Float {data; typ} ->
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      let textual_exp =
        if Option.is_some generate_typ_exp then
          Textual.Exp.Typ (Type.to_textual_typ lang ~mangled_map ~struct_map typ)
        else Textual.Exp.Const (Float (Float.of_string data))
      in
      (textual_exp, Some textual_typ, [])
  | Nondet {typ} ->
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name
        ~typ:textual_typ exp
  | FuncName {name} ->
      let s_exp = Textual.Exp.Const (Str name) in
      let exp =
        if is_closure lang name then
          let proc =
            Textual.QualifiedProcName.Map.find_opt (to_proc_name_closure_name name)
              proc_state.module_state.ModuleState.proc_map
          in
          match proc with
          | Some proc -> (
            match proc.formals_types with
            | Some formals ->
                let params =
                  List.mapi
                    ~f:(fun i _ -> Textual.VarName.of_string (Format.sprintf "var%d" (i + 1)))
                    formals
                in
                Textual.Exp.Closure
                  {proc= to_proc_name_closure_name name; attributes= []; captured= []; params}
            | _ ->
                s_exp )
          | None ->
              s_exp
        else s_exp
      in
      (exp, None, [])
  | Reg {id; name; typ} ->
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
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
  | Global {name; typ} ->
      Globals.translate_global ~proc_state ~lang ~struct_map ~mangled_map ~name ~typ
  | Ap1 (Select offset, llair_typ, llair_exp) -> (
    match extract_struct_name_opt proc_state.module_state llair_typ with
    | None ->
        undef_exp ~sourcefile:proc_state.sourcefile ~loc ~proc:proc_state.qualified_name exp
    | Some original_typ_name ->
        let exp, exp_typ, exp_instrs = to_textual_exp loc ~proc_state llair_exp in
        let typ_name, has_real_type =
          resolve_real_type proc_state llair_exp llair_typ original_typ_name
        in
        let typ_name, n =
          Models.translate_optional_protocol_witness ~proc_state exp exp_typ typ_name offset
        in
        let field =
          if Llair.Typ.is_tuple llair_typ && Int.equal n offset && not has_real_type then
            Field.tuple_field_of_pos typ_name n
          else Field.field_of_pos_with_map proc_state.module_state.field_offset_map typ_name n
        in
        let field_instrs, exp = add_deref ~proc_state exp loc in
        let field_instrs = inject_load_type field_instrs typ_name in
        let instrs = field_instrs @ exp_instrs in
        let exp = Textual.Exp.Field {exp; field} in
        let typ = Type.lookup_field_type ~struct_map typ_name field in
        (exp, typ, instrs) )
  | Ap1 (GetElementPtr (Static n), _typ, _exp) ->
      let n_arg = Llair.Exp.integer (Llair.Typ.integer ~bits:32 ~byts:4) (Z.of_int n) in
      let exp, _, instrs = to_textual_exp loc ~proc_state n_arg in
      let var_name = ProcState.mk_fresh_tmp_var State.get_element_ptr_offset_prefix proc_state in
      let new_var = Textual.Exp.Lvar var_name in
      ProcState.update_locals ~proc_state var_name
        (Textual.Typ.mk_without_attributes (Textual.Typ.mk_ptr Void)) ;
      ProcState.update_var_offset ~proc_state var_name n ;
      let store_instr = Textual.Instr.Store {exp1= new_var; exp2= exp; typ= None; loc} in
      (new_var, None, store_instr :: instrs)
  | Ap1 (GetElementPtr (DynamicWvd wvd_name), typ, base_ptr) as full_exp -> (
      let base_exp, base_typ_opt, base_instrs = to_textual_exp loc ~proc_state base_ptr in
      let base_deref_instrs, base_exp_deref = add_deref ~proc_state base_exp loc in
      let wvd_class_opt, field_name_str = Field.extract_class_and_field_from_wvd wvd_name in
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      let instrs = base_instrs @ base_deref_instrs in
      match (wvd_class_opt, field_name_str) with
      | _, "unknown_field" ->
          (* "unknown_field" is the sentinel emitted by the Swift Wvd
             mangled-name parser when it could not decode the field-name
             part of [wvd_name]. Emitting a [Textual.Exp.Field] with this
             name (regardless of whether the class part parsed) produces a
             malformed module that later trips the SIL Consistency Error
             "field <class>.unknown_field is not declared". Fall back to a
             non-deterministic value so capture can still succeed for the
             rest of the file. *)
          let reason =
            match wvd_class_opt with
            | None ->
                F.asprintf "could not parse Wvd mangled name %s" wvd_name
            | Some _ ->
                F.asprintf "could not parse field name in Wvd mangled name %s" wvd_name
          in
          let undef, _, undef_instrs =
            undef_exp ~reason ~sourcefile:proc_state.sourcefile ~loc ~typ:textual_typ
              ~proc:proc_state.qualified_name full_exp
          in
          (undef, Some textual_typ, instrs @ undef_instrs)
      | _ ->
          let name = Textual.FieldName.of_string field_name_str in
          let enclosing_class =
            match (wvd_class_opt, base_typ_opt) with
            | Some mangled_class, _ ->
                TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
                  mangled_class
            | None, Some (Textual.Typ.Ptr (Struct class_name, _)) ->
                class_name
            | _ ->
                Textual.TypeName.mk_swift_type_name Textual.BaseTypeName.swift_any_type_name.value
          in
          let field = {Textual.enclosing_class; name} in
          let exp = Textual.Exp.Field {exp= base_exp_deref; field} in
          (exp, Some textual_typ, instrs) )
  | Ap1 ((Convert _ | Signed _ | Unsigned _), dst_typ, exp) ->
      (* Signed is the translation of llvm's trunc and SExt and Unsigned is the translation of ZExt, all different types of cast,
       and convert translates other types of cast *)
      let exp, _, instrs = to_textual_exp loc ~proc_state exp in
      let deref_instrs, exp = add_deref ~proc_state exp loc in
      let textual_dst_typ = Type.to_textual_typ lang ~mangled_map ~struct_map dst_typ in
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
  | Ap2 (((Eq | Dq | Gt | Ge | Le | Lt | Ult | And | Or | Xor | Shl | Lshr | Ashr) as op), _, e1, e2)
    ->
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let deref_instrs1, exp1 = add_deref ~proc_state exp1 loc in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      let deref_instrs2, exp2 = add_deref ~proc_state exp2 loc in
      (* Check if either expression is a Var that we know came from getDynamicType *)
      let is_metadata exp =
        match exp with Textual.Exp.Var id -> ProcState.is_metadata_id ~proc_state id | _ -> false
      in
      let exp =
        if Llair.Exp.equal_op2 op Eq && (is_metadata exp1 || is_metadata exp2) then
          let proc = Models.builtin_qual_proc_name Models.swift_metadata_equals in
          Textual.Exp.Call {proc; args= [exp1; exp2]; kind= NonVirtual}
        else
          match to_textual_not_exp op exp1 exp2 with
          | Some exp ->
              exp
          | None ->
              let proc = to_textual_bool_exp_builtin op in
              Textual.Exp.Call {proc; args= [exp1; exp2]; kind= Textual.Exp.NonVirtual}
      in
      (exp, typ1, deref_instrs1 @ exp1_instrs @ deref_instrs2 @ exp2_instrs)
  | Ap2 (Update idx, typ, rcd, elt) ->
      let rcd_exp, _, rcd_instrs = to_textual_exp loc ~proc_state rcd in
      let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state elt in
      let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
      let elt_instrs = List.append elt_instrs elt_deref_instrs in
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      let type_name =
        match textual_typ with
        | Textual.Typ.(Ptr (Struct name, _)) ->
            name
        | _ ->
            L.internal_error "Llair2Textual: unexpected type %a in %a at %a" Textual.Typ.pp
              textual_typ SourceFile.pp proc_state.sourcefile Textual.Location.pp loc ;
            Textual.TypeName.mk_swift_type_name Textual.BaseTypeName.swift_any_type_name.value
      in
      let index_exp =
        Textual.Exp.Field {exp= rcd_exp; field= Field.tuple_field_of_pos type_name idx}
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
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      let type_name_opt =
        match textual_typ with
        | Textual.Typ.(Ptr (Struct name, _)) | Textual.Typ.Struct name ->
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
            let proc = Models.builtin_qual_proc_name Models.llvm_init_tuple in
            Textual.Exp.Call {proc; args= []; kind= NonVirtual}
          in
          let rcd_store_instr = Textual.Instr.Let {id= Some id; exp= undef_exp; loc} in
          (* for each element in the record we set the value to a field of the record variable. *)
          let to_textual_exp_index idx acc_instrs element =
            let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state element in
            let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
            let elt_instrs = List.append elt_instrs elt_deref_instrs in
            let field =
              if Llair.Typ.is_tuple typ then Field.tuple_field_of_pos type_name idx
              else
                Field.field_of_pos_with_map proc_state.module_state.field_offset_map type_name idx
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
    match (proc_state.ProcState.id_offset, List.last args) with
    | Some (proc_state_id_offset, offset), Some (Textual.Exp.Var self_id)
      when Textual.Ident.equal proc_state_id_offset id_offset
           && Textual.ProcName.equal (Textual.QualifiedProcName.name proc) Models.llvm_dynamic_call
      -> (
      match State.IdentMap.find_opt self_id proc_state.ProcState.ids_types with
      | Some {typ= Textual.Typ.Ptr ((Textual.Typ.Struct struct_name as inner_typ), _)}
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


and to_textual_call_aux ~(proc_state : ProcState.t) ~kind (proc : Textual.QualifiedProcName.t)
    return ?generate_typ_exp (llair_args : Llair.Exp.t list) (loc : Textual.Location.t) =
  let args_instrs, args_with_types =
    List.fold_map llair_args ~init:[] ~f:(fun acc_instrs exp ->
        let exp, typ, instrs = to_textual_exp loc ~proc_state ?generate_typ_exp exp in
        let deref_instrs, deref_exp =
          (* So far it looks like for C the load operations are already there when needed. *)
          if
            Textual.Lang.is_swift proc_state.module_state.lang
            && not
                 (Textual.ProcName.equal proc.Textual.QualifiedProcName.name
                    Models.swift_weak_assign )
          then add_deref ~proc_state ~from_call:true exp loc
          else ([], exp)
        in
        (* Store pair (exp, typ) in the list *)
        (deref_instrs @ acc_instrs @ instrs, (deref_exp, typ)) )
  in
  let args, arg_types = List.unzip args_with_types in
  let args, args_instrs =
    if Textual.QualifiedProcName.equal proc Textual.ProcDecl.assert_fail_name then
      ([Textual.Exp.Const Null], [])
    else (args, args_instrs)
  in
  let return_id = Option.map return ~f:(fun reg -> Var.reg_to_id ~proc_state reg |> fst) in
  let resolve_call_translation proc args =
    match resolve_method_call ~proc_state proc args with
    | Some call_instr ->
        (call_instr, [])
    | None ->
        (Textual.Exp.Call {proc; args; kind}, [])
  in
  let call_exp, call_instrs =
    if List.exists Models.functions_to_skip ~f:(Textual.ProcName.equal proc.name) then
      (* skip calls to the elements in functions_to_skip  and returns its first argument instead. *)
      (List.hd_exn args, [])
    else if
      List.exists
        ~f:(fun opaque_existential ->
          Textual.ProcName.equal proc.name (Textual.ProcName.of_string opaque_existential) )
        Models.boxed_opaque_existentials
    then
      match
        Models.translate_boxed_opaque_existential
          ~f_to_textual_exp:(fun ~proc_state loc exp -> to_textual_exp ~proc_state loc exp)
          ~f_add_deref:(fun ~proc_state exp loc -> add_deref ~proc_state exp loc)
          ~proc_state loc llair_args
      with
      | Some (textual_exp, instrs) ->
          (textual_exp, instrs)
      | None ->
          resolve_call_translation proc args
    else
      match
        Models.Metadata.try_translate_swift_metadata_call ~proc_state proc llair_args return_id loc
      with
      | Some exp_instrs ->
          exp_instrs
      | None ->
          resolve_call_translation proc args
  in
  update_id_return_type ~proc_state proc return_id ;
  let instrs = call_instrs @ args_instrs in
  (return_id, call_exp, instrs, arg_types)


and to_textual_call_instrs ~proc_state return proc args loc =
  let id, call_exp, args_instrs, _ =
    to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc return args loc
  in
  let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
  let_instr :: args_instrs


and to_textual_builtin ~proc_state return name args loc =
  let proc = Models.builtin_qual_proc_name name in
  to_textual_call_instrs ~proc_state return proc args loc


and to_textual_call ~(proc_state : ProcState.t) (call : 'a Llair.call) =
  let llair_args = StdUtils.iarray_to_list call.actuals in
  let proc, kind, exp_opt =
    match call.callee with
    | Direct {func} -> (
      match FuncName.unmangled_name func.Llair.name with
      | Some name when String.equal name Models.derived_enum_equals ->
          (Models.builtin_qual_proc_name Models.derived_enum_equals, Textual.Exp.NonVirtual, None)
      | _ ->
          let proc =
            if
              String.equal (FuncName.name func.Llair.name)
                (Procname.get_method BuiltinDecl.__assert_fail)
              || String.is_substring ~substring:"assertionFailure" (FuncName.name func.Llair.name)
            then Textual.ProcDecl.assert_fail_name
            else
              Proc.to_qualified_proc_name proc_state.module_state.method_class_index
                (FuncName.name func.Llair.name)
          in
          (proc, Textual.Exp.NonVirtual, None) )
    | Indirect {ptr} ->
        let proc =
          Models.builtin_qual_proc_name (Textual.ProcName.to_string Models.llvm_dynamic_call)
        in
        (proc, Textual.Exp.NonVirtual, Some ptr)
    | Intrinsic intrinsic ->
        let proc = Models.builtin_qual_proc_name (Llair.Intrinsic.to_name intrinsic) in
        (proc, Textual.Exp.NonVirtual, None)
  in
  let loc = to_textual_loc_instr ~proc_state call.loc in
  let llair_args = Option.to_list exp_opt @ llair_args in
  Models.save_metadata_type ~proc_state call llair_args proc ;
  let id, call_exp, args_instrs, arg_types =
    to_textual_call_aux ~proc_state ~kind proc call.areturn llair_args loc
  in
  let proc_name_str = Textual.ProcName.to_string proc.name in
  Models.try_propagate_objc_class ~proc_state proc_name_str id llair_args ;
  Models.update_selector_metadata ~proc_state id call_exp proc_name_str ;
  let call_exp =
    match call_exp with
    | Textual.Exp.Call {proc; args= textual_args} -> (
      match
        Models.get_alloc_class_name ~proc_state (Textual.QualifiedProcName.name proc) llair_args
      with
      | Some (class_name, builtin_alloc_proc) ->
          let args =
            if Textual.QualifiedProcName.equal builtin_alloc_proc Textual.ProcDecl.objc_alloc_name
            then Textual.Exp.Typ (Textual.Typ.Struct class_name) :: textual_args
            else [Textual.Exp.Typ (Textual.Typ.Struct class_name)]
          in
          Textual.Exp.Call {proc= builtin_alloc_proc; args; kind= Textual.Exp.NonVirtual}
      | None ->
          call_exp )
    | _ ->
        call_exp
  in
  let call_exp = Models.resolve_objc_msgSend ~proc_state call_exp arg_types in
  let instrs =
    match
      Models.try_rewrite_call_to_instrs ~proc_state
        ~f_to_textual_exp:(fun ~proc_state loc exp -> to_textual_exp ~proc_state loc exp)
        ~f_add_deref:(fun ~proc_state exp loc -> add_deref ~proc_state exp loc)
        ~loc ~call_exp ~llair_args ~args_instrs ~id
    with
    | Some model_instrs ->
        model_instrs
    | None ->
        [Textual.Instr.Let {id; exp= call_exp; loc}]
  in
  instrs @ args_instrs


let remove_store_zero_in_class typ_exp1 exp2 =
  let any_type_llvm_name =
    match Textual.Typ.any_type_llvm with Textual.Typ.Struct name -> name | _ -> assert false
  in
  match typ_exp1 with
  | Some (Textual.Typ.Ptr (Struct type_name, _))
    when Textual.Exp.is_zero_exp exp2 && not (Textual.TypeName.equal type_name any_type_llvm_name)
    ->
      true
  | _ ->
      false


let translate_move ~proc_state ~move_phi loc textual_instrs reg_exps =
  let reg_exps = StdUtils.iarray_to_list reg_exps in
  let loc = to_textual_loc_instr ~proc_state loc in
  let instrs =
    List.fold
      ~f:(fun instrs (reg, exp) ->
        let exp, exp_typ, exp_instrs = to_textual_exp loc ~proc_state exp in
        let id = Some (Var.reg_to_id ~proc_state reg |> fst) in
        ( match id with
        | Some lhs_id ->
            Models.Metadata.propagate ~proc_state lhs_id exp ~is_load:false
        | _ ->
            () ) ;
        let loaded_var =
          match exp with
          | Textual.Exp.Var var ->
              Option.value_map
                ~f:(fun id_data -> id_data.ProcState.loaded_var)
                (IdentMap.find_opt var proc_state.ProcState.ids_move)
                ~default:false
          | _ when move_phi ->
              true
          | _ ->
              false
        in
        ( match (id, exp_typ) with
        | Some id, Some exp_typ ->
            ProcState.update_ids_move ~proc_state id
              (Some (Textual.Typ.mk_without_attributes exp_typ))
              ~loaded_var
              ~deref_needed:(should_add_deref ~proc_state exp)
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
          Type.to_annotated_textual_typ proc_state.module_state.lang
            ~mangled_map:proc_state.module_state.mangled_map
            ~struct_map:proc_state.module_state.struct_map typ1
        in
        ProcState.subst_formal_local ~proc_state ~formal:name2 ~local:(name1, typ1) ;
        true )
      else false
  | _ ->
      false


let translate_store_in_field_zero ~(proc_state : ProcState.t) exp1 loc typ_name =
  let deref_instrs, base_exp = add_deref ~proc_state exp1 loc in
  let field_exp =
    Textual.Exp.Field
      { exp= base_exp
      ; field= Field.field_of_pos_with_map proc_state.module_state.field_offset_map typ_name 0 }
  in
  (field_exp, deref_instrs)


let cmnd_to_instrs ~(proc_state : ProcState.t) block =
  let ModuleState.{lang; struct_map; mangled_map} = proc_state.module_state in
  let to_instr textual_instrs inst =
    match inst with
    | Load {reg; ptr; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let id, _ = Var.reg_to_id ~proc_state reg in
        (* Check if we are loading a selector global or ObjC class name *)
        Models.try_capture_objc_metadata ~proc_state id ptr ;
        let exp, _, ptr_instrs = to_textual_exp loc ~proc_state ptr in
        (* Taint propagation (Address -> Value) *)
        Models.Metadata.propagate ~proc_state id exp ~is_load:true ;
        ProcState.update_id_offset ~proc_state id exp ;
        ProcState.update_ids_move ~proc_state id None ~loaded_var:true ~deref_needed:false ;
        let inferred_typ = Hashtbl.find proc_state.inferred_types (Reg.id reg) in
        Option.iter inferred_typ ~f:(fun t ->
            ProcState.update_ids_types ~proc_state id (Textual.Typ.mk_without_attributes t) ) ;
        let textual_instr = Textual.Instr.Load {id; exp; typ= inferred_typ; loc} in
        textual_instr :: List.append ptr_instrs textual_instrs
    | Store {ptr; exp}
      when Textual.Lang.is_swift lang && is_store_formal_to_local ~proc_state ptr exp ->
        textual_instrs
    | Store {ptr; exp; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state exp in
        let exp2_deref_instrs, exp2 = add_deref ~proc_state exp2 loc in
        let exp1, typ_exp1, exp1_instrs = to_textual_exp loc ~proc_state ptr in
        let exp1, exp1_deref_instrs =
          (* In llvm when we store the struct (or an optional int) without accessing any fields,
          it means the first field. The first field of a struct is always at offset 0, so the base
          pointer already points to it. No offset calculation is needed. *)
          match (exp1, typ_exp1) with
          | Textual.Exp.Lvar _, Some (Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) as typ_exp)
            when Type.is_ptr_struct typ_exp ->
              translate_store_in_field_zero ~(proc_state : ProcState.t) exp1 loc typ_name
          | _, Some (Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) as typ_exp)
            when Type.is_int_optional typ_exp ->
              translate_store_in_field_zero ~(proc_state : ProcState.t) exp1 loc typ_name
          | _ ->
              (exp1, [])
        in
        let textual_instr_opt =
          if remove_store_zero_in_class typ_exp1 exp2 then None
          else Some (Textual.Instr.Store {exp1; typ= None; exp2; loc})
        in
        (Option.to_list textual_instr_opt @ exp2_deref_instrs @ exp2_instrs)
        @ exp1_instrs @ exp1_deref_instrs @ textual_instrs
    | Alloc {reg} ->
        let reg_var_name = Var.reg_to_var_name reg in
        let ptr_typ = Type.to_textual_typ lang ~mangled_map ~struct_map (Reg.typ reg) in
        let ptr_typ =
          if not (Textual.Typ.is_pointer ptr_typ) then
            if Textual.Lang.is_swift lang then Textual.Typ.mk_ptr Textual.Typ.any_type_swift
            else Textual.Typ.mk_ptr Textual.Typ.any_type_llvm
          else ptr_typ
        in
        ProcState.update_locals ~proc_state reg_var_name (Textual.Typ.mk_without_attributes ptr_typ) ;
        textual_instrs
    | Free _ when Textual.Lang.is_swift lang ->
        (* ignore [free] in Swift for now until we know if/where it's needed *)
        textual_instrs
    | Free {ptr; loc} ->
        let proc = Textual.ProcDecl.free_name in
        let loc = to_textual_loc ~proc_state loc in
        let id, call_exp, call_textual_instrs, _ =
          to_textual_call_aux ~proc_state ~kind:Textual.Exp.NonVirtual proc None [ptr] loc
        in
        let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
        List.append (let_instr :: call_textual_instrs) textual_instrs
    | Nondet {reg; loc} ->
        (* llvm_init_tuple is also a nondet builtin but we return the type for tuples in the Textual to Sil translation *)
        let builtin_name_opt =
          match reg with
          | Some reg when Llair.Typ.is_tuple (Reg.typ reg) ->
              Some Models.llvm_init_tuple
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
            let id, call_exp, args_instrs, _ =
              to_textual_call_aux ~proc_state ~generate_typ_exp:(Some true)
                ~kind:Textual.Exp.NonVirtual proc reg [exp] loc
            in
            let let_instr = Textual.Instr.Let {id; exp= call_exp; loc} in
            List.append (List.append [let_instr] args_instrs) textual_instrs
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc}
      when Textual.Lang.is_swift lang && Llair.Builtin.equal name `memset -> (
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
            Textual.Instr.Let {id; exp; loc} :: (exp_instrs @ textual_instrs)
        | _ ->
            assert false )
    | Builtin {reg; name; args; loc} ->
        let loc = to_textual_loc ~proc_state loc in
        let name = Llair.Builtin.to_name name in
        let args = StdUtils.iarray_to_list args in
        let call_textual_instrs = to_textual_builtin ~proc_state reg name args loc in
        List.append call_textual_instrs textual_instrs
    | Move {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        translate_move ~proc_state ~move_phi:false loc textual_instrs reg_exps
    | MovePhi {reg_exps: (Reg.t * Exp.t) NS.iarray; loc} ->
        translate_move ~proc_state ~move_phi:true loc textual_instrs reg_exps
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
      ( (Textual.Terminator.Ret (Textual.Exp.Const Null), Some Textual.Typ.Void, no_succs)
      , Some loc
      , [] )
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


let should_translate plain_name mangled_name lang method_class_index source_file (loc : Llair.Loc.t)
    =
  let file =
    if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
    else loc.Llair.Loc.file
  in
  let source_file_loc = SourceFile.create file in
  let proc_name = Textual.ProcName.of_string mangled_name in
  let typ_name = Textual.ProcName.Hashtbl.find_opt method_class_index proc_name in
  let typ_name =
    Option.bind ~f:(fun name -> Textual.TypeName.swift_mangled_name_of_type_name name) typ_name
  in
  SourceFile.equal source_file source_file_loc
  (* the loc in these methods is empty but these are getters
     and setters or closure bodies, etc and we need to translate them *)
  || is_closure lang mangled_name
  || is_init_in_swift_overlay mangled_name
  || is_objc_thunk_not_init mangled_name
  || Option.exists
       ~f:(fun name -> String.is_suffix ~suffix:Globals.witness_protocol_suffix name)
       typ_name
  || Option.exists plain_name ~f:(fun plain_name ->
         String.is_substring ~substring:Field.get_suffix plain_name
         || String.is_substring ~substring:Field.set_suffix plain_name
         || String.is_substring ~substring:Field.modify_suffix plain_name )


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


let function_to_proc_decl lang signature_structs method_class_index ~struct_map (func_name, func) =
  let formal_types = to_formal_types lang signature_structs ~struct_map func in
  let loc = to_textual_loc func.Llair.loc in
  let qualified_name =
    Proc.to_qualified_proc_name method_class_index ~loc (FuncName.name func_name)
  in
  let plain_name = match lang with Textual.Lang.Swift -> to_name_attr func_name | _ -> None in
  let fun_result_typ =
    Textual.Typ.mk_without_attributes
      (Type.to_textual_typ_without_mangled_map lang ~struct_map (FuncName.typ func_name))
  in
  let result_type =
    match func.Llair.freturn_type with
    | Some typ ->
        let typ =
          Option.map ~f:Textual.Typ.mk_without_attributes
            (Type.signature_type_to_textual_typ signature_structs lang typ)
        in
        Option.value typ ~default:fun_result_typ
    | None ->
        fun_result_typ
  in
  Textual.ProcDecl.
    { qualified_name
    ; result_type
    ; attributes= Option.to_list plain_name
    ; formals_types= Some formal_types }


let update_formals_list formals_list formals_map =
  let update_formal varname =
    match Textual.VarName.Map.find_opt varname formals_map with
    | Some ProcState.{typ; assoc_local= Some local} ->
        (Some typ, local)
    | Some ProcState.{assoc_local= None} | None ->
        (None, varname)
  in
  List.map ~f:update_formal formals_list


let translate_code source_file ~module_state proc_descs (procdecl : Textual.ProcDecl.t)
    (func_name, func) =
  let ModuleState.{lang; method_class_index} = module_state in
  let should_translate =
    should_translate
      (FuncName.unmangled_name func_name)
      (FuncName.name func_name) lang method_class_index source_file func.Llair.loc
  in
  let formals_list = List.map ~f:Var.reg_to_var_name (StdUtils.iarray_to_list func.Llair.formals) in
  let formals_map =
    match procdecl.Textual.ProcDecl.formals_types with
    | None ->
        Textual.VarName.Map.empty
    | Some formals_types ->
        List.fold2_exn
          ~f:(fun formals varname typ ->
            Textual.VarName.Map.add varname
              ProcState.{typ; assoc_local= None; read= NotRead}
              formals )
          formals_list formals_types ~init:Textual.VarName.Map.empty
  in
  let loc = procdecl.qualified_name.Textual.QualifiedProcName.name.Textual.ProcName.loc in
  let inferred_types =
    if should_translate then Llair2TextualTypeInference.infer_func module_state func
    else Hashtbl.create (module Int)
  in
  let proc_state =
    ProcState.init ~qualified_name:procdecl.qualified_name ~sourcefile:source_file ~loc
      ~formals:formals_map ~module_state ~inferred_types
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
  then Textual.Module.Procdecl procdecl :: proc_descs
  else
    let locals = ProcState.compute_locals ~proc_state in
    Textual.Module.Proc
      Textual.ProcDesc.
        { params= List.map ~f:(fun (_, param) -> param) params_types
        ; locals
        ; procdecl
        ; start= block_to_node_name func.Llair.entry
        ; nodes
        ; fresh_ident= None
        ; exit_loc= Unknown (* TODO: get this location *) }
    :: proc_descs


let translate_llair_function_signatures lang method_class_index struct_map functions =
  let signature_structs = Hash_set.create (module String) in
  let proc_decls =
    List.map functions
      ~f:(function_to_proc_decl lang signature_structs method_class_index ~struct_map)
  in
  let struct_map = Type.update_struct_map signature_structs struct_map in
  (proc_decls, struct_map)


let update_function_signatures lang class_method_index method_class_index ~mangled_map ~struct_map
    ~plain_map functions =
  let update_proc_decl offset_attributes (procdecl : Textual.ProcDecl.t) =
    let procname = procdecl.qualified_name.Textual.QualifiedProcName.name in
    let loc = procname.Textual.ProcName.loc in
    let qualified_name =
      Proc.to_qualified_proc_name ~loc method_class_index (Textual.ProcName.to_string procname)
    in
    let offset_attribute =
      Textual.QualifiedProcName.Map.find_opt qualified_name offset_attributes
    in
    let formals_types, result_type =
      if Textual.Lang.is_swift lang then
        Type.update_signature_types lang ~mangled_map ~struct_map ~plain_map procdecl.formals_types
          procdecl.result_type
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


let translate_llair_functions source_file ~(module_state : ModuleState.t) =
  let ModuleState.{proc_decls; functions; _} = module_state in
  List.fold2_exn proc_decls functions ~init:[] ~f:(translate_code source_file ~module_state)


let populate_objc_method_index proc_decls =
  let index = Hashtbl.create (module String) in
  List.iter proc_decls ~f:(fun (decl : Textual.ProcDecl.t) ->
      List.find_map decl.attributes ~f:Textual.Attr.get_plain_name
      |> Option.iter ~f:(fun plain ->
             let qname = decl.qualified_name in
             let mangled = Textual.ProcName.to_string qname.name in
             (* Prioritize the thunk ONLY if it's not an initializer.
            If it IS an initializer, we let the fallback logic
            in rewrite_to_method handle it as a plain ObjC call.
         *)
             if is_objc_thunk_not_init mangled then Hashtbl.set index ~key:plain ~data:qname ) ) ;
  index


let init_module_state (llair_program : Llair.program) lang =
  let functions = FuncName.Map.to_list llair_program.functions in
  let globals_map = Globals.build_globals_map llair_program.globals in
  let struct_map = Llair2TextualType.translate_types_env lang llair_program.typ_defns in
  let method_class_index = Textual.ProcName.Hashtbl.create 16 in
  let proc_decls, struct_map =
    translate_llair_function_signatures lang method_class_index struct_map functions
  in
  let mangled_map = Llair2TextualTypeName.compute_mangled_map struct_map in
  let plain_map = Llair2TextualTypeName.compute_plain_map struct_map in
  let class_method_index = Textual.TypeName.Hashtbl.create 16 in
  let struct_map =
    Globals.process_globals lang class_method_index method_class_index ~mangled_map ~struct_map
      globals_map
  in
  let proc_decls =
    update_function_signatures lang class_method_index method_class_index ~mangled_map ~struct_map
      ~plain_map proc_decls
  in
  let class_name_offset_map =
    State.ClassMethodIndex.fill_class_name_offset_map class_method_index
  in
  let field_offset_map =
    Field.OffsetIndex.build_field_offset_map ~mangled_map ~plain_map lang struct_map functions
  in
  let struct_map = Type.update_struct_map_with_field_names field_offset_map struct_map in
  let proc_map =
    List.fold proc_decls ~init:Textual.QualifiedProcName.Map.empty ~f:(fun proc_map proc_decl ->
        Textual.QualifiedProcName.Map.add proc_decl.Textual.ProcDecl.qualified_name proc_decl
          proc_map )
  in
  let objc_method_index = populate_objc_method_index proc_decls in
  ModuleState.init ~functions ~struct_map ~mangled_map ~plain_map ~proc_decls ~proc_map ~globals_map
    ~lang ~method_class_index ~class_name_offset_map ~field_offset_map ~objc_method_index


let translate ~source_file ~(module_state : ModuleState.t) : Textual.Module.t =
  let ModuleState.{struct_map; globals_map; lang} = module_state in
  let source_file_ = SourceFile.create source_file in
  let globals, proc_descs =
    Textual.VarName.Map.fold
      (fun _ global (globals, proc_descs) ->
        let global, proc_desc_opt =
          Globals.to_textual_global ~f_to_textual_loc:(to_textual_loc ?proc_state:None)
            ~f_to_textual_exp:(fun ~proc_state loc exp -> to_textual_exp ~proc_state loc exp)
            ~module_state source_file_ global
        in
        (Textual.Module.Global global :: globals, Option.to_list proc_desc_opt @ proc_descs) )
      globals_map ([], [])
  in
  let procs = translate_llair_functions source_file_ ~module_state in
  let structs =
    Textual.TypeName.Map.bindings struct_map
    |> List.map ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
  in
  let decls = procs @ proc_descs @ globals @ structs in
  let attrs = [Textual.Attr.mk_source_language lang] in
  let sourcefile = Textual.SourceFile.create source_file in
  Textual.Module.{attrs; decls; sourcefile}
