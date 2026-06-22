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

(* Frontend-coverage counters consumed by [LlvmFrontend.log_stats] and shipped to Scuba. *)
type frontend_stats = {unsupported_exps: int; unsupported_op2s: int}

let frontend_stats_state = ref {unsupported_exps= 0; unsupported_op2s= 0}

let bump_unsupported_exp () =
  let s = !frontend_stats_state in
  frontend_stats_state := {s with unsupported_exps= s.unsupported_exps + 1}


let bump_unsupported_op2 () =
  let s = !frontend_stats_state in
  frontend_stats_state := {s with unsupported_op2s= s.unsupported_op2s + 1}


let read_and_reset_frontend_stats () =
  let s = !frontend_stats_state in
  frontend_stats_state := {unsupported_exps= 0; unsupported_op2s= 0} ;
  s


(* Swift closure bodies. Mangled with `fU<n>_` for explicit closures and
   `fu<n>_` for implicit/autoclosures (e.g. the body Swift synthesises around
   `Task(operation: self.runAsync)`). Both have empty / `<compiler-generated>`
   DI but their bodies must be translated for retain-cycle analysis to see
   captured-self stores. *)
let is_closure lang s =
  Textual.Lang.is_swift lang
  && (String.is_substring ~substring:"fU" s || String.is_substring ~substring:"fu" s)


let is_init_in_swift_overlay mangled_name =
  String.is_prefix mangled_name ~prefix:"$sSo"
  && (String.is_suffix mangled_name ~suffix:"fC" || String.is_suffix mangled_name ~suffix:"TO")


let is_objc_thunk_not_init mangled_name =
  let is_initializer mangled =
    String.is_suffix mangled ~suffix:"fCTo" || String.is_suffix mangled ~suffix:"fcTo"
  in
  String.is_suffix mangled_name ~suffix:"To" && not (is_initializer mangled_name)


(* Swift async continuation thunks have empty `loc.file` so they don't pass the
   source-file equality check, but their bodies are needed for retain-cycle
   analysis on async methods. The mangled suffixes are `_TY<n>_` (resume after
   suspend) and `_TQ<n>_` (await continuation). *)
let is_async_continuation_thunk mangled_name =
  let len = String.length mangled_name in
  if len < 5 || not (Char.equal mangled_name.[len - 1] '_') then false
  else
    (* Walk backwards over digits *)
    let rec digits_end i =
      if i < 0 then -1 else match mangled_name.[i] with '0' .. '9' -> digits_end (i - 1) | _ -> i
    in
    let after_digits = digits_end (len - 2) in
    after_digits >= 1
    && (Char.equal mangled_name.[after_digits] 'Y' || Char.equal mangled_name.[after_digits] 'Q')
    && Char.equal mangled_name.[after_digits - 1] 'T'


(* Swift partial-apply forwarder thunks. Emitted whenever a method or closure
   reference is bound to its arguments — e.g. `Task(operation: self.runAsync)`,
   `someCallback = self.method`. The thunk captures `self` and forwards the call,
   which is exactly the shape we need for retain-cycle analysis. The thunk has
   `<compiler-generated>` DI so it fails the source-file equality check.

   Mangled suffix is `TA`, optionally followed by an async-continuation suffix
   `T[YQ]<n>_` for partial applies of async functions. *)
let is_partial_apply_thunk mangled_name =
  let len = String.length mangled_name in
  let ends_with_TA i =
    i >= 1 && Char.equal mangled_name.[i] 'A' && Char.equal mangled_name.[i - 1] 'T'
  in
  if len < 2 then false
  else if ends_with_TA (len - 1) then true
  else if len >= 5 && Char.equal mangled_name.[len - 1] '_' then
    (* Check for `TA` followed by `T[YQ]<n>_` continuation suffix *)
    let rec digits_end i =
      if i < 0 then -1 else match mangled_name.[i] with '0' .. '9' -> digits_end (i - 1) | _ -> i
    in
    let after_digits = digits_end (len - 2) in
    after_digits >= 3
    && (Char.equal mangled_name.[after_digits] 'Y' || Char.equal mangled_name.[after_digits] 'Q')
    && Char.equal mangled_name.[after_digits - 1] 'T'
    && ends_with_TA (after_digits - 2)
  else false


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
  bump_unsupported_exp () ;
  (* TODO: should include the arguments here too *)
  (Textual.Exp.call_non_virtual undef_proc_name [], typ, [])


let to_textual_arith_exp_builtin ~loc ~sourcefile (op : Llair.Exp.op2) (typ : Llair.Typ.t) =
  let sil_binop : Binop.t option =
    match (op, typ) with
    | Add, Integer _ ->
        Some (PlusA (Some IInt))
    | Add, Pointer _ ->
        Some PlusPI
    | Add, Float _ ->
        Some (PlusA None)
    | Sub, Integer _ ->
        Some (MinusA (Some IInt))
    | Sub, Pointer _ ->
        Some MinusPI
    | Sub, Float _ ->
        Some (MinusA None)
    | Mul, Integer _ ->
        Some (Mult (Some IInt))
    | Mul, Float _ ->
        Some (Mult None)
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
      bump_unsupported_op2 () ;
      undef_proc_name


let to_textual_bool_exp_builtin (op : Llair.Exp.op2) =
  let sil_bin_op =
    match op with
    | Eq ->
        Binop.Eq
    | Dq ->
        Binop.Ne
    | Gt | Ugt ->
        Binop.Gt
    | Ge | Uge ->
        Binop.Ge
    | Le | Ule ->
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
      Some (Textual.Exp.call_non_virtual proc [exp2])
  | Llair.Exp.Xor when Textual.Exp.is_one_exp exp2 ->
      let proc = Textual.ProcDecl.of_unop Unop.LNot in
      Some (Textual.Exp.call_non_virtual proc [exp1])
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
      (* Llair's [Nondet] is an intentionally non-deterministic value (LLVM
         [undef] / uninitialised memory). The shape we want is exactly the
         [llvm_nondet] builtin call that [undef_exp] also produces, but without
         the [internal_error] log or the unsupported-exp counter bump — those
         should be reserved for genuine coverage gaps. *)
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      ( Textual.Exp.Call
          {proc= undef_proc_name; args= []; kind= NonVirtual; caller_ret_annots= Annot.Item.empty}
      , Some textual_typ
      , [] )
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
        (* When the inner expression is a move-bound register holding the address
           of an inline payload-bearing enum, the GEP'd address IS the inline E
           — there's no separate boxed pointer to load through. The [Field]
           projection below uses that address directly, so skip the synthetic
           load that [add_deref] would otherwise insert. *)
        let skip_deref_for_inline_enum =
          match exp with
          | Textual.Exp.Var ptr_id ->
              IdentMap.find_opt ptr_id proc_state.ProcState.ids_move
              |> Option.bind ~f:(fun (data : ProcState.id_data) -> data.typ)
              |> Option.exists ~f:(fun annot ->
                  match annot.Textual.Typ.typ with
                  | Textual.Typ.Struct name ->
                      Textual.TypeName.swift_mangled_name_of_type_name name
                      |> Option.exists ~f:Type.is_enum_mangled_name
                  | _ ->
                      false )
          | _ ->
              false
        in
        let field_instrs, exp =
          if skip_deref_for_inline_enum then ([], exp)
          else
            let field_instrs, exp = add_deref ~proc_state exp loc in
            (inject_load_type field_instrs typ_name, exp)
        in
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
  | Ap1 (GetElementPtr (StaticByteOffset n), _typ, llair_rcv_exp) as full_exp -> (
      (* Constant byte offset on an opaque pointer (e.g. [-O] inlined a Swift accessor and
         erased the struct type from the GEP).  Try to resolve the receiver to a known Swift
         class struct and look up a field whose declared offset matches [n]; fall back to a
         non-deterministic value when no such field exists, matching the prior behavior of
         this code path. *)
      let try_typed_field () =
        let rcv_exp, rcv_typ_opt, rcv_instrs = to_textual_exp loc ~proc_state llair_rcv_exp in
        (* Prefer the type recovered by [Llair2TextualTypeInference] for the receiver Reg if
           available — it survives the optimiser's opaque-pointer erasure for receivers that
           come out of [swift_allocObject] and similar. *)
        let rcv_typ_opt =
          match llair_rcv_exp with
          | Llair.Exp.Reg {id; _} ->
              Option.first_some (Hashtbl.find proc_state.inferred_types id) rcv_typ_opt
          | _ ->
              rcv_typ_opt
        in
        let extract_struct_name (typ : Textual.Typ.t) =
          match typ with
          | Textual.Typ.Ptr (Struct name, _) | Textual.Typ.Ptr (Ptr (Struct name, _), _) ->
              Some name
          | _ ->
              None
        in
        let rcv_struct_name_opt = Option.bind rcv_typ_opt ~f:extract_struct_name in
        match rcv_struct_name_opt with
        | None ->
            None
        | Some struct_name -> (
          match
            Field.lookup_field_by_byte_offset
              ~field_byte_offset_map:proc_state.module_state.field_byte_offset_map struct_map
              struct_name n
          with
          | None ->
              None
          | Some field ->
              let deref_instrs, rcv_exp = add_deref ~proc_state rcv_exp loc in
              let field_exp = Textual.Exp.Field {exp= rcv_exp; field} in
              let field_typ = Type.lookup_field_type ~struct_map struct_name field in
              Some (field_exp, field_typ, deref_instrs @ rcv_instrs) )
      in
      match try_typed_field () with
      | Some result ->
          result
      | None ->
          let textual_typ =
            Type.to_textual_typ lang ~mangled_map ~struct_map (Llair.Typ.pointer ~elt:_typ)
          in
          let undef, _, undef_instrs =
            undef_exp
              ~reason:(F.asprintf "byte offset %d on opaque pointer not resolved to a field" n)
              ~sourcefile:proc_state.sourcefile ~loc ~typ:textual_typ
              ~proc:proc_state.qualified_name full_exp
          in
          (undef, Some textual_typ, undef_instrs) )
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
      (Textual.Exp.call_non_virtual proc [Textual.Exp.Typ textual_dst_typ; exp], None, instrs)
  | Ap1 (Splat, _, _) ->
      (* [splat exp] initialises every element of an array with the element exp, so to be precise it
       needs to be translated as a loop. We translate here to a non-deterministic value for the array *)
      let proc = undef_proc_name in
      (Textual.Exp.call_non_virtual proc [], None, [])
  | Ap2 (((Add | Sub | Mul | Div | Rem) as op), typ, e1, e2) ->
      let proc = to_textual_arith_exp_builtin ~loc ~sourcefile:proc_state.sourcefile op typ in
      let exp1, typ1, exp1_instrs = to_textual_exp loc ~proc_state e1 in
      let deref_instrs1, exp1 = add_deref ~proc_state exp1 loc in
      let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state e2 in
      let deref_instrs2, exp2 = add_deref ~proc_state exp2 loc in
      ( Textual.Exp.call_non_virtual proc [exp1; exp2]
      , typ1
      , deref_instrs1 @ exp1_instrs @ deref_instrs2 @ exp2_instrs )
  | Ap2
      ( ( (Eq | Dq | Gt | Ge | Le | Lt | Ugt | Uge | Ule | Ult | And | Or | Xor | Shl | Lshr | Ashr)
          as op )
      , _
      , e1
      , e2 ) ->
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
          Textual.Exp.call_non_virtual proc [exp1; exp2]
        else
          match to_textual_not_exp op exp1 exp2 with
          | Some exp ->
              exp
          | None ->
              let proc = to_textual_bool_exp_builtin op in
              Textual.Exp.call_non_virtual proc [exp1; exp2]
      in
      (exp, typ1, deref_instrs1 @ exp1_instrs @ deref_instrs2 @ exp2_instrs)
  | Ap2 (Update idx, typ, rcd, elt) ->
      let rcd_exp, _, rcd_instrs = to_textual_exp loc ~proc_state rcd in
      let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state elt in
      let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
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
      (* Return in reverse-execution order, matching the convention used by the
         [cmnd_to_instrs] consumers: instr at position 0 runs LAST in execution.
         Concrete exec order is: compute [rcd_instrs], compute [elt_instrs],
         materialise the elt via [elt_deref_instrs], then [store_instr].
         Sub-results [rcd_instrs] and [elt_instrs] are themselves rev-exec
         from the recursive [to_textual_exp]. *)
      (rcd_exp, Some textual_typ, (store_instr :: elt_deref_instrs) @ elt_instrs @ rcd_instrs)
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
            Textual.Exp.call_non_virtual proc []
          in
          let rcd_store_instr = Textual.Instr.Let {id= Some id; exp= undef_exp; loc} in
          (* For each element in the record, set the value to a field of the
             record variable. Returned [instrs] is in reverse-execution order
             (instr at position 0 runs LAST), matching the convention used by
             [cmnd_to_instrs] consumers and by the [Ap2 (Update ...)] case
             above. Concrete forward-exec for one element is: compute
             [elt_instrs], materialise via [elt_deref_instrs], then
             [store_instr] at the field. Earlier elements run first, so their
             (rev-exec) blocks go AFTER later ones in the list;
             [rcd_store_instr] runs first overall, so it sits at the very
             tail. *)
          let to_textual_exp_index idx acc_instrs element =
            let elt_exp, _, elt_instrs = to_textual_exp loc ~proc_state element in
            let elt_deref_instrs, elt_exp_deref = add_deref ~proc_state elt_exp loc in
            let field =
              if Llair.Typ.is_tuple typ then Field.tuple_field_of_pos type_name idx
              else
                Field.field_of_pos_with_map proc_state.module_state.field_offset_map type_name idx
            in
            let index_exp = Textual.Exp.Field {exp= rcd_exp; field} in
            let store_instr =
              Textual.Instr.Store {exp1= index_exp; exp2= elt_exp_deref; typ= None; loc}
            in
            ((store_instr :: elt_deref_instrs) @ elt_instrs) @ acc_instrs
          in
          let instrs = List.foldi ~f:to_textual_exp_index ~init:[rcd_store_instr] elements in
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
            Some
              (Textual.Exp.Call
                 { proc= proc_name
                 ; args
                 ; kind= Textual.Exp.Virtual
                 ; caller_ret_annots= Annot.Item.empty } )
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
          let skip_deref_for = [Models.swift_weak_assign; Models.swift_weak_init] in
          if
            Textual.Lang.is_swift proc_state.module_state.lang
            && not
                 (List.mem skip_deref_for proc.Textual.QualifiedProcName.name
                    ~equal:Textual.ProcName.equal )
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
        (Textual.Exp.Call {proc; args; kind; caller_ret_annots= Annot.Item.empty}, [])
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
          let name = FuncName.name func.Llair.name in
          let proc =
            if
              String.equal name (Procname.get_method BuiltinDecl.__assert_fail)
              || String.is_substring ~substring:"assertionFailure" name
            then Textual.ProcDecl.assert_fail_name
            else Proc.to_qualified_proc_name proc_state.module_state.method_class_index name
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
          Textual.Exp.call_non_virtual builtin_alloc_proc args
      | None ->
          call_exp )
    | _ ->
        call_exp
  in
  let call_exp = Models.resolve_objc_msgSend ~proc_state call_exp arg_types in
  let call_exp =
    (* If the original LLAIR [objc_msgSend] [areturn] was identified by the
       bridge-analysis prepass as carrying a structural nullability signal
       (an [as?]-cast re-bridge or an [Optional<T>] passthrough getter),
       attach a [Nullable] caller_ret_annots so SwiftObjCNullability does
       not report on the call. *)
    match (call.areturn, call_exp) with
    | Some areturn, Textual.Exp.Call ({caller_ret_annots; _} as call_record)
      when List.is_empty caller_ret_annots
           && Hashtbl.mem proc_state.ProcState.nullability_hint_msg_sends (Llair.Reg.id areturn) ->
        Textual.Exp.Call {call_record with caller_ret_annots= [Annot.nullable]}
    | _ ->
        call_exp
  in
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


(* Rewrite Optional discriminator stores into calls to [__swift_optional_init_{none,some}].
   Recognises both the [Sg]-class shape (e.g. [Int?]) and the 2-component
   [__infer_tuple_class<int,int>] shape (e.g. [String?]). *)
let base_is_load_of_sg_pvar ~(proc_state : ProcState.t) ~exp1_instrs base_exp textual_instrs =
  (* Find the [Load { id= base_var_id; exp= Lvar pvar }] that produced [base_var_id] and
     check whether [pvar]'s declared local type ends in [Sg]. *)
  match base_exp with
  | Textual.Exp.Var base_id -> (
      let check_instr instr =
        match (instr : Textual.Instr.t) with
        | Load {id; exp= Textual.Exp.Lvar pvar; _} when Textual.Ident.equal id base_id ->
            Textual.VarName.Map.find_opt pvar proc_state.ProcState.locals
            |> Option.bind ~f:(fun (annot : Textual.Typ.annotated) ->
                match annot.typ with
                | Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) ->
                    Textual.TypeName.swift_mangled_name_of_type_name typ_name
                | _ ->
                    None )
            |> Option.value_map ~default:None ~f:(fun mangled ->
                if String.is_suffix mangled ~suffix:"Sg" then Some () else None )
        | _ ->
            None
      in
      match List.find_map exp1_instrs ~f:check_instr with
      | Some _ as result ->
          result
      | None ->
          List.find_map textual_instrs ~f:check_instr )
  | _ ->
      None


let try_rewrite_optional_discriminator_store ~proc_state ~exp1 ~exp2 ~loc ~exp1_instrs
    textual_instrs =
  let open IOption.Let_syntax in
  let* (field : Textual.qualified_fieldname), base_exp =
    match exp1 with Textual.Exp.Field {exp= base_exp; field} -> Some (field, base_exp) | _ -> None
  in
  let* () =
    Option.some_if
      ( String.equal field.name.value "field_1"
      || String.equal field.name.value "__infer_tuple_field_1" )
      ()
  in
  (* Sg-class: tag 1 = none, 0 = some.  Tuple-class: 0 = none, non-zero = some. *)
  let class_is_sg =
    Textual.TypeName.swift_mangled_name_of_type_name field.enclosing_class
    |> Option.exists ~f:(String.is_suffix ~suffix:"Sg")
  in
  let is_tuple_field_1 = String.equal field.name.value "__infer_tuple_field_1" in
  let is_two_component_tuple =
    Textual.BaseTypeName.equal field.enclosing_class.name
      Textual.BaseTypeName.swift_tuple_class_name
    && match field.enclosing_class.args with [_; _] -> true | _ -> false
  in
  let* shape =
    if class_is_sg && String.equal field.name.value "field_1" then Some `Sg_shape
    else if is_tuple_field_1 && is_two_component_tuple then
      Option.map (base_is_load_of_sg_pvar ~proc_state ~exp1_instrs base_exp textual_instrs)
        ~f:(fun () -> `Tuple_shape )
    else None
  in
  let* discriminator =
    match (shape, exp2) with
    | `Sg_shape, Textual.Exp.Const (Int z) when Z.equal z Z.zero ->
        Some `Some_
    | `Sg_shape, Textual.Exp.Const (Int z) when Z.equal z Z.one ->
        Some `None_
    | `Tuple_shape, Textual.Exp.Const (Int z) when Z.equal z Z.zero ->
        (* tag = 0 -> none *)
        Some `None_
    | `Tuple_shape, Textual.Exp.Const (Int z) when not (Z.equal z Z.zero) ->
        Some `Some_
    | `Sg_shape, _ ->
        (* Symbolic discriminator on [Sg]-class storage: emit the
           path-split builtin and let Pulse prune on the runtime tag
           value.  Covers interprocedural Optional flows where the by-value
           ABI reconstructs the Optional locally with a parameter-derived
           tag.  Tuple-shape writes do NOT path-split here because the
           bridge synthesises literal-[.some] Optionals via a runtime
           tuple load whose tag looks symbolic in SIL but is semantically
           constant -- a tuple-shape path-split injects a spurious
           [.none]-disjunct that downstream unwrap models then fire on. *)
        Some `Symbolic
    | `Tuple_shape, _ ->
        None
  in
  let init_call proc args =
    let exp = Textual.Exp.call_non_virtual (Models.builtin_qual_proc_name proc) args in
    Textual.Instr.Let {id= None; exp; loc}
  in
  match discriminator with
  | `None_ ->
      let call_instr =
        init_call (SwiftProcname.show_builtin SwiftProcname.OptionalInitNone) [base_exp]
      in
      Some (call_instr, textual_instrs)
  | `Some_ ->
      let is_field_0_store_of_same_class instr =
        match (instr : Textual.Instr.t) with
        | Store
            { exp1=
                Textual.Exp.Field
                  { exp= _
                  ; field= {Textual.enclosing_class= ec; name= ({value; _} : Textual.FieldName.t)}
                  } } ->
            (* Adjacent payload write (fresh load SSA names are common). *)
            Textual.TypeName.equal ec field.enclosing_class
            && (String.equal value "field_0" || String.equal value "__infer_tuple_field_0")
        | _ ->
            false
      in
      let payload, tail =
        match textual_instrs with
        | (Textual.Instr.Store {exp2= payload} as head) :: rest
          when is_field_0_store_of_same_class head ->
            (payload, rest)
        | _ ->
            (Textual.Exp.Const (Textual.Const.Int Z.one), textual_instrs)
      in
      let call_instr =
        init_call (SwiftProcname.show_builtin SwiftProcname.OptionalInitSome) [base_exp; payload]
      in
      Some (call_instr, tail)
  | `Symbolic ->
      (* Always [Sg]-shape here (tuple-shape symbolic case returns None above). *)
      let call_instr =
        init_call (SwiftProcname.show_builtin SwiftProcname.OptionalInitSg) [base_exp; exp2]
      in
      Some (call_instr, textual_instrs)


let cmnd_to_instrs ~(proc_state : ProcState.t) ~metadata_extract_loads block =
  let ModuleState.{lang; struct_map; mangled_map} = proc_state.module_state in
  let is_metadata_extract reg = Llair2TextualMetadataLoad.mem reg metadata_extract_loads in
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
        (* Metadata-extract idiom: [load %X] where [%X] is a formal-Reg and the
           loaded value is used only as the value operand of Stores. The LLVM
           SSA value [%X] is the parameter (a pointer); [load %X] reads through
           it. Textual's slot convention gives the parameter value with a single
           Load on [&var_X], so we add an extra deref to actually read at *X.
           Classification is done function-wide in [Llair2TextualMetadataLoad];
           do not relax the use-only-in-Stores predicate without also reworking
           the [__sil_cast (ptr-to-inferred_typ) (Var tmp_id)] shape below: the
           cast bridges [tmp_id : *formal_typ] to the pointer-to-inferred_typ
           the outer Load's verifier expects, and only typechecks because
           [tmp_id] holds the slot value of a formal whose type is recovered. *)
        let extra_deref_instrs, exp =
          match ptr with
          | Llair.Exp.Reg {name; id= reg_id; typ= reg_typ} when is_metadata_extract reg -> (
              let var_name = Var.reg_to_var_name (Reg.mk reg_typ reg_id name) in
              match Textual.VarName.Map.find_opt var_name proc_state.formals with
              | Some {ProcState.typ= formal_typ; _} ->
                  let tmp_id = Var.add_fresh_id ~proc_state () in
                  let tmp_load =
                    Textual.Instr.Load {id= tmp_id; exp; typ= Some formal_typ.Textual.Typ.typ; loc}
                  in
                  ProcState.update_ids_types ~proc_state tmp_id formal_typ ;
                  let casted_exp =
                    match inferred_typ with
                    | Some t ->
                        Textual.Exp.cast (Textual.Typ.mk_ptr t) (Textual.Exp.Var tmp_id)
                    | None ->
                        Textual.Exp.Var tmp_id
                  in
                  ([tmp_load], casted_exp)
              | None ->
                  ([], exp) )
          | _ ->
              ([], exp)
        in
        let textual_instr = Textual.Instr.Load {id; exp; typ= inferred_typ; loc} in
        textual_instr :: List.append extra_deref_instrs (List.append ptr_instrs textual_instrs)
    | Store {ptr; exp}
      when Textual.Lang.is_swift lang && is_store_formal_to_local ~proc_state ptr exp ->
        textual_instrs
    | Store {ptr; exp; loc} ->
        let loc = to_textual_loc_instr ~proc_state loc in
        let exp2, _, exp2_instrs = to_textual_exp loc ~proc_state exp in
        let exp2_deref_instrs, exp2 = add_deref ~proc_state exp2 loc in
        let exp1, typ_exp1, exp1_instrs = to_textual_exp loc ~proc_state ptr in
        (* Move-bound registers carry only the LLVM opaque pointer at the [Reg]
           lookup, but the move handler stashed the typed RHS expression in
           [ids_move]. Recover that type so the inline-enum arm below can fire
           on stores through a register that points at an inline E. The
           recovered field type [Struct E] is re-wrapped as [*E] to share the
           same [Ptr (Struct ...)] match shape with the other arms. *)
        let typ_exp1 =
          match (exp1, typ_exp1) with
          | Textual.Exp.Var id, _ -> (
            match IdentMap.find_opt id proc_state.ProcState.ids_move with
            | Some {typ= Some annot; _} -> (
              match annot.Textual.Typ.typ with
              | Textual.Typ.Struct _ as inline_typ
                when Type.is_ptr_enum (Textual.Typ.mk_ptr inline_typ) ->
                  Some (Textual.Typ.mk_ptr inline_typ)
              | typ when Type.is_ptr_enum typ ->
                  Some typ
              | _ ->
                  typ_exp1 )
            | _ ->
                typ_exp1 )
          | _ ->
              typ_exp1
        in
        let exp1, exp1_deref_instrs =
          (* In llvm when we store the struct (or an optional int) without accessing any fields,
          it means the first field. The first field of a struct is always at offset 0, so the base
          pointer already points to it. No offset calculation is needed. *)
          match (exp1, typ_exp1) with
          | Textual.Exp.Lvar _, Some (Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) as typ_exp)
            when Type.is_ptr_struct typ_exp || Type.is_ptr_enum typ_exp ->
              translate_store_in_field_zero ~(proc_state : ProcState.t) exp1 loc typ_name
          | _, Some (Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) as typ_exp)
            when Type.is_int_optional typ_exp ->
              translate_store_in_field_zero ~(proc_state : ProcState.t) exp1 loc typ_name
          | ( (Textual.Exp.Var _ | Textual.Exp.Field _)
            , Some (Textual.Typ.Ptr (Textual.Typ.Struct typ_name, _) as typ_exp) )
            when Type.is_ptr_enum typ_exp ->
              (* Inline payload-bearing enum: with the field declared inline,
                 a raw scalar store at the GEP'd enum address writes the
                 enum's first inline field. LLVM doesn't bother with the
                 [GEP E,0,0] for that write, so wrap explicitly with
                 [.E.field_0] to keep the store target well-typed. *)
              let field_exp =
                Textual.Exp.Field
                  { exp= exp1
                  ; field=
                      Field.field_of_pos_with_map proc_state.module_state.field_offset_map typ_name
                        0 }
              in
              (field_exp, [])
          | _ ->
              (exp1, [])
        in
        let textual_instr_opt =
          if remove_store_zero_in_class typ_exp1 exp2 then None
          else Some (Textual.Instr.Store {exp1; typ= None; exp2; loc})
        in
        (* Swift Optional [.none]/[.some(_)] construction: if the store we'd emit is a
           discriminator-write on an [Sg]-suffixed (Optional) class field, rewrite it to a
           call to one of the [__swift_optional_init_{none,some}] builtins.  See
           [try_rewrite_optional_discriminator_store]. *)
        let head_instrs, tail_instrs =
          if Textual.Lang.is_swift lang then
            match textual_instr_opt with
            | Some _ -> (
              match
                try_rewrite_optional_discriminator_store ~proc_state ~exp1 ~exp2 ~loc ~exp1_instrs
                  textual_instrs
              with
              | Some (replacement_instr, new_tail) ->
                  ([replacement_instr], new_tail)
              | None ->
                  (Option.to_list textual_instr_opt, textual_instrs) )
            | None ->
                ([], textual_instrs)
          else (Option.to_list textual_instr_opt, textual_instrs)
        in
        (head_instrs @ exp2_deref_instrs @ exp2_instrs)
        @ exp1_instrs @ exp1_deref_instrs @ tail_instrs
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
    | Builtin {reg; name; args; loc}
      when Textual.Lang.is_swift lang && Llair.Builtin.equal name `swift_optional_unsafelyUnwrapped
      ->
        (* [Optional<T>.unsafelyUnwrapped]: recognised at the LLVM
           frontend and routed to the Pulse builtin via the
           [SwiftProcname.Builtin] path used by [OptionalInit{None,Some}]. *)
        let loc = to_textual_loc ~proc_state loc in
        let args = StdUtils.iarray_to_list args in
        let call_textual_instrs =
          to_textual_builtin ~proc_state reg Models.swift_optional_unsafely_unwrapped args loc
        in
        List.append call_textual_instrs textual_instrs
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


let rec to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads (jump : Llair.jump)
    =
  let block = jump.dst in
  let node_label = block_to_node_name block in
  let node_label, typ_opt, succs =
    (* If we've seen this node, stop the recursion *)
    if Textual.NodeName.HashSet.mem seen_nodes node_label then
      (node_label, None, Textual.Node.Set.empty)
    else
      let (node : Textual.Node.t), typ_opt, nodes =
        block_to_node_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads block
      in
      (node.label, typ_opt, nodes)
  in
  let node_call = Textual.Terminator.{label= node_label; ssa_args= []} in
  (Textual.Terminator.Jump [node_call], typ_opt, succs)


and to_terminator_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads term =
  let no_succs = Textual.Node.Set.empty in
  match term with
  | Call {return; loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ( to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads return
      , Some loc
      , [] )
  | Return {exp= Some exp; loc= loc_} ->
      let loc = to_textual_loc_instr ~proc_state loc_ in
      let textual_exp, textual_typ_opt, instrs = to_textual_exp loc ~proc_state exp in
      let exp_deref_instrs, textual_exp = add_deref ~proc_state textual_exp loc in
      (* [to_textual_exp] returns [instrs] in reverse-execution order (matching
         the [cmnd_to_instrs] accumulator convention). [block_to_node_and_succs]
         appends our [term_instrs] onto the body in forward-execution order
         (post-[List.rev] from [cmnd_to_instrs]), so reverse [instrs] here. The
         single-instruction [exp_deref_instrs] is appended last in exec order
         and so comes after the reversed [instrs]. *)
      let instrs = List.rev instrs @ exp_deref_instrs in
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
            to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads zero_jump
          in
          let then_, if_typ, els_nodes =
            to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads els
          in
          let term = Textual.Terminator.If {bexp; then_; else_} in
          let nodes = Textual.Node.Set.union zero_nodes els_nodes in
          let typ_opt = Type.join_typ if_typ else_typ in
          (* [instrs] from [to_textual_bool_exp] is rev-exec; reverse for
             [block_to_node_and_succs]'s forward-exec append. *)
          ((term, typ_opt, nodes), Some loc, List.rev instrs)
      | [] when Exp.equal key Exp.false_ ->
          (* goto *)
          ( to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads els
          , Some loc
          , [] )
      | entries ->
          (* multi-entry switch: lower as a chain of nested if-then-else,
             comparing the key against each table entry and falling through
             to [els]. *)
          let key_textual, _, key_instrs = to_textual_exp loc ~proc_state key in
          let key_deref_instrs, key_textual = add_deref ~proc_state key_textual loc in
          let els_term, els_typ, els_nodes =
            to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads els
          in
          let eq_proc = to_textual_bool_exp_builtin Eq in
          let term, typ_opt, nodes, case_instrs_acc =
            List.fold_right entries ~init:(els_term, els_typ, els_nodes, [])
              ~f:(fun (case_exp, jump) (acc_term, acc_typ, acc_nodes, acc_instrs) ->
                let case_textual, _, case_instrs = to_textual_exp loc ~proc_state case_exp in
                let case_deref_instrs, case_textual = add_deref ~proc_state case_textual loc in
                let bexp =
                  Textual.BoolExp.Exp
                    (Textual.Exp.call_non_virtual eq_proc [key_textual; case_textual])
                in
                let then_term, then_typ, then_nodes =
                  to_textual_jump_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads jump
                in
                let term = Textual.Terminator.If {bexp; then_= then_term; else_= acc_term} in
                ( term
                , Type.join_typ then_typ acc_typ
                , Textual.Node.Set.union then_nodes acc_nodes
                , (List.rev case_instrs @ case_deref_instrs) @ acc_instrs ) )
          in
          ( (term, typ_opt, nodes)
          , Some loc
          , (List.rev key_instrs @ key_deref_instrs) @ case_instrs_acc ) )
  | Iswitch {loc} | Abort {loc} | Unreachable {loc} ->
      let loc = to_textual_loc_instr ~proc_state loc in
      ((Textual.Terminator.Unreachable, None, no_succs), Some loc, [])


(* TODO still various parts of the node left to be translated *)
and block_to_node_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads (block : Llair.block) =
  let node_name = block_to_node_name block in
  let instrs, first_loc, last_loc = cmnd_to_instrs ~proc_state ~metadata_extract_loads block in
  Textual.NodeName.HashSet.add node_name seen_nodes ;
  let (terminator, typ_opt, succs), term_loc_opt, term_instrs =
    to_terminator_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads block.term
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
  let metadata_extract_loads = Llair2TextualMetadataLoad.classify ~proc_state func in
  let _, typ_opt, nodes =
    block_to_node_and_succs ~proc_state ~seen_nodes ~metadata_extract_loads func.Llair.entry
  in
  (typ_opt, Textual.Node.Set.to_list nodes)


let is_undefined func =
  let entry = func.Llair.entry in
  match entry.term with
  | Unreachable _ ->
      String.equal entry.lbl "undefined" && List.is_empty (StdUtils.iarray_to_list entry.cmnd)
  | _ ->
      false


(* The literal string swiftc emits in LLVM debug info for synthetic procedures
   ([<compiler-generated>]). Matched against the [file] field of a proc's DI loc
   to decide whether the proc belongs to a real source or a compiler-generated one.
   Swift-frontend specific, hence local to this module. *)
let compiler_generated_di_marker = "<compiler-generated>"

(* A proc's LLVM debug info is "compiler-generated" when it has no real source file —
   either the [file] field is empty or it's the literal [<compiler-generated>] marker
   that swiftc emits for synthetic thunks (partial-apply, autoclosure bodies, async
   continuations, overlay initializers, ObjC bridging thunks, witness-table accessors). *)
let is_compiler_generated_loc lang (loc : Llair.Loc.t) =
  let file =
    if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
    else loc.Llair.Loc.file
  in
  String.is_empty file || String.equal file compiler_generated_di_marker


(* Predicate matching procedures whose body must be translated even though their DI
   doesn't point at any user source file. We collect all of these into a single
   synthetic [<compiler-generated>] Textual module so each one is translated once
   instead of once per source-file capture pass. *)
let matches_synthetic_filter plain_name mangled_name lang typ_name =
  is_closure lang mangled_name
  || is_init_in_swift_overlay mangled_name
  || is_objc_thunk_not_init mangled_name
  || (Textual.Lang.is_swift lang && is_async_continuation_thunk mangled_name)
  || (Textual.Lang.is_swift lang && is_partial_apply_thunk mangled_name)
  || Option.exists
       ~f:(fun name -> String.is_suffix ~suffix:Globals.witness_protocol_suffix name)
       typ_name
  || Option.exists plain_name ~f:(fun plain_name ->
      String.is_substring ~substring:Field.get_suffix plain_name
      || String.is_substring ~substring:Field.set_suffix plain_name
      || String.is_substring ~substring:Field.modify_suffix plain_name )


type translate_mode = User_pass of SourceFile.t | Compiler_generated_pass

let should_translate ~mode plain_name mangled_name lang method_class_index class_files_map
    (loc : Llair.Loc.t) =
  let file =
    if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
    else loc.Llair.Loc.file
  in
  let source_file_loc = SourceFile.create file in
  let proc_name = Textual.ProcName.of_string mangled_name in
  let class_typ_name = Textual.ProcName.Hashtbl.find_opt method_class_index proc_name in
  let typ_name =
    Option.bind class_typ_name ~f:(fun name ->
        Textual.TypeName.swift_mangled_name_of_type_name name )
  in
  let is_synthetic = is_compiler_generated_loc lang loc in
  let synthetic_match = matches_synthetic_filter plain_name mangled_name lang typ_name in
  (* A [synthetic_match] proc (e.g. a [.get]/[.set]/[.modify] accessor whose DI
     points at a real but irrelevant file) should only be broadcast into the
     per-source-file pass for the file(s) where its enclosing class is defined.
     Otherwise the accessor gets emitted into every [User_pass], and Pulse's
     report attribution lands on whichever pass deduplicates last. When the
     class has no recoverable source file (top-level free functions, true
     unknowns) we fall back to the current broadcast behaviour. *)
  let synthetic_match_for_this_file source_file =
    if not synthetic_match then false
    else
      match Option.bind class_typ_name ~f:(Textual.TypeName.Hashtbl.find_opt class_files_map) with
      | Some files when not (SourceFile.Set.is_empty files) ->
          SourceFile.Set.mem source_file files
      | _ ->
          true
  in
  match mode with
  | User_pass source_file ->
      (* In the per-source-file pass, translate procs whose DI matches this source
         file. Synthetic-DI procs (empty / <compiler-generated>) are skipped here and
         handled exclusively by the compiler-generated pass. *)
      (not is_synthetic)
      && (SourceFile.equal source_file source_file_loc || synthetic_match_for_this_file source_file)
  | Compiler_generated_pass ->
      is_synthetic && synthetic_match


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


let loc_file lang (loc : Llair.Loc.t) =
  if Textual.Lang.is_c lang then loc.Llair.Loc.dir ^ "/" ^ loc.Llair.Loc.file
  else loc.Llair.Loc.file


(* A real source file, i.e. not empty and not the [<compiler-generated>] DI marker. *)
let is_real_di_file f =
  (not (String.is_empty f)) && not (String.equal f compiler_generated_di_marker)


(* Distinct real source files on the function body's instruction [DILocation]s (whole CFG). *)
let func_instr_files lang (func : Llair.func) =
  Llair.Func.fold_cfg func IString.Set.empty ~f:(fun block acc ->
      StdUtils.iarray_to_list block.Llair.cmnd
      |> List.fold ~init:acc ~f:(fun acc inst ->
          let f = loc_file lang (Inst.loc inst) in
          if is_real_di_file f then IString.Set.add f acc else acc ) )


(* [accessor] for [.get]/[.set]/[.modify] property accessors, [function] otherwise. *)
let func_flavor func_name =
  match FuncName.unmangled_name func_name with
  | Some n
    when String.is_substring n ~substring:Field.get_suffix
         || String.is_substring n ~substring:Field.set_suffix
         || String.is_substring n ~substring:Field.modify_suffix ->
      "accessor"
  | _ ->
      "function"


(* Scuba probe (T273760556 / T273770363): the DISubprogram file [should_translate] routes by can be
   one the body never references, anchoring reports in the wrong file. Fire when it's on no body
   instruction. *)
let log_disubprogram_file_mismatch lang func_name (func : Llair.func) =
  let func_file = loc_file lang func.Llair.loc in
  if is_real_di_file func_file then
    let instr_files = func_instr_files lang func in
    if (not (IString.Set.is_empty instr_files)) && not (IString.Set.mem func_file instr_files) then (
      StatsLogging.log_count ~label:"swift_disubprogram_file_mismatch" ~value:1 ;
      StatsLogging.log_message_with_location ~label:"swift_disubprogram_file_mismatch"
        ~loc:(Format.asprintf "%s:%d" func_file func.Llair.loc.Llair.Loc.line)
        ~message:
          (Format.asprintf "proc=%s flavor=%s subprogram=%s:%d instr_files=[%s]"
             (FuncName.name func_name) (func_flavor func_name) func_file
             func.Llair.loc.Llair.Loc.line
             (String.concat ~sep:"," (IString.Set.elements instr_files)) ) )


let translate_code ~mode ~sourcefile_for_proc_state ~module_state proc_descs
    (procdecl : Textual.ProcDecl.t) (func_name, func) =
  let ModuleState.{lang; method_class_index; class_files_map} = module_state in
  let should_translate =
    should_translate ~mode
      (FuncName.unmangled_name func_name)
      (FuncName.name func_name) lang method_class_index class_files_map func.Llair.loc
  in
  if should_translate && Textual.Lang.is_swift lang then
    log_disubprogram_file_mismatch lang func_name func ;
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
  let nullability_hint_msg_sends =
    if should_translate then
      Llair2TextualBridgeAnalysis.find_msgsends_with_recoverable_nullability func
    else Hashtbl.create (module Int)
  in
  let proc_state =
    ProcState.init ~qualified_name:procdecl.qualified_name ~sourcefile:sourcefile_for_proc_state
      ~loc ~formals:formals_map ~module_state ~inferred_types ~nullability_hint_msg_sends
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
  let attributes =
    if proc_state.ProcState.captures_self_weakly then
      Textual.Attr.mk_weak_self_capture :: procdecl.Textual.ProcDecl.attributes
    else procdecl.Textual.ProcDecl.attributes
  in
  let procdecl = {procdecl with result_type; formals_types; attributes} in
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


let translate_llair_functions ~mode ~sourcefile_for_proc_state ~(module_state : ModuleState.t) =
  let ModuleState.{proc_decls; functions; _} = module_state in
  List.fold2_exn proc_decls functions ~init:[]
    ~f:(translate_code ~mode ~sourcefile_for_proc_state ~module_state)


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
  let class_method_index = Textual.TypeName.Hashtbl.create 16 in
  let struct_map =
    Globals.process_globals lang class_method_index method_class_index ~mangled_map ~struct_map
      globals_map
  in
  (* [plain_map] must be computed after [process_globals], which is where the
     [Textual.TypeName.t] keys in [struct_map] gain their plain-name annotations
     (the second positional arg). Computing it earlier produces an empty map and
     silently breaks downstream [struct_name_of_plain_name] lookups. *)
  let plain_map = Llair2TextualTypeName.compute_plain_map struct_map in
  let proc_decls =
    update_function_signatures lang class_method_index method_class_index ~mangled_map ~struct_map
      ~plain_map proc_decls
  in
  let class_name_offset_map =
    State.ClassMethodIndex.fill_class_name_offset_map class_method_index
  in
  (* Build class -> source files map for [should_translate]'s
     synthetic-broadcast fallback. Iterate the Llair functions; for each function
     with a real DI source file, look up its enclosing class via
     [method_class_index] and record (class, file). A class can land in multiple
     files (e.g. extensions split across modules), so we accumulate sets. *)
  let class_files_map = Textual.TypeName.Hashtbl.create 16 in
  List.iter functions ~f:(fun (func_name, (func : Llair.func)) ->
      if not (is_compiler_generated_loc lang func.loc) then
        let proc_name = Textual.ProcName.of_string (FuncName.name func_name) in
        match Textual.ProcName.Hashtbl.find_opt method_class_index proc_name with
        | None ->
            ()
        | Some class_name ->
            let file =
              if Textual.Lang.is_c lang then func.loc.Llair.Loc.dir ^ "/" ^ func.loc.Llair.Loc.file
              else func.loc.Llair.Loc.file
            in
            let source_file = SourceFile.create file in
            let prev =
              Option.value
                (Textual.TypeName.Hashtbl.find_opt class_files_map class_name)
                ~default:SourceFile.Set.empty
            in
            Textual.TypeName.Hashtbl.replace class_files_map class_name
              (SourceFile.Set.add source_file prev) ) ;
  let field_offset_map =
    Field.OffsetIndex.build_field_offset_map ~mangled_map ~plain_map lang struct_map functions
  in
  let struct_map = Type.update_struct_map_with_field_names field_offset_map struct_map in
  let struct_map = Globals.process_wvd_globals ~lang ~mangled_map globals_map struct_map in
  let struct_map =
    Globals.mark_weak_fields_from_swift_weak_assigns ~lang ~mangled_map functions struct_map
  in
  (* Add struct decls for synthetic closure-shaped types ([swift::function]) so
     every per-source-file textual module declares the closure layout. Without
     this, [TextualBasicVerification.fix] sees [UnknownField] on closure field
     accesses in modules whose [typ_defns] didn't happen to include the type,
     rewrites the enclosing class to [wildcard], and routes the field through
     [wildcard_sil_fieldname] — which drops [is_weak] and demotes any retain
     cycle crossing the field to [RETAIN_CYCLE_NO_WEAK_INFO]. *)
  let struct_map = Llair2TextualType.inject_synthetic_closure_decls lang struct_map in
  let field_byte_offset_map =
    Field.build_field_byte_offset_map lang ~mangled_map struct_map globals_map
  in
  let proc_map =
    List.fold proc_decls ~init:Textual.QualifiedProcName.Map.empty ~f:(fun proc_map proc_decl ->
        Textual.QualifiedProcName.Map.add proc_decl.Textual.ProcDecl.qualified_name proc_decl
          proc_map )
  in
  let objc_method_index = populate_objc_method_index proc_decls in
  ModuleState.init ~functions ~struct_map ~mangled_map ~plain_map ~proc_decls ~proc_map ~globals_map
    ~lang ~method_class_index ~class_files_map ~class_name_offset_map ~field_offset_map
    ~field_byte_offset_map ~objc_method_index


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
  let procs =
    translate_llair_functions ~mode:(User_pass source_file_) ~sourcefile_for_proc_state:source_file_
      ~module_state
  in
  let structs =
    Textual.TypeName.Map.bindings struct_map
    |> List.map ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
  in
  let decls = procs @ proc_descs @ globals @ structs in
  let attrs = [Textual.Attr.mk_source_language lang] in
  let sourcefile = Textual.SourceFile.create source_file in
  Llair2TextualForceUnwrap.rewrite_module Textual.Module.{attrs; decls; sourcefile}


(* Translate just the compiler-generated procedures (Swift partial-apply thunks,
   autoclosure bodies, async continuations, overlay initializers, ObjC bridging
   thunks, witness-table accessors) into a canonical Textual module attributed to
   [SourceFile.compiler_generated ~bitcode_id]. Called once per capture run, outside
   the per-source-file loop. Returns [None] when there are no compiler-generated
   procs to translate (so callers can skip downstream verification / SIL conversion).
   Struct and global declarations are emitted alongside the procs so SIL verification
   sees every referenced name; the same decls also appear in the per-source-file
   modules but last-write-wins on the global tenv makes the duplication harmless. *)
let translate_compiler_generated ~bitcode_id ~(module_state : ModuleState.t) :
    Textual.Module.t option =
  let ModuleState.{struct_map; globals_map; lang} = module_state in
  let sourcefile_for_proc_state = SourceFile.compiler_generated ~bitcode_id in
  let procs =
    translate_llair_functions ~mode:Compiler_generated_pass ~sourcefile_for_proc_state ~module_state
  in
  let has_any_proc = List.exists procs ~f:(function Textual.Module.Proc _ -> true | _ -> false) in
  if not has_any_proc then None
  else
    let globals, proc_descs =
      Textual.VarName.Map.fold
        (fun _ global (globals, proc_descs) ->
          let global, proc_desc_opt =
            Globals.to_textual_global ~f_to_textual_loc:(to_textual_loc ?proc_state:None)
              ~f_to_textual_exp:(fun ~proc_state loc exp -> to_textual_exp ~proc_state loc exp)
              ~module_state sourcefile_for_proc_state global
          in
          (Textual.Module.Global global :: globals, Option.to_list proc_desc_opt @ proc_descs) )
        globals_map ([], [])
    in
    let structs =
      Textual.TypeName.Map.bindings struct_map
      |> List.map ~f:(fun (_, struct_) -> Textual.Module.Struct struct_)
    in
    let decls = procs @ proc_descs @ globals @ structs in
    let attrs = [Textual.Attr.mk_source_language lang] in
    let sourcefile = Textual.SourceFile.create (SourceFile.to_string sourcefile_for_proc_state) in
    Some (Llair2TextualForceUnwrap.rewrite_module Textual.Module.{attrs; decls; sourcefile})
