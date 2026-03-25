(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState
module Globals = Llair2TextualGlobals
module Var = Llair2TextualVar
module IdentMap = Textual.Ident.Map

(* --- Dependency Injection Types --- *)
type f_to_textual_exp =
     proc_state:ProcState.t
  -> Textual.Location.t
  -> Exp.t
  -> Textual.Exp.t * Textual.Typ.t option * Textual.Instr.t list

type f_add_deref =
     proc_state:ProcState.t
  -> Textual.Exp.t
  -> Textual.Location.t
  -> Textual.Instr.t list * Textual.Exp.t

let swift_weak_assign = Textual.ProcName.of_string "swift_weakAssign"

let llvm_dynamic_call = Textual.ProcName.of_string "llvm_dynamic_call"

let derived_enum_equals = "__derived_enum_equals"

let llvm_init_tuple = "llvm_init_tuple"

let swift_get_dynamic_type = SwiftProcname.to_string (SwiftProcname.Builtin SwiftGetDynamicType)

let swift_metadata_equals = SwiftProcname.to_string (SwiftProcname.Builtin MetadataEquals)

let swift_instantiateConcreteTypeFromMangledName = "__swift_instantiateConcreteTypeFromMangledName"

let functions_to_skip =
  List.map ~f:Textual.ProcName.of_string
    [ "swift_unknownObjectRetain"
    ; "swift_weakLoadStrong"
    ; "swift_bridgeObjectRetain"
    ; "swift_getObjCClassFromMetadata" ]


let boxed_opaque_existentials =
  [ "__swift_mutable_project_boxed_opaque_existential_1"
  ; "__swift_project_boxed_opaque_existential_1" ]


let builtin_qual_proc_name =
  let enclosing_class = Textual.(QualifiedProcName.Enclosing (TypeName.of_string "$builtins")) in
  fun name : Textual.QualifiedProcName.t ->
    {enclosing_class; name= Textual.ProcName.of_string name; metadata= None}


let is_protocol_witness_optional_deinit_copy lang mangled_name =
  let suffixes = ["WOd"; "WOb"; "WOc"] in
  Textual.Lang.is_swift lang
  && List.exists ~f:(fun s -> String.is_suffix ~suffix:s mangled_name) suffixes
  && String.is_substring ~substring:"_p" mangled_name


module Metadata = struct
  let metadata_response = "swift::metadata_response"

  let is_swift_metadata_response_name name = String.equal name metadata_response

  let is_metadata_field (field : Textual.qualified_fieldname) =
    let mangled_class_name =
      Textual.TypeName.swift_mangled_name_of_type_name field.enclosing_class
    in
    let field_name = Textual.FieldName.to_string field.name in
    match mangled_class_name with
    | Some name ->
        is_swift_metadata_response_name name && String.equal field_name "field_0"
    | None ->
        false


  let propagate ~proc_state id exp ~is_load =
    match exp with
    | Textual.Exp.Field {field} when is_metadata_field field ->
        if is_load then ProcState.mark_as_metadata ~proc_state id
        else ProcState.mark_as_metadata_address ~proc_state id
    | Textual.Exp.Var src_id ->
        if is_load then
          if ProcState.is_metadata_address_id ~proc_state src_id then
            ProcState.mark_as_metadata ~proc_state id
          else if ProcState.is_metadata_id ~proc_state src_id then
            ProcState.mark_as_metadata ~proc_state id
          else if ProcState.is_metadata_address_id ~proc_state src_id then
            ProcState.mark_as_metadata_address ~proc_state id
    | _ ->
        ()


  let translate_ma_accessor ~(proc_state : ProcState.t) proc_name_str loc =
    let ModuleState.{lang; struct_map; mangled_map; _} = proc_state.module_state in
    let middle = String.sub proc_name_str ~pos:2 ~len:(String.length proc_name_str - 4) in
    let class_mangled_name = "T" ^ middle in
    let class_type_name =
      TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
        class_mangled_name
    in
    let id = ProcState.mk_fresh_id proc_state in
    let init_metadata_exp =
      Textual.Exp.Call {proc= builtin_qual_proc_name llvm_init_tuple; args= []; kind= NonVirtual}
    in
    let let_metadata = Textual.Instr.Let {id= Some id; exp= init_metadata_exp; loc} in
    let alloc_args = [Textual.Exp.Typ (Textual.Typ.Struct class_type_name)] in
    let alloc_exp =
      Textual.Exp.Call {proc= Textual.ProcDecl.swift_alloc_name; args= alloc_args; kind= NonVirtual}
    in
    let metadata_resp_type = Textual.TypeName.mk_swift_type_name metadata_response in
    let field =
      Field.field_of_pos_with_map proc_state.module_state.field_offset_map metadata_resp_type 0
    in
    let field_access_exp = Textual.Exp.Field {exp= Textual.Exp.Var id; field} in
    let store_instr =
      Textual.Instr.Store {exp1= field_access_exp; typ= None; exp2= alloc_exp; loc}
    in
    (Textual.Exp.Var id, [store_instr; let_metadata])


  let translate_instantiate_mangled_name ~(proc_state : ProcState.t) global_name_str =
    let ModuleState.{lang; struct_map; mangled_map; _} = proc_state.module_state in
    let middle = String.sub global_name_str ~pos:2 ~len:(String.length global_name_str - 4) in
    let class_mangled_name = "T" ^ middle in
    let class_type_name =
      TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
        class_mangled_name
    in
    let alloc_args = [Textual.Exp.Typ (Textual.Typ.Struct class_type_name)] in
    let alloc_exp =
      Textual.Exp.Call {proc= Textual.ProcDecl.swift_alloc_name; args= alloc_args; kind= NonVirtual}
    in
    (alloc_exp, [])


  let try_translate_swift_metadata_call ~proc_state (proc : Textual.QualifiedProcName.t) llair_args
      return_id loc =
    let proc_name_str = Textual.ProcName.to_string proc.name in
    if
      String.equal proc_name_str swift_get_dynamic_type
      || String.equal proc_name_str swift_instantiateConcreteTypeFromMangledName
      || String.is_substring ~substring:"Cm" proc_name_str
    then Option.iter return_id ~f:(fun id -> ProcState.mark_as_metadata ~proc_state id) ;
    if String.is_suffix proc_name_str ~suffix:"Ma" then
      let exp_instrs = translate_ma_accessor ~proc_state proc_name_str loc in
      Some exp_instrs
    else if String.equal proc_name_str swift_instantiateConcreteTypeFromMangledName then
      let extract_global_name (exp : Llair.Exp.t) =
        match exp with Global {name; _} -> Some name | _ -> None
      in
      let global_name_opt = List.hd llair_args |> Option.bind ~f:extract_global_name in
      match global_name_opt with
      | Some global_name_str ->
          let exp, instrs = translate_instantiate_mangled_name ~proc_state global_name_str in
          Some (exp, instrs)
      | _ ->
          None
    else None
end

let translate_optional_protocol_witness ~proc_state exp exp_typ typ_name n =
  let ModuleState.{struct_map; mangled_map; lang; _} = proc_state.ProcState.module_state in
  let exp_typ =
    if Int.equal n 3 then
      match exp with
      | Textual.Exp.Var ident -> (
        match IdentMap.find_opt ident proc_state.ProcState.ids_move with
        | Some id_data ->
            Option.map ~f:(fun typ -> typ.Textual.Typ.typ) id_data.ProcState.typ
        | None ->
            None )
      | Textual.Exp.Lvar _ ->
          exp_typ
      | _ ->
          None
    else None
  in
  match exp_typ with
  | Some (Textual.Typ.Ptr (Struct struct_name, _)) -> (
      let optional_protocol_suffix = "_pSg" in
      match Textual.TypeName.swift_mangled_name_of_type_name struct_name with
      | Some name when String.is_suffix name ~suffix:optional_protocol_suffix ->
          let name = String.substr_replace_all ~pattern:optional_protocol_suffix ~with_:"P" name in
          let protocol_typ_name =
            TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
              name
          in
          (protocol_typ_name, 0)
      | _ ->
          (typ_name, n) )
  | _ ->
      (typ_name, n)


let translate_boxed_opaque_existential ~f_to_textual_exp ~f_add_deref ~proc_state loc llair_args =
  let arg = List.hd_exn llair_args in
  let typ =
    match arg with
    | Llair.Exp.Reg {id; name; typ} -> (
        let exp, exp_typ = Var.reg_to_textual_var ~proc_state (Reg.mk typ id name) in
        match (exp, exp_typ) with
        | _, Some _ ->
            exp_typ
        | Textual.Exp.Var var_name, None -> (
          match Textual.Ident.Map.find_opt var_name proc_state.ProcState.ids_move with
          | Some {typ= Some annotated_typ} ->
              Some annotated_typ.typ
          | _ ->
              None )
        | _ ->
            None )
    | _ ->
        None
  in
  match typ with
  | Some (Textual.Typ.Ptr (Struct struct_name, _)) ->
      let elts = NS.IArray.init 1 ~f:(fun _ -> lazy (1, Llair.Typ.bool)) in
      let name = Textual.TypeName.swift_mangled_name_of_type_name struct_name |> Option.value_exn in
      let name = String.substr_replace_all ~pattern:"_pSg" ~with_:"P" name in
      let llair_typ = Typ.struct_ ~name elts ~bits:0 ~byts:0 in
      let llair_exp = Llair.Exp.select llair_typ arg 0 in
      let textual_exp, _, instrs = f_to_textual_exp ~proc_state loc llair_exp in
      let deref_instrs, textual_exp = f_add_deref ~proc_state textual_exp loc in
      Some (textual_exp, deref_instrs @ instrs)
  | _ ->
      None


let translate_protocol_witness_optional_deinit_copy ~f_to_textual_exp ~f_add_deref ~proc_state ptr
    exp loc =
  let exp2, _, exp2_instrs = f_to_textual_exp ~proc_state loc exp in
  let exp2_deref_instrs, exp2 = f_add_deref ~proc_state exp2 loc in
  let exp1, _, exp1_instrs = f_to_textual_exp ~proc_state loc ptr in
  let instr = Textual.Instr.Store {exp1; typ= None; exp2; loc} in
  let instrs = exp2_deref_instrs @ exp2_instrs @ exp1_instrs in
  instr :: instrs


(** Peels away __sil_cast calls to find the underlying Identifier *)
let rec get_base_ident exp =
  match exp with
  | Some (Textual.Exp.Var id) ->
      Some id
  | Some (Textual.Exp.Call {proc; args; _})
    when String.is_substring (Textual.ProcName.to_string proc.name) ~substring:"sil_cast" ->
      (* sil_cast usually has the form: cast<type>(value) *)
      get_base_ident (List.last args)
  | _ ->
      None


let rewrite_to_method ~(proc_state : ProcState.t) method_name args arg_types =
  let module_state = proc_state.module_state in
  (* Extract the receiver (Index 0) *)
  let receiver = List.hd args |> Option.to_list in
  (* IMPORTANT: Index 1 is the selector. We MUST skip it.
     List.drop args 2 starts taking from Index 2 onwards.
  *)
  let actual_params = List.drop args 2 in
  (* 1. Search the objc_method_index for a Swift implementation ($s...To) *)
  let resolved_qname_opt = Hashtbl.find module_state.objc_method_index method_name in
  match resolved_qname_opt with
  | Some qname ->
      (* CASE: Found a Swift @objc implementation in the LLVM file *)
      let mangled_str = Textual.ProcName.to_string qname.name in
      let final_args =
        if String.is_suffix mangled_str ~suffix:"To" then
          (* Thunk: (Receiver, Null_Selector, ...Params) *)
          receiver @ [Textual.Exp.Const Null] @ actual_params
        else
          (* Native: (Receiver, ...Params) *)
          receiver @ actual_params
      in
      Textual.Exp.Call {proc= qname; args= final_args; kind= Virtual}
  | None ->
      (* CASE: Fallback for ObjC Land (External/SDK) *)
      let receiver_type_opt = List.hd arg_types in
      let enclosing_class =
        match receiver_type_opt with
        | Some (Some (Textual.Typ.Ptr ((Struct type_name as struct_typ), _)))
          when not (Textual.Typ.equal struct_typ Textual.Typ.any_type_swift) -> (
          match Textual.TypeName.swift_plain_name_of_type_name type_name with
          | Some plain_name ->
              Textual.QualifiedProcName.Enclosing (Textual.TypeName.of_string plain_name)
          | None -> (
            match Textual.TypeName.swift_mangled_name_of_type_name type_name with
            | Some mangled_name ->
                Textual.QualifiedProcName.Enclosing (Textual.TypeName.of_string mangled_name)
            | None ->
                Textual.QualifiedProcName.TopLevel ) )
        | _ ->
            Textual.QualifiedProcName.TopLevel
      in
      (* Tag as Objective-C so Pulse uses Clang-style naming convention *)
      let metadata =
        Some
          { Textual.QualifiedProcName.lang= Some Textual.Lang.ObjectiveC
          ; method_kind= Some Textual.QualifiedProcName.InstanceMethod }
      in
      let qname =
        Textual.QualifiedProcName.
          {name= Textual.ProcName.of_string method_name; enclosing_class; metadata}
      in
      let final_args = receiver @ actual_params in
      Textual.Exp.Call {proc= qname; args= final_args; kind= Virtual}


let resolve_objc_msgSend ~proc_state call_exp arg_types =
  match call_exp with
  | Textual.Exp.Call {proc; args; _} ->
      let proc_name = Textual.ProcName.to_string proc.name in
      if
        String.is_substring proc_name ~substring:"objc_msgSend"
        || String.is_substring proc_name ~substring:"performSelector"
      then
        let selector_lit_arg = List.nth args 1 in
        let selector_lit_name =
          match get_base_ident selector_lit_arg with
          | Some id ->
              ProcState.find_selector proc_state id
          | _ ->
              None
        in
        match selector_lit_name with
        | Some "performSelector:" -> (
            let real_method_arg = List.nth args 2 in
            match get_base_ident real_method_arg with
            | Some id -> (
              match ProcState.find_selector proc_state id with
              | Some method_name ->
                  (* For performSelector(obj, perf_sel, target_sel), we need to
                     construct a new args list: [obj, target_sel]
                     so that rewrite_to_method's 'drop 2' works correctly. *)
                  let receiver = List.hd args |> Option.to_list in
                  let target_selector = Option.to_list real_method_arg in
                  let remaining_args = List.drop args 3 in
                  let new_args = receiver @ target_selector @ remaining_args in
                  rewrite_to_method ~proc_state method_name new_args arg_types
              | None ->
                  call_exp )
            | _ ->
                call_exp )
        | Some method_name ->
            (* Standard objc_msgSend(obj, sel, ...) matches drop 2 perfectly *)
            rewrite_to_method ~proc_state method_name args arg_types
        | None ->
            call_exp
      else call_exp
  | _ ->
      call_exp


let save_metadata_type ~proc_state call llair_args (proc : Textual.QualifiedProcName.t) =
  if String.equal (Textual.ProcName.to_string proc.name) "swift_getObjCClassFromMetadata" then
    match llair_args with
    | [Llair.Exp.Reg {id; name; typ}] -> (
        let _, type_opt = Var.reg_to_textual_var ~proc_state (Reg.mk typ id name) in
        match type_opt with
        | Some (Textual.Typ.Ptr (Struct type_name, _)) -> (
          match Textual.TypeName.swift_plain_name_of_type_name type_name with
          | Some metatype_string ->
              let actual_type_name = TypeName.extract_class_from_metatype metatype_string in
              Option.iter call.areturn ~f:(fun ret_reg ->
                  let var_name = Var.reg_to_var_name ret_reg in
                  ProcState.add_class_type proc_state var_name actual_type_name )
          | None ->
              () )
        | _ ->
            () )
    | _ ->
        ()


let get_alloc_class_name =
  let swift_alloc_object = Textual.ProcName.of_string "swift_allocObject" in
  let alloc_with_zone = Textual.ProcName.of_string "allocWithZone:" in
  let alloc = Textual.ProcName.of_string "alloc" in
  let objc_alloc = Textual.ProcName.of_string "objc_alloc" in
  let objc_allocWithZone = Textual.ProcName.of_string "objc_allocWithZone" in
  let ns_object = Textual.TypeName.of_string "NSObject" in
  fun ~proc_state proc_name llair_args ->
    match Textual.QualifiedProcName.get_class_name proc_state.ProcState.qualified_name with
    | Some class_name when Textual.ProcName.equal proc_name swift_alloc_object ->
        Some (class_name, Textual.ProcDecl.swift_alloc_name)
    | _
      when Textual.ProcName.equal proc_name alloc_with_zone
           || Textual.ProcName.equal proc_name alloc
           || Textual.ProcName.equal proc_name objc_alloc
           || Textual.ProcName.equal proc_name objc_allocWithZone ->
        let tracked_type_opt =
          match llair_args with
          | [Llair.Exp.Reg {id; name; typ}] ->
              let var_name = Var.reg_to_var_name (Reg.mk typ id name) in
              ProcState.get_class_type proc_state var_name
          | _ ->
              None
        in
        let class_name = Option.value tracked_type_opt ~default:ns_object in
        Some (class_name, Textual.ProcDecl.objc_alloc_name)
    | _ ->
        None


let update_selector_metadata ~(proc_state : ProcState.t) id_opt call_exp proc_name =
  match id_opt with
  | Some return_id -> (
      let textual_args = match call_exp with Textual.Exp.Call {args} -> args | _ -> [] in
      if String.is_substring proc_name ~substring:"builtinStringLiteral" then
        match textual_args with
        | Textual.Exp.Const (Str s) :: _ ->
            ProcState.add_string_literal proc_state return_id s
        | _ ->
            ()
      else if String.is_substring proc_name ~substring:"bridgeToObjectiveC" then
        match textual_args with
        | Textual.Exp.Var src_id :: _ ->
            ProcState.get_string_literal proc_state src_id
            |> Option.iter ~f:(ProcState.add_string_literal proc_state return_id)
        | _ ->
            ()
      else if String.is_substring proc_name ~substring:"NSSelectorFromString" then
        match textual_args with
        | Textual.Exp.Var src_id :: _ -> (
          match ProcState.get_string_literal proc_state src_id with
          | Some s ->
              ProcState.add_selector proc_state return_id s
          | None ->
              ProcState.get_last_string_added proc_state
              |> Option.iter ~f:(ProcState.add_selector proc_state return_id) )
        | _ ->
            () )
  | None ->
      ()


(** Scans the instruction list to see if [id] was loaded from [global_name] *)
let is_load_of_global id instrs global_name =
  List.exists instrs ~f:(function
    | Textual.Instr.Load {id= load_id; exp= Textual.Exp.Lvar name; _} ->
        Textual.Ident.equal id load_id && String.equal (Textual.VarName.to_string name) global_name
    | _ ->
        false )


(** Models the Swift runtime's 'swift_dynamicCast' by unrolling it into direct memory operations.

  In Swift/ObjC interop, values are often wrapped in 'AnyObject' existential containers
  (boxes). For example, an Int '5' returned from an @objc method is stored inside the
  container's value buffer (field_0).

  Without this model, Pulse treats the cast as an unknown function, causing the
  symbolic value '5' to be lost. This model:
  1. Verifies the destination metadata (e.g., Swift.Int via global $sSiN).
  2. Generates a 'Load' to extract the raw value from the source AnyObject box.
  3. Generates a 'Store' to place that value into the destination type's storage.
  4. Returns a success constant (1) so branching logic in the CFG remains valid.

  This enables end-to-end value tracking through dynamic dispatch and protocol casting.
*)
let rewrite_swift_dynamic_cast ~(proc_state : ProcState.t) loc processed_args args_instrs return_id
    =
  let module_state = proc_state.module_state in
  let field_map = module_state.field_offset_map in
  let any_object_struct = Textual.TypeName.mk_swift_type_name "AnyObject" in
  let tsi_struct = Textual.TypeName.mk_swift_type_name "TSi" in
  match processed_args with
  | [dest_ptr; src_ptr; _src_metadata; dest_metadata_exp; _flags] ->
      let is_int_metadata =
        match dest_metadata_exp with
        | Textual.Exp.Var id ->
            is_load_of_global id args_instrs Globals.int_metadata_global
        | _ ->
            Globals.is_int_metadata_exp dest_metadata_exp
      in
      if is_int_metadata then
        let val_id = ProcState.mk_fresh_id proc_state in
        let any_object_field0 = Field.field_of_pos_with_map field_map any_object_struct 0 in
        let tsi_field0 = Field.field_of_pos_with_map field_map tsi_struct 0 in
        let src_field_exp = Textual.Exp.Field {exp= src_ptr; field= any_object_field0} in
        (* 1. LOAD: This defines val_id (n51) *)
        let unbox_load = Textual.Instr.Load {id= val_id; exp= src_field_exp; typ= None; loc} in
        let dest_field_exp = Textual.Exp.Field {exp= dest_ptr; field= tsi_field0} in
        (* 2. STORE: This USES val_id (n51) *)
        let unbox_store =
          Textual.Instr.Store {exp1= dest_field_exp; typ= None; exp2= Textual.Exp.Var val_id; loc}
        in
        (* 3. LET: This sets the return value (n50) *)
        let ret_instr =
          match return_id with
          | Some id ->
              [Textual.Instr.Let {id= Some id; exp= Textual.Exp.Const (Int Z.one); loc}]
          | None ->
              []
        in
        [unbox_store; unbox_load] @ ret_instr
      else []
  | _ ->
      []


let try_rewrite_call_to_instrs ~proc_state ~f_to_textual_exp ~f_add_deref ~loc ~call_exp ~llair_args
    ~args_instrs ~id =
  match (call_exp, llair_args) with
  (* 1. swift_dynamicCast -> Load/Store unboxing *)
  | Textual.Exp.Call {proc; args= processed_args}, _
    when String.is_substring (Textual.ProcName.to_string proc.name) ~substring:"swift_dynamicCast"
    ->
      let model_instrs = rewrite_swift_dynamic_cast ~proc_state loc processed_args args_instrs id in
      if List.is_empty model_instrs then None else Some model_instrs
  (* 2. swift_weakAssign -> Store *)
  | Textual.Exp.Call {proc; args= [arg1; arg2]}, _
    when Textual.ProcName.equal proc.Textual.QualifiedProcName.name swift_weak_assign ->
      let instrs2, arg2_deref = f_add_deref ~proc_state arg2 loc in
      Some (Textual.Instr.Store {exp1= arg1; typ= None; exp2= arg2_deref; loc} :: instrs2)
  (* 3. Protocol Witness Optional Deinit Copy *)
  | Textual.Exp.Call {proc}, [exp; ptr]
    when is_protocol_witness_optional_deinit_copy proc_state.ProcState.module_state.lang
           (Textual.ProcName.to_string proc.Textual.QualifiedProcName.name) ->
      Some
        (translate_protocol_witness_optional_deinit_copy ~f_to_textual_exp ~f_add_deref ~proc_state
           ptr exp loc )
  | _ ->
      None
