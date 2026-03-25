(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module Type = Llair2TextualType
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState

let witness_protocol_suffix = "WP"

let class_virtual_table_suffix = "Mf"

let build_globals_map globals =
  let add_global map global =
    let name = global.GlobalDefn.name |> Global.name |> Textual.VarName.of_string in
    Textual.VarName.Map.add name global map
  in
  let globals = StdUtils.iarray_to_list globals in
  List.fold ~f:add_global globals ~init:Textual.VarName.Map.empty


let add_method_to_class_method_index class_method_index class_name proc_name index =
  let methods =
    Textual.TypeName.Hashtbl.find_opt class_method_index class_name |> Option.value ~default:[]
  in
  Textual.TypeName.Hashtbl.replace class_method_index class_name ((proc_name, index) :: methods)


let class_from_global ~lang ~mangled_map ~suffix struct_map global_name =
  let name =
    if String.equal suffix witness_protocol_suffix then global_name
    else
      let name = String.substr_replace_first global_name ~pattern:"$s" ~with_:"T" in
      String.chop_suffix_exn name ~suffix
  in
  TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map name


let process_func_name ~lang ~mangled_map ~class_method_index ~method_class_index struct_map name
    ~suffix global offset =
  let class_name = class_from_global ~lang ~mangled_map ~suffix struct_map global in
  let proc_name = Textual.ProcName.of_string name in
  let proc =
    Textual.QualifiedProcName.
      {enclosing_class= Enclosing class_name; name= proc_name; metadata= None}
  in
  add_method_to_class_method_index class_method_index class_name proc offset ;
  Textual.ProcName.Hashtbl.replace method_class_index proc_name class_name


let process_exp ~lang ~mangled_map ~class_method_index ~method_class_index struct_map global ~suffix
    (last_offset, carry) exp typ =
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
          process_func_name ~lang ~mangled_map ~class_method_index ~method_class_index struct_map
            name ~suffix global (offset - 3)
      | _ ->
          () ) ;
      (offset, 0)
  | _ ->
      (last_offset, carry)


let process_exp_witness_protocol ~lang ~mangled_map ~class_method_index ~method_class_index
    struct_map global ~suffix offset exp =
  match exp with
  | Llair.Exp.FuncName {name} ->
      process_func_name ~lang ~mangled_map ~class_method_index ~method_class_index struct_map name
        ~suffix global offset
  | _ ->
      ()


let collect_class_method_indices ~lang ~mangled_map ~class_method_index ~method_class_index
    struct_map ~suffix global_name exp =
  match exp with
  | Llair.Exp.ApN (Record, Llair.Typ.Tuple {elts}, elements)
  | Llair.Exp.ApN (Record, Llair.Typ.Struct {elts}, elements) ->
      let elements = StdUtils.iarray_to_list elements in
      let _, types = StdUtils.iarray_to_list elts |> List.unzip in
      ignore
        (List.fold2_exn
           ~f:
             (process_exp ~lang ~mangled_map ~class_method_index ~method_class_index struct_map
                global_name ~suffix )
           ~init:(-1, 0) elements types )
  | Llair.Exp.ApN (Record, Llair.Typ.Array _, elements) ->
      let elements = StdUtils.iarray_to_list elements in
      ignore
        (List.mapi
           ~f:
             (process_exp_witness_protocol ~lang ~mangled_map ~class_method_index
                ~method_class_index struct_map global_name ~suffix )
           elements )
  | _ ->
      ()


let process_wvd_global ~lang ~mangled_map global_name struct_map =
  if String.is_suffix global_name ~suffix:"Wvd" then
    let class_name_opt, field_name_str = Field.extract_class_and_field_from_wvd global_name in
    match class_name_opt with
    | Some mangled_class ->
        let class_name =
          TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
            mangled_class
        in
        let field_name = Textual.FieldName.of_string field_name_str in
        let field_decl =
          Textual.FieldDecl.
            { qualified_name= {enclosing_class= class_name; name= field_name}
            ; typ= Textual.Typ.any_type_swift
            ; attributes= [] }
        in
        let struct_ =
          match Textual.TypeName.Map.find_opt class_name struct_map with
          | Some (struct_ : Textual.Struct.t) ->
              let has_llvm_layout =
                List.exists struct_.fields ~f:(fun f ->
                    String.is_prefix
                      (Textual.FieldName.to_string f.Textual.FieldDecl.qualified_name.name)
                      ~prefix:"field_" )
              in
              if has_llvm_layout then struct_
              else if
                List.exists struct_.fields ~f:(fun (field : Textual.FieldDecl.t) ->
                    Textual.FieldName.equal field.qualified_name.name field_name )
              then struct_
              else {struct_ with fields= field_decl :: struct_.fields}
          | None ->
              Textual.Struct.{name= class_name; supers= []; fields= [field_decl]; attributes= []}
        in
        Textual.TypeName.Map.add class_name struct_ struct_map
    | None ->
        struct_map
  else struct_map


let process_global_item ~lang ~mangled_map ~class_method_index ~method_class_index _var global
    struct_map =
  match global with
  | GlobalDefn.{name; init= Some exp_typ} ->
      let global_name = Global.name name in
      let struct_map = process_wvd_global ~lang ~mangled_map global_name struct_map in
      let suffix = "C" ^ class_virtual_table_suffix in
      if String.is_suffix global_name ~suffix then (
        let class_name =
          class_from_global ~lang ~mangled_map ~suffix:class_virtual_table_suffix struct_map
            global_name
        in
        let struct_map =
          if Textual.TypeName.Map.mem class_name struct_map then struct_map
          else
            let struct_ =
              Textual.Struct.{name= class_name; supers= []; fields= []; attributes= []}
            in
            Textual.TypeName.Map.add class_name struct_ struct_map
        in
        collect_class_method_indices ~lang ~mangled_map ~class_method_index ~method_class_index
          struct_map ~suffix:class_virtual_table_suffix global_name (fst exp_typ) ;
        struct_map )
      else
        let suffix = witness_protocol_suffix in
        if String.is_suffix global_name ~suffix then (
          let struct_name = TypeName.to_textual_type_name lang global_name in
          let struct_ =
            Textual.Struct.{name= struct_name; supers= []; fields= []; attributes= []}
          in
          let struct_map = Textual.TypeName.Map.add struct_name struct_ struct_map in
          collect_class_method_indices ~lang ~mangled_map ~class_method_index ~method_class_index
            struct_map ~suffix global_name (fst exp_typ) ;
          struct_map )
        else struct_map
  | _ ->
      struct_map


let process_globals lang class_method_index method_class_index ~mangled_map ~struct_map globals_map
    =
  Textual.VarName.Map.fold
    (process_global_item ~lang ~mangled_map ~class_method_index ~method_class_index)
    globals_map struct_map


type special_global = WitnessProtocol of string | SwiftMetadata of string | Standard

let classify_global name =
  if String.is_suffix name ~suffix:witness_protocol_suffix then WitnessProtocol name
  else if String.is_prefix name ~prefix:"$s" && String.is_suffix name ~suffix:"N" then
    SwiftMetadata name
  else Standard


(* Unique Identity Tags for Swift Types to help Pulse recognize them *)
let any_object_metadata_global = "$syXlN"

let any_object_metadata_tag = 1001

let int_metadata_global = "$sSiN"

let int_metadata_tag = 1002

let string_metadata_global = "$sSSN"

let string_metadata_tag = 1003

let get_synthetic_metadata_constant global_name =
  if String.equal global_name any_object_metadata_global then
    Some (Textual.Exp.Const (Int (Z.of_int any_object_metadata_tag)))
  else if String.equal global_name int_metadata_global then
    Some (Textual.Exp.Const (Int (Z.of_int int_metadata_tag)))
  else if String.equal global_name string_metadata_global then
    Some (Textual.Exp.Const (Int (Z.of_int string_metadata_tag)))
  else None


let is_int_metadata_exp exp =
  match exp with Textual.Exp.Const (Int i) -> Int.equal (Z.to_int i) int_metadata_tag | _ -> false


let to_textual_global ~f_to_textual_loc ~f_to_textual_exp ~module_state sourcefile global =
  let ModuleState.{lang; struct_map; mangled_map; _} = module_state in
  let global_ = global.GlobalDefn.name in
  let global_name = Global.name global_ in
  let name = Textual.VarName.of_string global_name in
  let global_typ =
    match global.GlobalDefn.init with Some (_, typ) -> typ | None -> Global.typ global_
  in
  let typ = Type.to_textual_typ lang ~mangled_map ~struct_map global_typ in
  (* Initialize Swift metadata globals with their constant Identity Tags *)
  let init_exp = get_synthetic_metadata_constant global_name in
  let proc_desc_opt =
    if Config.llvm_translate_global_init then
      let loc = f_to_textual_loc global.GlobalDefn.loc in
      let global_proc_state = ProcState.global_proc_state sourcefile loc module_state global_name in
      match global.GlobalDefn.init with
      | Some (exp, _) ->
          let init_exp, _, instrs = f_to_textual_exp ~proc_state:global_proc_state loc exp in
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
          Some (Textual.Module.Proc proc_desc)
      | _ ->
          None
    else None
  in
  let global = Textual.Global.{name; typ; attributes= []; init_exp} in
  (global, proc_desc_opt)


let rec translate_global ~proc_state ~lang ~struct_map ~mangled_map ~name ~typ =
  match classify_global name with
  | WitnessProtocol name ->
      if
        Option.is_some
          (Textual.TypeName.Map.find_opt (Textual.TypeName.mk_swift_type_name name) struct_map)
      then
        let class_name =
          TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map name
        in
        let args = [Textual.Exp.Typ (Textual.Typ.Struct class_name)] in
        let exp =
          Textual.Exp.Call
            {proc= Textual.ProcDecl.swift_alloc_name; args; kind= Textual.Exp.NonVirtual}
        in
        (exp, None, [])
      else handle_standard_global ~proc_state ~lang ~struct_map ~mangled_map ~name ~typ
  | SwiftMetadata name ->
      (* Return the Lvar (pointer). Pulse will load the ID value from this pointer. *)
      let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
      (Textual.Exp.Lvar (Textual.VarName.of_string name), Some textual_typ, [])
  | Standard ->
      handle_standard_global ~proc_state ~lang ~struct_map ~mangled_map ~name ~typ


and handle_standard_global ~(proc_state : ProcState.t) ~lang ~struct_map ~mangled_map ~name ~typ =
  let textual_typ = Type.to_textual_typ lang ~mangled_map ~struct_map typ in
  let global_info_opt =
    Textual.VarName.Map.find_opt (Textual.VarName.of_string name)
      proc_state.module_state.globals_map
  in
  let string_opt =
    Option.bind global_info_opt ~f:(fun g ->
        Option.bind g.Llair.GlobalDefn.init ~f:(fun (exp, _) -> Llair.Exp.string_of_exp exp) )
  in
  let typ_opt =
    Option.bind global_info_opt ~f:(fun g ->
        Option.map g.Llair.GlobalDefn.init ~f:(fun (_, t) ->
            Type.to_textual_typ lang ~mangled_map ~struct_map t ) )
  in
  let textual_exp =
    match string_opt with
    | Some s ->
        Textual.Exp.Const (Str s)
    | None ->
        Textual.Exp.Lvar (Textual.VarName.of_string name)
  in
  let final_typ = Option.value typ_opt ~default:textual_typ in
  (textual_exp, Some final_typ, [])
