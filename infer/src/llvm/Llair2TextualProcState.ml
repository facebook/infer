(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module VarMap = Textual.VarName.Map
module IdentMap = Textual.Ident.Map
module RegMap = Llair.Exp.Reg.Map

type structMap = Textual.Struct.t Textual.TypeName.Map.t

type globalMap = Llair.GlobalDefn.t Textual.VarName.Map.t

type procMap = Textual.ProcDecl.t Textual.QualifiedProcName.Map.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: Textual.Typ.annotated VarMap.t
  ; mutable ids_move: Textual.Typ.annotated IdentMap.t
  ; mutable ids_types: Textual.Typ.annotated IdentMap.t
  ; mutable id_offset: (Textual.Ident.t * int) option
  ; mutable get_element_ptr_offset: (Textual.VarName.t * int) option
  ; mutable reg_map: Textual.Ident.t RegMap.t
  ; mutable last_id: Textual.Ident.t
  ; mutable last_tmp_var: int
  ; struct_map: structMap
  ; globals: globalMap
  ; lang: Textual.Lang.t
  ; proc_map: procMap }

let get_element_ptr_offset_prefix = "getelementptr_offset"

let mk_fresh_id ?reg proc_state =
  let fresh_id ?reg () =
    proc_state.last_id <- Textual.Ident.of_int (Textual.Ident.to_int proc_state.last_id + 1) ;
    ( match reg with
    | Some reg ->
        proc_state.reg_map <- RegMap.add ~key:reg ~data:proc_state.last_id proc_state.reg_map
    | None ->
        () ) ;
    proc_state.last_id
  in
  match reg with
  | Some reg -> (
    match RegMap.find reg proc_state.reg_map with Some id -> id | None -> fresh_id ~reg () )
  | None ->
      fresh_id ()


let mk_fresh_tmp_var name proc_state =
  proc_state.last_tmp_var <- proc_state.last_tmp_var + 1 ;
  Textual.VarName.of_string (Format.sprintf "%s_%d" name proc_state.last_tmp_var)


let last_fake_line : int ref = ref 100

let get_fresh_fake_line () =
  last_fake_line := !last_fake_line + 1 ;
  !last_fake_line


let pp_ids fmt current_ids =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.Ident.pp ~snd:Textual.Typ.pp_annotated))
    (IdentMap.bindings current_ids)


let pp_vars fmt vars =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.VarName.pp ~snd:Textual.Typ.pp_annotated))
    (VarMap.bindings vars)


let pp_struct_map fmt struct_map =
  let pp_item key value =
    F.fprintf fmt "%a -> @\n%a@\n" Textual.TypeName.pp key Textual.Struct.pp value
  in
  Textual.TypeName.Map.iter pp_item struct_map


let pp fmt ~print_types proc_state =
  F.fprintf fmt
    "@[<v>@[<v>qualified_name: %a@]@;\
     @[loc: %a@]@;\
     @[locals: %a@]@;\
     @[formals: %a@]@;\
     @[ids_move: %a@]@;\
     @[ids_types: %a@]@;\
     @[id_offset: %a@]@;\
     @[get_element_ptr_offset: %a@]@;\
     ]@]"
    Textual.QualifiedProcName.pp proc_state.qualified_name Textual.Location.pp proc_state.loc
    pp_vars proc_state.locals pp_vars proc_state.formals pp_ids proc_state.ids_move pp_ids
    proc_state.ids_types
    (Pp.option (Pp.pair ~fst:Textual.Ident.pp ~snd:Int.pp))
    proc_state.id_offset
    (Pp.option (Pp.pair ~fst:Textual.VarName.pp ~snd:Int.pp))
    proc_state.get_element_ptr_offset ;
  if print_types then F.fprintf fmt "types: %a@" pp_struct_map proc_state.struct_map


let update_locals ~proc_state varname typ =
  proc_state.locals <- VarMap.add varname typ proc_state.locals


let update_ids_move ~proc_state id typ =
  proc_state.ids_move <- IdentMap.add id typ proc_state.ids_move


let update_ids_types ~proc_state id typ =
  proc_state.ids_types <- IdentMap.add id typ proc_state.ids_types


let update_id_offset ~proc_state id exp =
  match (exp, proc_state.get_element_ptr_offset) with
  | Textual.Exp.Lvar varname, Some (var, offset) when Textual.VarName.equal var varname ->
      proc_state.id_offset <- Some (id, offset)
  | _ ->
      ()


let update_var_offset ~proc_state varname offset =
  if String.is_prefix ~prefix:get_element_ptr_offset_prefix (Textual.VarName.to_string varname) then
    proc_state.get_element_ptr_offset <- Some (varname, offset)


let reset_offsets ~proc_state =
  proc_state.id_offset <- None ;
  proc_state.get_element_ptr_offset <- None


let global_proc_state lang loc global_var =
  let global_init_name = Format.sprintf "global_init_%s" global_var in
  let qualified_name =
    Textual.QualifiedProcName.
      {enclosing_class= TopLevel; name= Textual.ProcName.of_string global_init_name}
  in
  { qualified_name
  ; loc
  ; formals= VarMap.empty
  ; locals= VarMap.empty
  ; ids_move= IdentMap.empty
  ; ids_types= IdentMap.empty
  ; id_offset= None
  ; get_element_ptr_offset= None
  ; reg_map= RegMap.empty
  ; last_id= Textual.Ident.of_int 0
  ; last_tmp_var= 0
  ; struct_map= Textual.TypeName.Map.empty
  ; globals= VarMap.empty
  ; lang
  ; proc_map= Textual.QualifiedProcName.Map.empty }


let find_method_with_offset ~proc_state struct_name offset =
  let proc_map = proc_state.proc_map in
  let class_procs =
    Textual.QualifiedProcName.Map.filter
      (fun proc_name _ ->
        match Textual.QualifiedProcName.get_class_name proc_name with
        | Some class_name ->
            Textual.TypeName.equal class_name struct_name
        | None ->
            false )
      proc_map
  in
  let _, methods = Textual.QualifiedProcName.Map.bindings class_procs |> List.unzip in
  let find_offset offset_opt (proc_decl : Textual.ProcDecl.t) =
    match offset_opt with
    | Some offset ->
        Some offset
    | None -> (
      match List.find_map ~f:Textual.Attr.get_method_offset proc_decl.attributes with
      | Some method_offset ->
          if Int.equal offset method_offset then Some proc_decl.qualified_name else None
      | None ->
          None )
  in
  List.fold ~init:None ~f:find_offset methods
