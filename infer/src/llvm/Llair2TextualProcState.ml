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

type methodClassIndex = Textual.TypeName.t Textual.ProcName.Hashtbl.t

let method_class_index : methodClassIndex = Textual.ProcName.Hashtbl.create 16

module ClassNameOffset = struct
  type t = {class_name: Textual.TypeName.t; offset: int} [@@deriving compare, hash, equal]
end

module ClassNameOffsetMap = Stdlib.Hashtbl.Make (ClassNameOffset)

type classNameOffsetMap = Textual.QualifiedProcName.t ClassNameOffsetMap.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; sourcefile: SourceFile.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: (Textual.Typ.annotated * Textual.VarName.t option) VarMap.t
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
  ; proc_map: procMap
  ; class_name_offset_map: Textual.QualifiedProcName.t ClassNameOffsetMap.t }

let init_state ~qualified_name ~sourcefile ~loc ~formals ~struct_map ~globals ~lang ~proc_map
    ~class_name_offset_map =
  { qualified_name
  ; sourcefile
  ; loc
  ; formals
  ; locals= VarMap.empty
  ; ids_move= IdentMap.empty
  ; ids_types= IdentMap.empty
  ; id_offset= None
  ; get_element_ptr_offset= None
  ; reg_map= RegMap.empty
  ; last_id= Textual.Ident.of_int 0
  ; last_tmp_var= 0
  ; struct_map
  ; globals
  ; lang
  ; proc_map
  ; class_name_offset_map }


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


let pp_formals fmt vars =
  F.fprintf fmt "%a"
    (Pp.comma_seq
       (Pp.pair ~fst:Textual.VarName.pp
          ~snd:(Pp.pair ~fst:Textual.Typ.pp_annotated ~snd:(Pp.option Textual.VarName.pp)) ) )
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
    pp_vars proc_state.locals pp_formals proc_state.formals pp_ids proc_state.ids_move pp_ids
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


(* debug_name = var1,
debug_name is originally a local and var1 is originally a formal. We are
removing this intruction and substituting var1 for debug_name in the code.
Result of this: var1 -> debug_name is added to the formals  such that we can
use the substitution in the code later on. *)
let subst_formal_local ~proc_state ~formal ~local =
  let formal_binding = VarMap.find_opt formal proc_state.formals in
  match formal_binding with
  | Some (formal_typ, _) ->
      let local, local_typ = local in
      let new_typ =
        match (local_typ.Textual.Typ.typ, formal_typ.Textual.Typ.typ) with
        | Textual.Typ.Ptr (Struct _), Int | Textual.Typ.Int, Textual.Typ.Ptr (Struct _) ->
            (* This is to avoid a type error when the signature type was int, because internally
           int is a pointer to a struct. Now, we use the local type for the formal in case of
           such a contradiction. *)
            local_typ
        | _ ->
            formal_typ
      in
      proc_state.formals <- VarMap.add formal (new_typ, Some local) proc_state.formals
  | _ ->
      ()


let compute_locals ~proc_state =
  let remove_locals_in_formals _ (_, local) locals =
    match local with Some local -> VarMap.remove local locals | None -> locals
  in
  let locals = VarMap.fold remove_locals_in_formals proc_state.formals proc_state.locals in
  VarMap.fold (fun varname typ locals -> (varname, typ) :: locals) locals []


let update_id_offset ~proc_state id exp =
  match (exp, proc_state.get_element_ptr_offset) with
  | Textual.Exp.Lvar varname, Some (var, offset) when Textual.VarName.equal var varname ->
      proc_state.id_offset <- Some (id, offset)
  | _ ->
      ()


let update_var_offset ~proc_state varname offset =
  if String.is_prefix ~prefix:get_element_ptr_offset_prefix (Textual.VarName.to_string varname) then
    proc_state.get_element_ptr_offset <- Some (varname, offset)


let update_ids_types ~proc_state id typ =
  proc_state.ids_types <- IdentMap.add id typ proc_state.ids_types


let reset_offsets ~proc_state =
  proc_state.id_offset <- None ;
  proc_state.get_element_ptr_offset <- None


let global_proc_state lang sourcefile loc global_var =
  let global_init_name = Format.sprintf "global_init_%s" global_var in
  let qualified_name =
    Textual.QualifiedProcName.
      {enclosing_class= TopLevel; name= Textual.ProcName.of_string global_init_name}
  in
  { qualified_name
  ; sourcefile
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
  ; proc_map= Textual.QualifiedProcName.Map.empty
  ; class_name_offset_map= ClassNameOffsetMap.create 16 }


let find_method_with_offset ~proc_state struct_name offset =
  let key = ClassNameOffset.{class_name= struct_name; offset} in
  ClassNameOffsetMap.find_opt proc_state.class_name_offset_map key
