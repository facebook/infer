(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack
open Sawja_pack


(*  line numbers where the code for the initialization of the static final fields starts  *)
let field_final_pcs : int list ref = ref []

(*  line numbers where the code for the initialization of the static nonfinal fields starts  *)
let field_nonfinal_pcs : int list ref = ref []

let reset_pcs () =
  field_final_pcs := [];
  field_nonfinal_pcs := []

let sort_pcs () =
  field_final_pcs := (IList.sort Pervasives.compare !field_final_pcs);
  field_nonfinal_pcs := (IList.sort Pervasives.compare !field_nonfinal_pcs)

(** Returns whether the node contains static final fields
    that are not of a primitive type or String. *)
let has_static_final_fields node =
  let detect _ f test =
    test || (Javalib.is_static_field f && Javalib.is_final_field f) in
  JBasics.FieldMap.fold detect (Javalib.get_fields node) false
(* Seems that there is no function "exists" on this implementation of *)
(* Patricia trees *)

(** collects the code line where the fields are initialised. The list is
    reversed in order to access the previous element in the list easier (as the successor.) *)
let collect_field_pc instrs field_pc_list =
  let aux pc instr =
    match instr with
    | JBir.AffectStaticField (_, fs, _) ->
        field_pc_list := (fs, pc)::!field_pc_list
    | _ -> () in
  (Array.iteri aux instrs);
  (IList.rev !field_pc_list)

(** Changes every position in the code where a static field is set to a value,
    to returning that value *)
let add_return_field instrs =
  let aux instr =
    match instr with
    | JBir.AffectStaticField (_, _, e) ->
        JBir.Return (Some e)
    | _ -> instr in
  (Array.map aux instrs)

(** Given a list with the lines where the fields are initialised,
    finds the line where the code for the initialisation of the given field starts,
    which is the line after the previous field has been initialised. *)
let rec find_pc field list =
  match list with
  | (fs, _):: rest ->
      if JBasics.fs_equal field fs then
        try
          let (_, npc) = IList.hd rest in
          npc + 1
        with _ -> 1
      else (find_pc field rest)
  | [] -> -1

(* Removes the lines of code for initializing nonfinal static fields. *)
let remove_nonfinal_instrs code end_pc =
  try
    sort_pcs ();
    let rec aux2 pc =
      let next_pc = pc + 1 in
      if not (IList.mem (=) pc !field_final_pcs) && not (IList.mem (=) pc !field_nonfinal_pcs) then
        begin
          Array.set code pc JBir.Nop;
          if next_pc < end_pc then aux2 next_pc
        end
      else () in
    let aux pc _ =
      if IList.mem (=) pc !field_nonfinal_pcs then
        begin
          Array.set code pc JBir.Nop;
          aux2 (pc +1)
        end
      else () in
    Array.iteri aux code
  with Invalid_argument _ -> assert false

let has_unclear_control_flow code =
  let aux instr nok =
    match instr with
    | JBir.Goto _ -> true
    | _ -> nok in
  Array.fold_right aux code false


(** In the initialiser of static fields, we add instructions
    for returning the field selected by the parameter. *)
(* The constant s means the parameter field of the function.
   Note that we remove the initialisation of non - final static fields. *)
let static_field_init_complex code fields length =
  let code = Array.append [| (JBir.Goto length ) |] code in
  let s = JConfig.field_cst in
  let field_pc_list = ref [] in
  let _ = collect_field_pc code field_pc_list in
  let code = add_return_field code in
  let rec aux s fields =
    match fields with
    | (fs, field):: rest ->
        let pc = find_pc fs !field_pc_list in
        if Javalib.is_static_field field && Javalib.is_final_field field && pc <> -1 then
          let _ = field_final_pcs := pc::!field_final_pcs in
          let fs_const = JBir.Const (`String (JBasics.make_jstr (JBasics.fs_name fs))) in
          let arg_const = JBir.Const (`String (JBasics.make_jstr s)) in
          let arr = [| JBir.Ifd ((`Eq, fs_const, arg_const), pc); JBir.Nop |] in
          let rest_instrs = (aux s rest) in
          Array.append arr rest_instrs
        else
          let _ =
            if Javalib.is_static_field field && pc <> -1 then
              field_nonfinal_pcs := pc::!field_nonfinal_pcs in
          aux s rest
    | [] -> [| JBir.Nop |] in
  let new_instrs = aux s fields in
  let code = Array.append code new_instrs in
  remove_nonfinal_instrs code length;
  reset_pcs ();
  code

(** In the initialiser of static fields, we add instructions
    for returning the field selected by the parameter without changing
    the control flow of the original code. *)
let static_field_init_simple cn code fields length =
  let s = JConfig.field_cst in
  let rec aux s pc fields =
    match fields with
    | (fs, field):: rest ->
        if Javalib.is_static_field field && Javalib.is_final_field field then
          let npc = pc + 2 in
          let fs_const = JBir.Const (`String (JBasics.make_jstr (JBasics.fs_name fs))) in
          let arg_const = JBir.Const (`String (JBasics.make_jstr s)) in
          let arr = [| JBir.Ifd ((`Ne, fs_const, arg_const), npc); JBir.Return (Some (JBir.StaticField (cn, fs))) |] in
          let rest_instrs = (aux s npc rest) in
          Array.append arr rest_instrs
        else (aux s pc rest)
    | [] -> [| JBir.Nop |] in
  let new_instrs = aux s length fields in
  let code = Array.append code new_instrs in
  code

(** In the initialiser of static fields, we add instructions
    for returning the field selected by the parameter. In normal
    cases the code for the initialisation of each field is clearly separated
    from the code for the initialisation of the next field. However, in some cases
    the fields are initialised in static blocks in which they may use try and catch.
    In these cases it is not possible to separate the code for the initialisation
    of each field, so we do not change the original code, but append intructions
    for returning the selected field. *)
let static_field_init node cn code =
  try
    let field_list = JBasics.FieldMap.elements (Javalib.get_fields node) in
    (* TODO: this translation to a list can be removed and map iterators can be used afterward *)
    let length = Array.length code in
    Array.set code (length -1) JBir.Nop;
    (* TODO: make sure this modification of the array has no side effect *)
    let code =
      if has_unclear_control_flow code then
        static_field_init_simple cn code field_list length
      else static_field_init_complex code field_list length in
    code
  with Not_found -> code

(* when accessing a static final field, we call the initialiser method. *)
let translate_instr_static_field context callee_procdesc fs field_type loc =
  let cg = JContext.get_cg context in
  let caller_procdesc = JContext.get_procdesc context in
  let ret_id = Ident.create_fresh Ident.knormal in
  let caller_procname = (Cfg.Procdesc.get_proc_name caller_procdesc) in
  let callee_procname = Cfg.Procdesc.get_proc_name callee_procdesc in
  let callee_fun = Sil.Const (Sil.Cfun callee_procname) in
  let field_arg = Sil.Const (Sil.Cstr (JBasics.fs_name fs)) in
  let call_instr = Sil.Call([ret_id], callee_fun, [field_arg, field_type], loc, Sil.cf_default) in
  Cg.add_edge cg caller_procname callee_procname;
  ([ret_id], [call_instr], Sil.Var ret_id)


let is_static_final_field context cn fs =
  match JClasspath.lookup_node cn (JContext.get_program context) with
  | None -> false
  | Some node ->
      try
        let f = Javalib.get_field node fs in
        let is_static = Javalib.is_static_field f in
        let is_final = Javalib.is_final_field f in
        (is_static && is_final)
      with Not_found -> false

(*
let is_basic_type fs =
  let vt = (JBasics.fs_type fs) in
  match vt with
  | JBasics.TBasic bt -> true
  | JBasics.TObject ot -> false
*)
