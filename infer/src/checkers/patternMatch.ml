(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for Pattern matching. *)

module L = Logging
module F = Format
open Utils

let object_name = Mangled.from_string "java.lang.Object"

let type_is_object = function
  | Sil.Tptr (Sil.Tstruct (_, _, _, Some name, _, _, _), _) -> Mangled.equal name object_name
  | _ -> false

let java_proc_name_with_class_method pn class_with_path method_name =
  (try
     Procname.java_get_class pn = class_with_path &&
     Procname.java_get_method pn = method_name
   with _ -> false)

let is_direct_subtype_of this_type super_type_name =
  match this_type with
  | Sil.Tptr (Sil.Tstruct (_, _, _, _, supertypes, _, _), _) ->
      list_exists (fun (x, y) -> super_type_name = Mangled.to_string y) supertypes
  | _ -> false

(** The type the method is invoked on *)
let get_this_type proc_desc = match Cfg.Procdesc.get_formals proc_desc with
  | (n, t):: args -> Some t
  | _ -> None

let type_get_direct_supertypes = function
  | Sil.Tptr (Sil.Tstruct (_, _, _, _, supertypes, _, _), _)
  | Sil.Tstruct (_, _, _, _, supertypes, _, _) ->
      list_map (fun (_, m) -> m) supertypes
  | _ -> []

let type_get_class_name t = match t with
  | Sil.Tptr (Sil.Tstruct (_, _, _, Some cn, _, _, _), _) ->
      Some cn
  | Sil.Tptr (Sil.Tvar (Sil.TN_csu (Sil.Class, cn)), _) ->
      Some cn
  | _ -> None

let type_get_annotation
    (t: Sil.typ): Sil.item_annotation option =
  match t with
  | Sil.Tptr (Sil.Tstruct (_, _, _, _, _, _, ia), _)
  | Sil.Tstruct (_, _, _, _, _, _, ia) -> Some ia
  | _ -> None

let type_has_class_name t name =
  type_get_class_name t = Some name

let type_has_direct_supertype (t : Sil.typ) (s : Mangled.t) =
  list_exists (fun c -> c = s) (type_get_direct_supertypes t)

let type_find_supertype
    (tenv: Sil.tenv)
    (typ: Sil.typ)
    (csu_option: Sil.csu option)
    (filter: Mangled.t -> bool): bool =
  let rec has_supertype typ visited =
    if Sil.TypSet.mem typ visited then
      false
    else
      begin
        match Sil.expand_type tenv typ with
        | Sil.Tptr (Sil.Tstruct (_, _, _, _, supertypes, _, _), _)
        | Sil.Tstruct (_, _, _, _, supertypes, _, _) ->
            let match_supertype (csu, m) =
              let match_name () = filter m in
              let match_csu () = match csu_option with
                | Some c -> c = csu
                | None -> true in
              let has_indirect_supertype () =
                match Sil.get_typ m csu_option tenv with
                | Some supertype -> has_supertype supertype (Sil.TypSet.add typ visited)
                | None -> false in
              (match_csu () && match_name () (* only and always visit name with expected csu *))
              || has_indirect_supertype () in
            list_exists match_supertype supertypes
        | _ -> false
      end in
  has_supertype typ Sil.TypSet.empty

let type_has_supertype
    (tenv: Sil.tenv)
    (typ: Sil.typ)
    (csu_option: Sil.csu option)
    (name: Mangled.t): bool =
  let filter m = Mangled.equal m name in
  type_find_supertype tenv typ csu_option filter

let type_get_supertypes
    (tenv: Sil.tenv)
    (typ: Sil.typ)
    (csu_option: Sil.csu option) : Mangled.t list =
  let res = ref [] in
  let filter m =
    res := m :: !res;
    false in
  let _ = type_find_supertype tenv typ csu_option filter in
  list_rev !res

let type_is_nested_in_type t n = match t with
  | Sil.Tptr (Sil.Tstruct (_, _, _, Some m, _, _, _), _) ->
      string_is_prefix (Mangled.to_string n ^ "$") (Mangled.to_string m)
  | _ -> false

let type_is_nested_in_direct_supertype t n =
  let is_nested_in m2 m1 = string_is_prefix (Mangled.to_string m2 ^ "$") (Mangled.to_string m1) in
  list_exists (is_nested_in n) (type_get_direct_supertypes t)

let type_is_nested_in_supertype tenv t csu_option n =
  let is_nested_in m2 m1 = string_is_prefix (Mangled.to_string m2 ^ "$") (Mangled.to_string m1) in
  list_exists (is_nested_in n) (type_get_supertypes tenv t csu_option)

let rec get_type_name = function
  | Sil.Tstruct (_, _, _, Some mangled, _, _, _) -> Mangled.to_string mangled
  | Sil.Tptr (t, _) -> get_type_name t
  | Sil.Tvar tn -> Sil.typename_name tn
  | _ -> "_"

let get_field_type_name
    (typ: Sil.typ)
    (fieldname: Ident.fieldname): string option =
  match typ with
  | Sil.Tstruct (fields, _, _, _, _, _, _)
  | Sil.Tptr (Sil.Tstruct (fields, _, _, _, _, _, _), _) -> (
      try
        let _, ft, _ = list_find
            (function | (fn, _, _) -> Ident.fieldname_equal fn fieldname)
            fields in
        Some (get_type_name ft)
      with Not_found -> None)
  | _ -> None

let java_get_const_type_name
    (const: Sil.const): string =
  match const with
  | Sil.Cstr _ -> "java.lang.String"
  | Sil.Cint _ -> "java.lang.Integer"
  | Sil.Cfloat _ -> "java.lang.Double"
  | _ -> "_"

let get_vararg_type_names
    (call_node: Cfg.Node.t)
    (ivar: Sil.pvar): string list =
  (* Is this the node creating ivar? *)
  let rec initializes_array instrs =
    match instrs with
    | Sil.Call ([t1], Sil.Const (Sil.Cfun pn), _, _, _)::
      Sil.Set (Sil.Lvar iv, _, Sil.Var t2, _):: is ->
        (Sil.pvar_equal ivar iv && Ident.equal t1 t2 &&
         Procname.equal pn (Procname.from_string_c_fun "__new_array"))
        || initializes_array is
    | i:: is -> initializes_array is
    | _ -> false in

  (* Get the type name added to ivar or None *)
  let added_type_name node =
    let rec nvar_type_name nvar instrs =
      match instrs with
      | Sil.Letderef (nv, Sil.Lfield (_, id, t), _, _):: _
        when Ident.equal nv nvar -> get_field_type_name t id
      | Sil.Letderef (nv, e, t, _):: _
        when Ident.equal nv nvar ->
          Some (get_type_name t)
      | i:: is -> nvar_type_name nvar is
      | _ -> None in
    let rec added_nvar array_nvar instrs =
      match instrs with
      | Sil.Set (Sil.Lindex (Sil.Var iv, _), _, Sil.Var nvar, _):: _
        when Ident.equal iv array_nvar -> nvar_type_name nvar (Cfg.Node.get_instrs node)
      | Sil.Set (Sil.Lindex (Sil.Var iv, _), _, Sil.Const c, _):: _
        when Ident.equal iv array_nvar -> Some (java_get_const_type_name c)
      | i:: is -> added_nvar array_nvar is
      | _ -> None in
    let rec array_nvar instrs =
      match instrs with
      | Sil.Letderef (nv, Sil.Lvar iv, _, _):: _
        when Sil.pvar_equal iv ivar ->
          added_nvar nv instrs
      | i:: is -> array_nvar is
      | _ -> None in
    array_nvar (Cfg.Node.get_instrs node) in

  (* Walk nodes backward until definition of ivar, adding type names *)
  let rec type_names node =
    if initializes_array (Cfg.Node.get_instrs node) then
      []
    else
      match (Cfg.Node.get_preds node) with
      | [n] -> (match (added_type_name node) with
          | Some name -> name:: type_names n
          | None -> type_names n)
      | _ -> raise Not_found in

  list_rev (type_names call_node)

let has_type_name typ type_name =
  get_type_name typ = type_name

let has_formal_proc_argument_type_names proc_desc proc_name argument_type_names =
  let formals = Cfg.Procdesc.get_formals proc_desc in
  let equal_formal_arg (_, typ) arg_type_name = get_type_name typ = arg_type_name in
  list_length formals = list_length argument_type_names
  && list_for_all2 equal_formal_arg formals argument_type_names

let has_formal_method_argument_type_names cfg proc_name argument_type_names =
  has_formal_proc_argument_type_names
    cfg proc_name ((Procname.java_get_class proc_name):: argument_type_names)

let is_getter proc_name =
  Str.string_match (Str.regexp "get*") (Procname.java_get_method proc_name) 0

let is_setter proc_name =
  Str.string_match (Str.regexp "set*") (Procname.java_get_method proc_name) 0

(** Returns the signature of a field access (class name, field name, field type name) *)
let get_java_field_access_signature = function
  | Sil.Letderef (id, Sil.Lfield (e, fn, ft), bt, loc) ->
      Some (get_type_name bt, Ident.java_fieldname_get_field fn, get_type_name ft)
  | _ -> None

(** Returns the formal signature (class name, method name,
    argument type names and return type name) *)
let get_java_method_call_formal_signature = function
  | Sil.Call (ret_ids, Sil.Const (Sil.Cfun pn), (te, tt):: args, loc, cf) ->
      (try
         let arg_names = list_map (function | e, t -> get_type_name t) args in
         let rt_name = Procname.java_get_return_type pn in
         let m_name = Procname.java_get_method pn in
         Some (get_type_name tt, m_name, arg_names, rt_name)
       with _ -> None)
  | _ -> None


let type_is_class = function
  | Sil.Tptr (Sil.Tstruct _, _) -> true
  | Sil.Tptr (Sil.Tvar _, _) -> true
  | Sil.Tptr (Sil.Tarray _, _) -> true
  | Sil.Tstruct _ -> true
  | _ -> false

let initializer_classes = list_map Mangled.from_string [
    "android.app.Activity";
    "android.app.Application";
    "android.app.Fragment";
    "android.support.v4.app.Fragment";
  ]

let initializer_methods = [
  "onActivityCreated";
  "onAttach";
  "onCreate";
  "onCreateView";
]

(** Check if the type has in its supertypes from the initializer_classes list. *)
let type_has_initializer
    (tenv: Sil.tenv)
    (t: Sil.typ): bool =
  let check_candidate cname = type_has_supertype tenv t (Some Sil.Class) cname in
  list_exists check_candidate initializer_classes

(** Check if the method is one of the known initializer methods. *)
let method_is_initializer
    (tenv: Sil.tenv)
    (proc_name: Procname.t)
    (proc_desc: Cfg.Procdesc.t) : bool =
  match get_this_type proc_desc with
  | Some this_type ->
      if type_has_initializer tenv this_type then
        let mname = Procname.java_get_method proc_name in
        list_exists (string_equal mname) initializer_methods
      else
        false
  | None -> false

(** Get the vararg values by looking for array assignments to the pvar. *)
let java_get_vararg_values node pvar idenv pdesc =
  let values = ref [] in
  let do_instr = function
    | Sil.Set (Sil.Lindex (array_exp, _), _, content_exp, _)
      when Sil.exp_equal (Sil.Lvar pvar) (Idenv.expand_expr idenv array_exp) ->
        (* Each vararg argument is an assigment to a pvar denoting an array of objects. *)
        values := content_exp :: !values
    | _ -> () in
  let do_node n =
    list_iter do_instr (Cfg.Node.get_instrs n) in
  let () = match Errdesc.find_program_variable_assignment node pvar with
    | Some (node', _) ->
        Cfg.Procdesc.iter_slope_range do_node pdesc node' node
    | None -> () in
  !values

let proc_calls get_proc_desc pname pdesc filter : (Procname.t * Cfg.Procdesc.t) list =
  let res = ref [] in
  let do_instruction node instr = match instr with
    | Sil.Call (_, Sil.Const (Sil.Cfun callee_pn), _, _, _) ->
        begin
          match get_proc_desc callee_pn with
          | Some callee_pd ->
              if filter callee_pn callee_pd then res := (callee_pn, callee_pd) :: !res
          | None -> ()
        end
    | _ -> () in
  let do_node node =
    let instrs = Cfg.Node.get_instrs node in
    list_iter (do_instruction node) instrs in
  let nodes = Cfg.Procdesc.get_nodes pdesc in
  list_iter do_node nodes;
  list_rev !res
