(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for Pattern matching. *)

module L = Logging
module F = Format

type taint_spec = {
  classname : string;
  method_name : string;
  ret_type : string;
  params : string list;
  is_static : bool;
  taint_kind : PredSymb.taint_kind;
  language : Config.language
}

let object_name = Mangled.from_string "java.lang.Object"

let type_is_object = function
  | Typ.Tptr (Typ.Tstruct { Typ.struct_name = Some name }, _) ->
      Mangled.equal name object_name
  | _ -> false

let java_proc_name_with_class_method pn_java class_with_path method_name =
  (try
     Procname.java_get_class_name pn_java = class_with_path &&
     Procname.java_get_method pn_java = method_name
   with _ -> false)

let get_direct_supers tenv = function
  | { Typ.csu = Csu.Class _; superclasses } ->
      IList.map (Tenv.lookup tenv) superclasses
      |> IList.flatten_options
  | _ ->
      []

(** get the superclasses of [typ]. does not include [typ] itself *)
let strict_supertype_iter tenv f_typ orig_struct_typ =
  let rec get_supers_rec struct_typ =
    let direct_supers = get_direct_supers tenv struct_typ in
    IList.iter f_typ direct_supers;
    IList.iter get_supers_rec direct_supers in
  get_supers_rec orig_struct_typ

(** Return [true] if [f_typ] evaluates to true on a strict supertype of [orig_struct_typ] *)
let strict_supertype_exists tenv f_typ orig_struct_typ =
  let rec get_supers_rec struct_typ =
    let direct_supers = get_direct_supers tenv struct_typ in
    IList.exists f_typ direct_supers ||
    IList.exists get_supers_rec direct_supers in
  get_supers_rec orig_struct_typ

let is_immediate_subtype this_type super_type_name =
  IList.exists (Typename.equal super_type_name) this_type.Typ.superclasses

(** return true if [typ0] <: [typ1] *)
let is_subtype tenv struct_typ0 struct_typ1 =
  Typ.struct_typ_equal struct_typ0 struct_typ1 ||
  strict_supertype_exists tenv (Typ.struct_typ_equal struct_typ1) struct_typ0

let is_subtype_of_str tenv cn1 classname_str =
  let typename = Typename.Java.from_string classname_str in
  let lookup = Tenv.lookup tenv in
  match lookup cn1, lookup typename with
  | Some struct_typ1, Some struct_typ2 -> is_subtype tenv struct_typ1 struct_typ2
  | _ -> false

(** The type the method is invoked on *)
let get_this_type proc_attributes = match proc_attributes.ProcAttributes.formals with
  | (_, t):: _ -> Some t
  | _ -> None

let type_get_direct_supertypes = function
  | Typ.Tptr (Tstruct { superclasses }, _)
  | Typ.Tstruct { superclasses } ->
      superclasses
  | _ ->
      []

let type_get_class_name t = match t with
  | Typ.Tptr (Typ.Tstruct { Typ.struct_name = Some cn }, _) ->
      Some cn
  | Typ.Tptr (Typ.Tvar (Typename.TN_csu (Csu.Class _, cn)), _) ->
      Some cn
  | _ -> None

let type_get_annotation
    (t: Typ.t): Typ.item_annotation option =
  match t with
  | Typ.Tptr (Typ.Tstruct { Typ.struct_annotations }, _)
  | Typ.Tstruct { Typ.struct_annotations } ->
      Some struct_annotations
  | _ -> None

let type_has_class_name t name =
  type_get_class_name t = Some name

let type_has_direct_supertype (typ : Typ.t) (class_name : Typename.t) =
  IList.exists (fun cn -> Typename.equal cn class_name) (type_get_direct_supertypes typ)

let type_has_supertype
    (tenv: Tenv.t)
    (typ: Typ.t)
    (class_name: Typename.t): bool =
  let rec has_supertype typ visited =
    if Typ.Set.mem typ visited then
      false
    else
      begin
        match Tenv.expand_type tenv typ with
        | Typ.Tptr (Typ.Tstruct { Typ.superclasses }, _)
        | Typ.Tstruct { Typ.superclasses } ->
            let match_supertype cn =
              let match_name () = Typename.equal cn class_name in
              let has_indirect_supertype () =
                match Tenv.lookup tenv cn with
                | Some supertype ->
                    has_supertype (Typ.Tstruct supertype) (Typ.Set.add typ visited)
                | None -> false in
              (match_name () || has_indirect_supertype ()) in
            IList.exists match_supertype superclasses
        | _ -> false
      end in
  has_supertype typ Typ.Set.empty


let type_is_nested_in_type t n = match t with
  | Typ.Tptr (Typ.Tstruct { Typ.struct_name = Some name }, _) ->
      string_is_prefix (Mangled.to_string n ^ "$") (Mangled.to_string name)
  | _ -> false

let type_is_nested_in_direct_supertype t n =
  let is_nested_in cn1 cn2 = string_is_prefix (Typename.name cn1 ^ "$") (Typename.name cn2) in
  IList.exists (is_nested_in n) (type_get_direct_supertypes t)

let rec get_type_name = function
  | Typ.Tstruct { Typ.struct_name = Some name } ->
      Mangled.to_string name
  | Typ.Tptr (t, _) -> get_type_name t
  | Typ.Tvar tn -> Typename.name tn
  | _ -> "_"

let get_field_type_name
    (typ: Typ.t)
    (fieldname: Ident.fieldname): string option =
  match typ with
  | Typ.Tstruct { Typ.instance_fields }
  | Typ.Tptr (Typ.Tstruct { Typ.instance_fields }, _) -> (
      try
        let _, ft, _ = IList.find
            (function | (fn, _, _) -> Ident.fieldname_equal fn fieldname)
            instance_fields in
        Some (get_type_name ft)
      with Not_found -> None)
  | _ -> None

let java_get_const_type_name
    (const: Const.t): string =
  match const with
  | Const.Cstr _ -> "java.lang.String"
  | Const.Cint _ -> "java.lang.Integer"
  | Const.Cfloat _ -> "java.lang.Double"
  | _ -> "_"

let get_vararg_type_names
    (call_node: Cfg.Node.t)
    (ivar: Pvar.t): string list =
  (* Is this the node creating ivar? *)
  let rec initializes_array instrs =
    match instrs with
    | Sil.Call ([t1], Exp.Const (Const.Cfun pn), _, _, _)::
      Sil.Set (Exp.Lvar iv, _, Exp.Var t2, _):: is ->
        (Pvar.equal ivar iv && Ident.equal t1 t2 &&
         Procname.equal pn (Procname.from_string_c_fun "__new_array"))
        || initializes_array is
    | _:: is -> initializes_array is
    | _ -> false in

  (* Get the type name added to ivar or None *)
  let added_type_name node =
    let rec nvar_type_name nvar instrs =
      match instrs with
      | Sil.Letderef (nv, Exp.Lfield (_, id, t), _, _):: _
        when Ident.equal nv nvar -> get_field_type_name t id
      | Sil.Letderef (nv, _, t, _):: _
        when Ident.equal nv nvar ->
          Some (get_type_name t)
      | _:: is -> nvar_type_name nvar is
      | _ -> None in
    let rec added_nvar array_nvar instrs =
      match instrs with
      | Sil.Set (Exp.Lindex (Exp.Var iv, _), _, Exp.Var nvar, _):: _
        when Ident.equal iv array_nvar -> nvar_type_name nvar (Cfg.Node.get_instrs node)
      | Sil.Set (Exp.Lindex (Exp.Var iv, _), _, Exp.Const c, _):: _
        when Ident.equal iv array_nvar -> Some (java_get_const_type_name c)
      | _:: is -> added_nvar array_nvar is
      | _ -> None in
    let rec array_nvar instrs =
      match instrs with
      | Sil.Letderef (nv, Exp.Lvar iv, _, _):: _
        when Pvar.equal iv ivar ->
          added_nvar nv instrs
      | _:: is -> array_nvar is
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

  IList.rev (type_names call_node)

let has_formal_proc_argument_type_names proc_desc argument_type_names =
  let formals = Cfg.Procdesc.get_formals proc_desc in
  let equal_formal_arg (_, typ) arg_type_name = get_type_name typ = arg_type_name in
  IList.length formals = IList.length argument_type_names
  && IList.for_all2 equal_formal_arg formals argument_type_names

let has_formal_method_argument_type_names cfg pname_java argument_type_names =
  has_formal_proc_argument_type_names
    cfg ((Procname.java_get_class_name pname_java):: argument_type_names)

let is_getter pname_java =
  Str.string_match (Str.regexp "get*") (Procname.java_get_method pname_java) 0

let is_setter pname_java =
  Str.string_match (Str.regexp "set*") (Procname.java_get_method pname_java) 0

(** Returns the signature of a field access (class name, field name, field type name) *)
let get_java_field_access_signature = function
  | Sil.Letderef (_, Exp.Lfield (_, fn, ft), bt, _) ->
      Some (get_type_name bt, Ident.java_fieldname_get_field fn, get_type_name ft)
  | _ -> None

(** Returns the formal signature (class name, method name,
    argument type names and return type name) *)
let get_java_method_call_formal_signature = function
  | Sil.Call (_, Exp.Const (Const.Cfun pn), (_, tt):: args, _, _) ->
      (match pn with
       | Procname.Java pn_java ->
           let arg_names = IList.map (function | _, t -> get_type_name t) args in
           let rt_name = Procname.java_get_return_type pn_java in
           let m_name = Procname.java_get_method pn_java in
           Some (get_type_name tt, m_name, arg_names, rt_name)
       |  _ ->
           None)
  | _ -> None


let type_is_class = function
  | Typ.Tptr (Typ.Tstruct _, _) -> true
  | Typ.Tptr (Typ.Tvar _, _) -> true
  | Typ.Tptr (Typ.Tarray _, _) -> true
  | Typ.Tstruct _ -> true
  | _ -> false

let initializer_classes =
  IList.map
    (fun name -> Typename.TN_csu (Csu.Class Csu.Java, Mangled.from_string name))
    [
      "android.app.Activity";
      "android.app.Application";
      "android.app.Fragment";
      "android.app.Service";
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
    (tenv: Tenv.t)
    (t: Typ.t): bool =
  let check_candidate class_name = type_has_supertype tenv t class_name in
  IList.exists check_candidate initializer_classes

(** Check if the method is one of the known initializer methods. *)
let method_is_initializer
    (tenv: Tenv.t)
    (proc_attributes: ProcAttributes.t) : bool =
  match get_this_type proc_attributes with
  | Some this_type ->
      if type_has_initializer tenv this_type then
        match proc_attributes.ProcAttributes.proc_name with
        | Procname.Java pname_java ->
            let mname = Procname.java_get_method pname_java in
            IList.exists (string_equal mname) initializer_methods
        | _ ->
            false
      else
        false
  | None -> false

(** Get the vararg values by looking for array assignments to the pvar. *)
let java_get_vararg_values node pvar idenv =
  let values = ref [] in
  let do_instr = function
    | Sil.Set (Exp.Lindex (array_exp, _), _, content_exp, _)
      when Exp.equal (Exp.Lvar pvar) (Idenv.expand_expr idenv array_exp) ->
        (* Each vararg argument is an assigment to a pvar denoting an array of objects. *)
        values := content_exp :: !values
    | _ -> () in
  let do_node n =
    IList.iter do_instr (Cfg.Node.get_instrs n) in
  let () = match Errdesc.find_program_variable_assignment node pvar with
    | Some (node', _) ->
        Cfg.Procdesc.iter_slope_range do_node node' node
    | None -> () in
  !values

let proc_calls resolve_attributes pdesc filter : (Procname.t * ProcAttributes.t) list =
  let res = ref [] in
  let do_instruction _ instr = match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, _, _) ->
        begin
          match resolve_attributes callee_pn with
          | Some callee_attributes ->
              if filter callee_pn callee_attributes then
                res := (callee_pn, callee_attributes) :: !res
          | None -> ()
        end
    | _ -> () in
  let do_node node =
    let instrs = Cfg.Node.get_instrs node in
    IList.iter (do_instruction node) instrs in
  let nodes = Cfg.Procdesc.get_nodes pdesc in
  IList.iter do_node nodes;
  IList.rev !res


(** Iterate over all the methods overridden by the procedure.
    Only Java supported at the moment. *)
let proc_iter_overridden_methods f tenv proc_name =
  let do_super_type tenv super_class_name =
    let super_proc_name =
      Procname.replace_class proc_name (Typename.name super_class_name) in
    match Tenv.lookup tenv super_class_name with
    | Some ({ Typ.def_methods }) ->
        let is_override pname =
          Procname.equal pname super_proc_name &&
          not (Procname.is_constructor pname) in
        IList.iter
          (fun pname ->
             if is_override pname
             then f pname)
          def_methods
    | _ -> () in

  match proc_name with
  | Procname.Java proc_name_java ->
      let type_name =
        let class_name = Procname.java_get_class_name proc_name_java in
        Typename.TN_csu (Csu.Class Csu.Java, Mangled.from_string class_name) in
      (match Tenv.lookup tenv type_name with
       | Some curr_struct_typ ->
           IList.iter
             (do_super_type tenv)
             (type_get_direct_supertypes (Typ.Tstruct curr_struct_typ))
       | None ->
           ())
  | _ ->
      () (* Only java supported at the moment *)

(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
let get_fields_nullified procdesc =
  (* walk through the instructions and look for instance fields that are assigned to null *)
  let collect_nullified_flds (nullified_flds, this_ids) _ = function
    | Sil.Set (Exp.Lfield (Exp.Var lhs, fld, _), _, rhs, _)
      when Exp.is_null_literal rhs && Ident.IdentSet.mem lhs this_ids ->
        (Ident.FieldSet.add fld nullified_flds, this_ids)
    | Sil.Letderef (id, rhs, _, _) when Exp.is_this rhs ->
        (nullified_flds, Ident.IdentSet.add id this_ids)
    | _ -> (nullified_flds, this_ids) in
  let (nullified_flds, _) =
    Cfg.Procdesc.fold_instrs
      collect_nullified_flds (Ident.FieldSet.empty, Ident.IdentSet.empty) procdesc in
  nullified_flds

(** Checks if the exception is an unchecked exception *)
let is_runtime_exception tenv typename =
  is_subtype_of_str tenv typename "java.lang.RuntimeException"

(** Checks if the class name is a Java exception *)
let is_exception tenv typename =
  is_subtype_of_str tenv typename "java.lang.Exception"

(** Checks if the class name is a Java exception *)
let is_throwable tenv typename =
  is_subtype_of_str tenv typename "java.lang.Throwable"
