(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Pattern matching. *)

let type_is_object typ =
  match typ.Typ.desc with
  | Tptr ({desc= Tstruct name}, _) ->
      Typ.Name.equal name Typ.Name.Java.java_lang_object
  | _ ->
      false


(** Holds iff the predicate holds on a supertype of the named type, including the type itself *)
let rec supertype_exists tenv pred name =
  match Tenv.lookup tenv name with
  | Some ({supers} as struct_typ) ->
      pred name struct_typ || List.exists ~f:(fun name -> supertype_exists tenv pred name) supers
  | None ->
      false


let rec supertype_find_map_opt tenv f name =
  match f name with
  | None -> (
    match Tenv.lookup tenv name with
    | Some {supers} ->
        List.find_map ~f:(supertype_find_map_opt tenv f) supers
    | None ->
        None )
  | result ->
      result


(** return true if [typ0] <: [typ1] *)
let is_subtype tenv name0 name1 =
  Typ.Name.equal name0 name1
  || supertype_exists tenv (fun name _ -> Typ.Name.equal name name1) name0


let is_subtype_of_str tenv cn1 classname_str =
  let typename = Typ.Name.Java.from_string classname_str in
  is_subtype tenv cn1 typename


let implements interface tenv typename =
  let is_interface s _ = String.equal interface (Typ.Name.name s) in
  supertype_exists tenv is_interface (Typ.Name.Java.from_string typename)


let implements_iterator = implements "java.util.Iterator"

let implements_collection = implements "java.util.Collection"

let implements_list = implements "java.util.List"

let implements_pseudo_collection t s =
  implements "android.util.SparseArray" t s
  || implements "android.util.SparseIntArray" t s
  || implements "com.moblica.common.xmob.utils.IntArrayList" t s


let implements_enumeration = implements "java.util.Enumeration"

let implements_inject class_name = implements ("javax.inject." ^ class_name)

let implements_io class_name = implements ("java.io." ^ class_name)

let implements_map = implements "java.util.Map"

let implements_map_entry = implements "java.util.Map$Entry"

let implements_queue = implements "java.util.Queue"

let implements_lang class_name = implements ("java.lang." ^ class_name)

let implements_google class_name = implements ("com.google." ^ class_name)

let implements_android class_name = implements ("android." ^ class_name)

let implements_jackson class_name = implements ("com.fasterxml.jackson." ^ class_name)

(** The type the method is invoked on *)
let get_this_type proc_attributes =
  match proc_attributes.ProcAttributes.formals with (_, t) :: _ -> Some t | _ -> None


let type_get_direct_supertypes tenv (typ : Typ.t) =
  match typ.desc with
  | Tptr ({desc= Tstruct name}, _) | Tstruct name -> (
    match Tenv.lookup tenv name with Some {supers} -> supers | None -> [] )
  | _ ->
      []


let type_get_class_name {Typ.desc} =
  match desc with Typ.Tptr (typ, _) -> Typ.name typ | _ -> None


let type_get_annotation tenv (typ : Typ.t) : Annot.Item.t option =
  match typ.desc with
  | Tptr ({desc= Tstruct name}, _) | Tstruct name -> (
    match Tenv.lookup tenv name with Some {annots} -> Some annots | None -> None )
  | _ ->
      None


let rec get_type_name {Typ.desc} =
  match desc with
  | Typ.Tstruct name ->
      Typ.Name.name name
  | Typ.Tptr (t, _) ->
      get_type_name t
  | _ ->
      "_"


let get_field_type_name tenv (typ : Typ.t) (fieldname : Typ.Fieldname.t) : string option =
  match typ.desc with
  | Tstruct name | Tptr ({desc= Tstruct name}, _) -> (
    match Tenv.lookup tenv name with
    | Some {fields} -> (
      match List.find ~f:(function fn, _, _ -> Typ.Fieldname.equal fn fieldname) fields with
      | Some (_, ft, _) ->
          Some (get_type_name ft)
      | None ->
          None )
    | None ->
        None )
  | _ ->
      None


let java_get_const_type_name (const : Const.t) : string =
  match const with
  | Const.Cstr _ ->
      "java.lang.String"
  | Const.Cint _ ->
      "java.lang.Integer"
  | Const.Cfloat _ ->
      "java.lang.Double"
  | _ ->
      "_"


let get_vararg_type_names tenv (call_node : Procdesc.Node.t) (ivar : Pvar.t) : string list =
  (* Is this the node creating ivar? *)
  let initializes_array instrs =
    instrs
    |> Instrs.find_map ~f:(function
         | Sil.Store (Exp.Lvar iv, _, Exp.Var t2, _) when Pvar.equal ivar iv ->
             Some t2
         | _ ->
             None )
    |> Option.exists ~f:(fun t2 ->
           Instrs.exists instrs ~f:(function
             | Sil.Call ((t1, _), Exp.Const (Const.Cfun pn), _, _, _) ->
                 Ident.equal t1 t2
                 && Typ.Procname.equal pn (Typ.Procname.from_string_c_fun "__new_array")
             | _ ->
                 false ) )
  in
  (* Get the type name added to ivar or None *)
  let added_type_name instrs =
    let nvar_type_name nvar =
      instrs
      |> Instrs.find_map ~f:(function
           | Sil.Load (nv, e, t, _) when Ident.equal nv nvar ->
               Some (e, t)
           | _ ->
               None )
      |> Option.bind ~f:(function
           | Exp.Lfield (_, id, t), _ ->
               get_field_type_name tenv t id
           | _, t ->
               Some (get_type_name t) )
    in
    let added_nvar array_nvar =
      instrs
      |> Instrs.find_map ~f:(function
           | Sil.Store (Exp.Lindex (Exp.Var iv, _), _, Exp.Var nvar, _)
             when Ident.equal iv array_nvar ->
               Some (nvar_type_name nvar)
           | Sil.Store (Exp.Lindex (Exp.Var iv, _), _, Exp.Const c, _)
             when Ident.equal iv array_nvar ->
               Some (Some (java_get_const_type_name c))
           | _ ->
               None )
      |> Option.join
    in
    let array_nvar =
      instrs
      |> Instrs.find_map ~f:(function
           | Sil.Load (nv, Exp.Lvar iv, _, _) when Pvar.equal iv ivar ->
               Some nv
           | _ ->
               None )
      |> Option.bind ~f:added_nvar
    in
    array_nvar
  in
  (* Walk nodes backward until definition of ivar, adding type names *)
  let rec type_names acc node =
    let instrs = Procdesc.Node.get_instrs node in
    if initializes_array instrs then acc
    else
      match Procdesc.Node.get_preds node with
      | [n] -> (
        match added_type_name instrs with
        | Some name ->
            type_names (name :: acc) n
        | None ->
            type_names acc n )
      | _ ->
          raise Caml.Not_found
  in
  type_names [] call_node


let is_getter pname_java =
  Str.string_match (Str.regexp "get*") (Typ.Procname.Java.get_method pname_java) 0


let type_is_class typ =
  match typ.Typ.desc with
  | Tptr ({desc= Tstruct _}, _) ->
      true
  | Tptr ({desc= Tarray _}, _) ->
      true
  | Tstruct _ ->
      true
  | _ ->
      false


let initializer_classes =
  List.map ~f:Typ.Name.Java.from_string
    [ "android.app.Activity"
    ; "android.app.Application"
    ; "android.app.Fragment"
    ; "android.app.Service"
    ; "android.support.v4.app.Fragment"
    ; "junit.framework.TestCase" ]


let initializer_methods = ["onActivityCreated"; "onAttach"; "onCreate"; "onCreateView"; "setUp"]

(** Check if the type has in its supertypes from the initializer_classes list. *)
let type_has_initializer (tenv : Tenv.t) (t : Typ.t) : bool =
  let is_initializer_class typename _ =
    List.mem ~equal:Typ.Name.equal initializer_classes typename
  in
  match t.desc with
  | Typ.Tstruct name | Tptr ({desc= Tstruct name}, _) ->
      supertype_exists tenv is_initializer_class name
  | _ ->
      false


(** Check if the method is one of the known initializer methods. *)
let method_is_initializer (tenv : Tenv.t) (proc_attributes : ProcAttributes.t) : bool =
  match get_this_type proc_attributes with
  | Some this_type ->
      if type_has_initializer tenv this_type then
        match proc_attributes.ProcAttributes.proc_name with
        | Typ.Procname.Java pname_java ->
            let mname = Typ.Procname.Java.get_method pname_java in
            List.exists ~f:(String.equal mname) initializer_methods
        | _ ->
            false
      else false
  | None ->
      false


(** Get the vararg values by looking for array assignments to the pvar. *)
let java_get_vararg_values node pvar idenv =
  let values_of_instr acc = function
    | Sil.Store (Exp.Lindex (array_exp, _), _, content_exp, _)
      when Exp.equal (Exp.Lvar pvar) (Idenv.expand_expr idenv array_exp) ->
        (* Each vararg argument is an assignment to a pvar denoting an array of objects. *)
        content_exp :: acc
    | _ ->
        acc
  in
  let values_of_node acc n =
    Procdesc.Node.get_instrs n |> Instrs.fold ~f:values_of_instr ~init:acc
  in
  match Errdesc.find_program_variable_assignment node pvar with
  | Some (node', _) ->
      Procdesc.fold_slope_range node' node ~f:values_of_node ~init:[]
  | None ->
      []


let proc_calls resolve_attributes pdesc filter : (Typ.Procname.t * ProcAttributes.t) list =
  let res = ref [] in
  let do_instruction _ instr =
    match instr with
    | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, _, _) -> (
      match resolve_attributes callee_pn with
      | Some callee_attributes ->
          if filter callee_pn callee_attributes then res := (callee_pn, callee_attributes) :: !res
      | None ->
          () )
    | _ ->
        ()
  in
  let do_node node =
    let instrs = Procdesc.Node.get_instrs node in
    Instrs.iter ~f:(do_instruction node) instrs
  in
  let nodes = Procdesc.get_nodes pdesc in
  List.iter ~f:do_node nodes ; List.rev !res


let override_find ?(check_current_type = true) f tenv proc_name =
  let method_name = Typ.Procname.get_method proc_name in
  let parameter_length = List.length (Typ.Procname.get_parameters proc_name) in
  let is_override pname =
    (not (Typ.Procname.is_constructor pname))
    && String.equal (Typ.Procname.get_method pname) method_name
    (* TODO (T32979782): match parameter types, taking subtyping and type erasure into account *)
    && Int.equal (List.length (Typ.Procname.get_parameters pname)) parameter_length
  in
  let rec find_super_type super_class_name =
    Tenv.lookup tenv super_class_name
    |> Option.bind ~f:(fun {Typ.Struct.methods; supers} ->
           match List.find ~f:(fun pname -> is_override pname && f pname) methods with
           | None ->
               List.find_map ~f:find_super_type supers
           | pname_opt ->
               pname_opt )
  in
  let find_super_type type_name =
    List.find_map ~f:find_super_type (type_get_direct_supertypes tenv (Typ.mk (Tstruct type_name)))
  in
  if check_current_type && f proc_name then Some proc_name
  else
    match proc_name with
    | Typ.Procname.Java proc_name_java ->
        find_super_type
          (Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name proc_name_java))
    | Typ.Procname.ObjC_Cpp proc_name_cpp ->
        find_super_type (Typ.Procname.ObjC_Cpp.get_class_type_name proc_name_cpp)
    | _ ->
        None


let override_exists ?(check_current_type = true) f tenv proc_name =
  override_find ~check_current_type f tenv proc_name |> Option.is_some


(* Only java supported at the moment *)

let override_iter f tenv proc_name =
  ignore (override_exists (fun pname -> f pname ; false) tenv proc_name)


let lookup_attributes tenv proc_name =
  let found_attributes = ref None in
  let f pname =
    match Attributes.load pname with
    | None ->
        false
    | Some _ as attributes ->
        found_attributes := attributes ;
        true
  in
  ignore (override_find ~check_current_type:true f tenv proc_name) ;
  !found_attributes


(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
let get_fields_nullified procdesc =
  (* walk through the instructions and look for instance fields that are assigned to null *)
  let collect_nullified_flds (nullified_flds, this_ids) _ = function
    | Sil.Store (Exp.Lfield (Exp.Var lhs, fld, _), _, rhs, _)
      when Exp.is_null_literal rhs && Ident.Set.mem lhs this_ids ->
        (Typ.Fieldname.Set.add fld nullified_flds, this_ids)
    | Sil.Load (id, rhs, _, _) when Exp.is_this rhs ->
        (nullified_flds, Ident.Set.add id this_ids)
    | _ ->
        (nullified_flds, this_ids)
  in
  let nullified_flds, _ =
    Procdesc.fold_instrs procdesc ~f:collect_nullified_flds
      ~init:(Typ.Fieldname.Set.empty, Ident.Set.empty)
  in
  nullified_flds


(** Checks if the exception is an unchecked exception *)
let is_runtime_exception tenv typename =
  is_subtype_of_str tenv typename "java.lang.RuntimeException"


(** Checks if the class name is a Java exception *)
let is_throwable tenv typename = is_subtype_of_str tenv typename "java.lang.Throwable"

(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
    including for supertypes*)
let check_class_attributes check tenv = function
  | Typ.Procname.Java java_pname ->
      let check_class_annots _ {Typ.Struct.annots} = check annots in
      supertype_exists tenv check_class_annots (Typ.Procname.Java.get_class_type_name java_pname)
  | _ ->
      false


(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
    for the current class only*)
let check_current_class_attributes check tenv = function
  | Typ.Procname.Java java_pname -> (
    match Tenv.lookup tenv (Typ.Procname.Java.get_class_type_name java_pname) with
    | Some struct_typ ->
        check struct_typ.annots
    | _ ->
        false )
  | _ ->
      false


(** find superclasss with attributes (e.g., @ThreadSafe), including current class*)
let rec find_superclasses_with_attributes check tenv tname =
  match Tenv.lookup tenv tname with
  | Some struct_typ ->
      let result_from_supers =
        List.concat (List.map ~f:(find_superclasses_with_attributes check tenv) struct_typ.supers)
      in
      if check struct_typ.annots then tname :: result_from_supers else result_from_supers
  | _ ->
      []
