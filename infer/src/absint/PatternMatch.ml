(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Pattern matching. *)

(** Holds iff the predicate holds on a supertype of the named type, including the type itself *)
let rec supertype_exists tenv pred name =
  match Tenv.lookup tenv name with
  | Some ({supers} as struct_typ) ->
      pred name struct_typ || List.exists ~f:(fun name -> supertype_exists tenv pred name) supers
  | None ->
      false


(** Holds iff the predicate holds on a protocol of the named type *)
let protocol_exists tenv pred name =
  match Tenv.lookup tenv name with
  | Some {objc_protocols} ->
      List.exists ~f:(fun name -> pred name) objc_protocols
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


(** The type the method is invoked on *)
let get_this_type_nonstatic_methods_only proc_attributes =
  match proc_attributes.ProcAttributes.formals with (_, t) :: _ -> Some t | _ -> None


let type_get_direct_supertypes tenv (typ : Typ.t) =
  match typ.desc with
  | Tptr ({desc= Tstruct name}, _) | Tstruct name -> (
    match Tenv.lookup tenv name with Some {supers} -> supers | None -> [] )
  | _ ->
      []


let type_get_class_name {Typ.desc} = match desc with Typ.Tptr (typ, _) -> Typ.name typ | _ -> None

let type_name_get_annotation tenv (name : Typ.name) : Annot.Item.t option =
  match Tenv.lookup tenv name with Some {annots} -> Some annots | None -> None


let type_get_annotation tenv (typ : Typ.t) : Annot.Item.t option =
  match typ.desc with
  | Tptr ({desc= Tstruct name}, _) | Tstruct name ->
      type_name_get_annotation tenv name
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


let get_field_type_name tenv (typ : Typ.t) (fieldname : Fieldname.t) : string option =
  match typ.desc with
  | Tstruct name | Tptr ({desc= Tstruct name}, _) -> (
    match Tenv.lookup tenv name with
    | Some {fields} -> (
      match List.find ~f:(function fn, _, _ -> Fieldname.equal fn fieldname) fields with
      | Some (_, ft, _) ->
          Some (get_type_name ft)
      | None ->
          None )
    | None ->
        None )
  | _ ->
      None


module Java = struct
  let implements interface tenv typename =
    let is_interface s _ = String.equal interface (Typ.Name.name s) in
    supertype_exists tenv is_interface (Typ.Name.Java.from_string typename)


  let implements_arrays = implements "java.util.Arrays"

  let implements_iterable = implements "java.lang.Iterable"

  let implements_iterator = implements "java.util.Iterator"

  let implements_collection = implements "java.util.Collection"

  let implements_collections = implements "java.util.Collections"

  let implements_list = implements "java.util.List"

  let implements_xmob_utils class_name = implements ("com.moblica.common.xmob.utils." ^ class_name)

  let implements_pseudo_collection =
    let androidx_class_names =
      List.map
        ~f:(fun class_name -> "androidx.collection." ^ class_name)
        [ "ArrayMap"
        ; "ArraySet"
        ; "CircularArray"
        ; "LongSparseArray"
        ; "LruCache"
        ; "SimpleArrayMap"
        ; "SparseArrayCompat" ]
    in
    fun t s ->
      implements "android.util.SparseArray" t s
      || implements "android.util.SparseIntArray" t s
      || implements_xmob_utils "IntArrayList" t s
      || List.exists ~f:(fun class_name -> implements class_name t s) androidx_class_names


  let implements_enumeration = implements "java.util.Enumeration"

  let implements_inject class_name = implements ("javax.inject." ^ class_name)

  let implements_io class_name = implements ("java.io." ^ class_name)

  let implements_nio class_name = implements ("java.nio." ^ class_name)

  let implements_map = implements "java.util.Map"

  let implements_androidx_map = implements "androidx.collection.SimpleArrayMap"

  let implements_set = implements "java.util.Set"

  let implements_map_entry = implements "java.util.Map$Entry"

  let implements_queue = implements "java.util.Queue"

  let implements_lang class_name = implements ("java.lang." ^ class_name)

  let implements_google class_name = implements ("com.google." ^ class_name)

  let implements_android class_name = implements ("android." ^ class_name)

  let implements_infer_annotation class_name =
    implements ("com.facebook.infer.annotation." ^ class_name)


  let implements_jackson class_name = implements ("com.fasterxml.jackson." ^ class_name)

  let implements_org_json class_name = implements ("org.json." ^ class_name)

  let implements_app_activity = implements "android.app.Activity"

  let implements_app_fragment = implements "androidx.fragment.app.Fragment"

  let implements_graphql_story = implements "com.facebook.graphql.model.GraphQLStory"

  let implements_psi_element = implements "com.intellij.psi.PsiElement"

  let implements_sparse_float_array = implements "com.facebook.litho.internal.SparseFloatArray"

  let implements_view_group = implements "android.view.ViewGroup"

  let implements_view_parent = implements "android.view.ViewParent"

  let initializer_classes =
    List.map ~f:Typ.Name.Java.from_string
      [ "android.app.Activity"
      ; "android.app.Application"
      ; "android.app.Fragment"
      ; "android.app.Service"
      ; "android.support.v4.app.Fragment"
      ; "androidx.fragment.app.Fragment"
      ; "junit.framework.TestCase" ]


  let initializer_methods =
    ["onActivityCreated"; "onAttach"; "onCreate"; "onCreateView"; "setUp"; "onViewCreated"]


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
    match get_this_type_nonstatic_methods_only proc_attributes with
    | Some this_type ->
        if type_has_initializer tenv this_type then
          match proc_attributes.ProcAttributes.proc_name with
          | Procname.Java pname_java ->
              let mname = Procname.Java.get_method pname_java in
              List.exists ~f:(String.equal mname) initializer_methods
          | _ ->
              false
        else false
    | None ->
        false


  let get_const_type_name (const : Const.t) : string =
    match const with
    | Const.Cstr _ ->
        "java.lang.String"
    | Const.Cint _ ->
        "java.lang.Integer"
    | Const.Cfloat _ ->
        "java.lang.Double"
    | _ ->
        ""


  (** Checks if the class name is a Java exception *)
  let is_throwable tenv typename = is_subtype_of_str tenv typename "java.lang.Throwable"

  let is_enum tenv typename = is_subtype_of_str tenv typename "java.lang.Enum"

  (** tests whether any class attributes (e.g., [@ThreadSafe]) pass check of first argument,
      including for supertypes*)
  let check_class_attributes check tenv = function
    | Procname.Java java_pname ->
        let check_class_annots _ {Struct.annots} = check annots in
        supertype_exists tenv check_class_annots (Procname.Java.get_class_type_name java_pname)
    | _ ->
        false


  (** tests whether any class attributes (e.g., [@ThreadSafe]) pass check of first argument, for the
      current class only*)
  let check_current_class_attributes check tenv = function
    | Procname.Java java_pname -> (
      match Tenv.lookup tenv (Procname.Java.get_class_type_name java_pname) with
      | Some struct_typ ->
          check struct_typ.annots
      | _ ->
          false )
    | _ ->
        false


  (** find superclasss with attributes (e.g., [@ThreadSafe]), including current class*)
  let rec find_superclasses_with_attributes check tenv tname =
    match Tenv.lookup tenv tname with
    | Some struct_typ ->
        let result_from_supers =
          List.concat (List.map ~f:(find_superclasses_with_attributes check tenv) struct_typ.supers)
        in
        if check struct_typ.annots then tname :: result_from_supers else result_from_supers
    | _ ->
        []


  let is_override_of_lang_object_equals curr_pname =
    let is_only_param_of_object_type = function
      | [Procname.Parameter.JavaParameter param_type]
        when Typ.equal param_type StdTyp.Java.pointer_to_java_lang_object ->
          true
      | _ ->
          false
    in
    String.equal (Procname.get_method curr_pname) "equals"
    && is_only_param_of_object_type (Procname.get_parameters curr_pname)
end

module ObjectiveC = struct
  let implements interface tenv typename =
    let is_interface s _ = String.equal interface (Typ.Name.name s) in
    supertype_exists tenv is_interface (Typ.Name.Objc.from_string typename)


  let conforms_to =
    let protocol_reg = Str.regexp ".+<\\(.+\\)>" in
    fun ~protocol tenv typename ->
      let is_protocol s = String.equal protocol (Typ.Name.name s) in
      protocol_exists tenv is_protocol (Typ.Name.Objc.from_string typename)
      || (* Corresponds to the case where we look inside protocols in
            ObjCClass<P1,P2...Pn> *)
      Str.string_match protocol_reg typename 0
      &&
      let conformed_protocols = Str.matched_group 1 typename |> String.split ~on:',' in
      List.exists conformed_protocols ~f:(String.equal protocol)


  let implements_collection =
    let coll = ["NSArray"; "NSDictionary"; "NSOrderedSet"; "NSSet"] in
    fun tenv typ_str -> List.exists ~f:(fun obj_class -> implements obj_class tenv typ_str) coll


  let is_core_graphics_create_or_copy _ procname =
    String.is_prefix ~prefix:"CG" procname
    && ( String.is_substring ~substring:"Create" procname
       || String.is_substring ~substring:"Copy" procname )


  let is_core_foundation_create_or_copy _ procname =
    String.is_prefix ~prefix:"CF" procname
    && ( String.is_substring ~substring:"Create" procname
       || String.is_substring ~substring:"Copy" procname )


  let is_core_graphics_release _ procname =
    String.is_prefix ~prefix:"CG" procname && String.is_suffix ~suffix:"Release" procname


  let is_modelled_as_alloc _ procname =
    match Config.pulse_model_alloc_pattern with
    | Some regex ->
        Str.string_match regex procname 0
    | None ->
        false


  let is_modelled_as_release _ procname =
    match Config.pulse_model_release_pattern with
    | Some regex ->
        Str.string_match regex procname 0
    | None ->
        false
end

let get_vararg_type_names tenv (call_node : Procdesc.Node.t) (ivar : Pvar.t) : string list =
  (* Is this the node creating ivar? *)
  let initializes_array instrs =
    instrs
    |> Instrs.find_map ~f:(function
         | Sil.Store {e1= Exp.Lvar iv; e2= Exp.Var t2} when Pvar.equal ivar iv ->
             Some t2
         | _ ->
             None )
    |> Option.exists ~f:(fun t2 ->
           Instrs.exists instrs ~f:(function
             | Sil.Call ((t1, _), Exp.Const (Const.Cfun pn), _, _, _) ->
                 Ident.equal t1 t2 && Procname.equal pn (Procname.from_string_c_fun "__new_array")
             | _ ->
                 false ) )
  in
  (* Get the type name added to ivar or None *)
  let added_type_name instrs =
    let nvar_type_name nvar =
      instrs
      |> Instrs.find_map ~f:(function
           | Sil.Load {id= nv; e; typ= t} when Ident.equal nv nvar ->
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
           | Sil.Store {e1= Exp.Lindex (Exp.Var iv, _); e2= Exp.Var nvar}
             when Ident.equal iv array_nvar ->
               Some (nvar_type_name nvar)
           | Sil.Store {e1= Exp.Lindex (Exp.Var iv, _); e2= Exp.Const c}
             when Ident.equal iv array_nvar ->
               Some (Some (Java.get_const_type_name c))
           | _ ->
               None )
      |> Option.join
    in
    let array_nvar =
      instrs
      |> Instrs.find_map ~f:(function
           | Sil.Load {id= nv; e= Exp.Lvar iv} when Pvar.equal iv ivar ->
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


let proc_calls resolve_attributes pdesc filter : (Procname.t * ProcAttributes.t) list =
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
  List.iter ~f:do_node nodes ;
  List.rev !res


let has_same_signature proc_name =
  let method_name = Procname.get_method proc_name in
  let params = Procname.get_parameters proc_name in
  Staged.stage (fun other_pname ->
      let other_method_name = Procname.get_method other_pname in
      let other_params = Procname.get_parameters other_pname in
      (not (Procname.is_constructor other_pname))
      && String.equal other_method_name method_name
      (* Check that parameter types match exactly (no subtyping or what not). *)
      &&
      match List.for_all2 params other_params ~f:Procname.Parameter.equal with
      | List.Or_unequal_lengths.Ok res ->
          res
      | List.Or_unequal_lengths.Unequal_lengths ->
          false )


let override_find ?(check_current_type = true) f tenv proc_name =
  let is_override = Staged.unstage (has_same_signature proc_name) in
  let rec find_super_type super_class_name =
    Tenv.lookup tenv super_class_name
    |> Option.bind ~f:(fun {Struct.methods; supers} ->
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
    | Procname.Java proc_name_java ->
        find_super_type (Procname.Java.get_class_type_name proc_name_java)
    | Procname.ObjC_Cpp proc_name_cpp ->
        find_super_type (Procname.ObjC_Cpp.get_class_type_name proc_name_cpp)
    | _ ->
        None


let override_exists ?(check_current_type = true) f tenv proc_name =
  override_find ~check_current_type f tenv proc_name |> Option.is_some


(* Only java supported at the moment *)

let override_iter f tenv proc_name =
  ignore
    (override_exists
       (fun pname ->
         f pname ;
         false )
       tenv proc_name)


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


let lookup_attributes_exn tenv proc_name =
  match lookup_attributes tenv proc_name with
  | Some result ->
      result
  | None ->
      Logging.die InternalError "Did not find attributes for %a" Procname.pp proc_name


(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
let get_fields_nullified procdesc =
  (* walk through the instructions and look for instance fields that are assigned to null *)
  let collect_nullified_flds (nullified_flds, this_ids) _ = function
    | Sil.Store {e1= Exp.Lfield (Exp.Var lhs, fld, _); e2= rhs}
      when Exp.is_null_literal rhs && Ident.Set.mem lhs this_ids ->
        (Fieldname.Set.add fld nullified_flds, this_ids)
    | Sil.Load {id; e= rhs} when Exp.is_this rhs ->
        (nullified_flds, Ident.Set.add id this_ids)
    | _ ->
        (nullified_flds, this_ids)
  in
  let nullified_flds, _ =
    Procdesc.fold_instrs procdesc ~f:collect_nullified_flds
      ~init:(Fieldname.Set.empty, Ident.Set.empty)
  in
  nullified_flds
