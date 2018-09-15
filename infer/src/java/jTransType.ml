(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
open Sawja_pack
module L = Logging

(** Type transformations between Javalib datatypes and sil datatypes *)

exception Type_tranlsation_error of string

let basic_type = function
  | `Int ->
      Typ.mk (Tint Typ.IInt)
  | `Bool ->
      Typ.mk (Tint Typ.IBool)
  | `Byte ->
      Typ.mk (Tint Typ.IChar)
  | `Char ->
      Typ.mk (Tint Typ.IChar)
  | `Double ->
      Typ.mk (Tfloat Typ.FDouble)
  | `Float ->
      Typ.mk (Tfloat Typ.FFloat)
  | `Long ->
      Typ.mk (Tint Typ.ILong)
  | `Short ->
      Typ.mk (Tint Typ.IShort)


let cast_type = function
  | JBir.F2I | JBir.L2I | JBir.D2I ->
      Typ.mk (Typ.Tint Typ.IInt)
  | JBir.D2L | JBir.F2L | JBir.I2L ->
      Typ.mk (Typ.Tint Typ.ILong)
  | JBir.I2F | JBir.L2F | JBir.D2F ->
      Typ.mk (Typ.Tfloat Typ.FFloat)
  | JBir.L2D | JBir.F2D | JBir.I2D ->
      Typ.mk (Typ.Tfloat Typ.FDouble)
  | JBir.I2B ->
      Typ.mk (Typ.Tint Typ.IBool)
  | JBir.I2C ->
      Typ.mk (Typ.Tint Typ.IChar)
  | JBir.I2S ->
      Typ.mk (Typ.Tint Typ.IShort)


let const_type const =
  match const with
  | `String _ ->
      JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.string_cl))
  | `Class _ ->
      JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.class_cl))
  | `Double _ ->
      JBasics.TBasic `Double
  | `Int _ ->
      JBasics.TBasic `Int
  | `Float _ ->
      JBasics.TBasic `Float
  | `Long _ ->
      JBasics.TBasic `Long
  | `ANull ->
      JConfig.obj_type


let typename_of_classname cn = Typ.Name.Java.from_string (JBasics.cn_name cn)

let rec get_named_type vt : Typ.t =
  match vt with
  | JBasics.TBasic bt ->
      basic_type bt
  | JBasics.TObject ot -> (
    match ot with
    | JBasics.TArray vt ->
        let content_type = get_named_type vt in
        Typ.mk (Tptr (Typ.mk_array content_type, Typ.Pk_pointer))
    | JBasics.TClass cn ->
        Typ.mk (Tptr (Typ.mk (Tstruct (typename_of_classname cn)), Typ.Pk_pointer)) )


let rec create_array_type typ dim =
  if dim > 0 then
    let content_typ = create_array_type typ (dim - 1) in
    Typ.mk (Tptr (Typ.mk_array content_typ, Typ.Pk_pointer))
  else typ


let extract_cn_no_obj typ =
  match typ.Typ.desc with
  | Typ.Tptr ({desc= Tstruct (JavaClass _ as name)}, Pk_pointer) ->
      let class_name = JBasics.make_cn (Typ.Name.name name) in
      if JBasics.cn_equal class_name JBasics.java_lang_object then None
      else
        let jbir_class_name = class_name in
        Some jbir_class_name
  | _ ->
      None


(** Printing types *)
let rec array_type_to_string vt =
  let s =
    match vt with
    | JBasics.TBasic bt -> (
      match bt with
      | `Bool ->
          JConfig.boolean_code
      | `Byte ->
          JConfig.byte_code
      | `Char ->
          JConfig.char_code
      | `Double ->
          JConfig.double_code
      | `Float ->
          JConfig.float_code
      | `Int ->
          JConfig.int_code
      | `Long ->
          JConfig.long_code
      | `Short ->
          JConfig.short_code )
    | JBasics.TObject ot ->
        object_type_to_string' ot
  in
  "[" ^ s


and object_type_to_string' ot =
  match ot with
  | JBasics.TClass class_name ->
      JConfig.class_code (JBasics.cn_name class_name)
  | JBasics.TArray vt ->
      array_type_to_string vt


let object_type_to_string ot =
  match ot with
  | JBasics.TClass class_name ->
      JBasics.cn_name class_name
  | JBasics.TArray vt ->
      array_type_to_string vt


let string_of_basic_type = function
  | `Bool ->
      JConfig.boolean_st
  | `Byte ->
      JConfig.byte_st
  | `Char ->
      JConfig.char_st
  | `Double ->
      JConfig.double_st
  | `Float ->
      JConfig.float_st
  | `Int ->
      JConfig.int_st
  | `Long ->
      JConfig.long_st
  | `Short ->
      JConfig.short_st


let rec string_of_type vt =
  match vt with
  | JBasics.TBasic bt ->
      string_of_basic_type bt
  | JBasics.TObject ot -> (
    match ot with
    | JBasics.TArray vt ->
        string_of_type vt ^ "[]"
    | JBasics.TClass cn ->
        JBasics.cn_name cn )


let package_to_string = function [] -> None | p -> Some (String.concat ~sep:"." p)

let cn_to_java_type cn =
  Typ.Name.Java.Split.make
    ?package:(package_to_string (JBasics.cn_package cn))
    (JBasics.cn_simple_name cn)


let vt_to_java_type vt =
  match vt with
  | JBasics.TBasic bt ->
      Typ.Name.Java.Split.make (string_of_basic_type bt)
  | JBasics.TObject ot -> (
    match ot with
    | JBasics.TArray vt ->
        Typ.Name.Java.Split.make (string_of_type vt ^ "[]")
    | JBasics.TClass cn ->
        cn_to_java_type cn )


let method_signature_names ms =
  let return_type_name =
    match JBasics.ms_rtype ms with
    | None ->
        if String.equal (JBasics.ms_name ms) JConfig.constructor_name then None
        else Some (Typ.Name.Java.Split.make JConfig.void)
    | Some vt ->
        Some (vt_to_java_type vt)
  in
  let method_name = JBasics.ms_name ms in
  let args_types = List.map ~f:vt_to_java_type (JBasics.ms_args ms) in
  (return_type_name, method_name, args_types)


let get_method_kind m =
  if Javalib.is_static_method m then Typ.Procname.Java.Static else Typ.Procname.Java.Non_Static


let create_fieldname cn fs =
  let fieldname = JBasics.fs_name fs in
  let classname = JBasics.cn_name cn in
  Typ.Fieldname.Java.from_string (classname ^ "." ^ fieldname)


let create_sil_class_field cn {Javalib.cf_signature; cf_annotations; cf_kind} =
  let field_name = create_fieldname cn cf_signature
  and field_type = get_named_type (JBasics.fs_type cf_signature)
  and annotation =
    let real_annotations = JAnnotation.translate_item cf_annotations in
    (* translate modifiers like "volatile" as annotations *)
    match cf_kind with
    | Javalib.Volatile ->
        (Annot.volatile, true) :: real_annotations
    | Javalib.Final ->
        (Annot.final, true) :: real_annotations
    | Javalib.NotFinal ->
        real_annotations
  in
  (field_name, field_type, annotation)


(** Collect static field if static is true, otherwise non-static ones. *)
let collect_class_field cn cf (statics, nonstatics) =
  let field = create_sil_class_field cn cf in
  if Javalib.is_static_field (Javalib.ClassField cf) then (field :: statics, nonstatics)
  else (statics, field :: nonstatics)


(** Collect an interface field. *)
let collect_interface_field cn inf l =
  let fs = inf.Javalib.if_signature in
  let field_type = get_named_type (JBasics.fs_type fs) in
  let field_name = create_fieldname cn fs in
  let annotation = JAnnotation.translate_item inf.Javalib.if_annotations in
  (field_name, field_type, annotation) :: l


let collect_models_class_fields classpath_field_map cn cf fields =
  let static, nonstatic = fields in
  let field_name, field_type, annotation = create_sil_class_field cn cf in
  try
    let classpath_ft = Typ.Fieldname.Map.find field_name classpath_field_map in
    if Typ.equal classpath_ft field_type then fields
    else
      (* TODO (#6711750): fix type equality for arrays before failing here *)
      let () =
        L.(debug Capture Quiet)
          "Found inconsistent types for %s@\n\tclasspath: %a@\n\tmodels: %a@\n@."
          (Typ.Fieldname.to_string field_name)
          (Typ.pp_full Pp.text) classpath_ft (Typ.pp_full Pp.text) field_type
      in
      fields
  with Caml.Not_found ->
    if Javalib.is_static_field (Javalib.ClassField cf) then
      ((field_name, field_type, annotation) :: static, nonstatic)
    else (static, (field_name, field_type, annotation) :: nonstatic)


let add_model_fields program classpath_fields cn =
  let statics, nonstatics = classpath_fields in
  let classpath_field_map =
    let collect_fields map =
      List.fold ~f:(fun map (fn, ft, _) -> Typ.Fieldname.Map.add fn ft map) ~init:map
    in
    collect_fields (collect_fields Typ.Fieldname.Map.empty statics) nonstatics
  in
  try
    match JBasics.ClassMap.find cn (JClasspath.get_models program) with
    | Javalib.JClass _ as jclass ->
        Javalib.cf_fold
          (collect_models_class_fields classpath_field_map cn)
          jclass classpath_fields
    | _ ->
        classpath_fields
  with Caml.Not_found -> classpath_fields


let rec get_method_procname program tenv cn ms method_kind =
  let _ : Typ.Struct.t = get_class_struct_typ program tenv cn in
  let return_type_name, method_name, args_type_name = method_signature_names ms in
  let class_name = Typ.Name.Java.from_string (JBasics.cn_name cn) in
  let proc_name_java =
    Typ.Procname.Java.make class_name return_type_name method_name args_type_name method_kind
  in
  Typ.Procname.Java proc_name_java


(* create a mangled procname from an abstract or concrete method *)
and translate_method_name program tenv m =
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let proc_name = get_method_procname program tenv cn ms (get_method_kind m) in
  JClasspath.add_missing_callee program proc_name cn ms ;
  proc_name


and get_all_fields program tenv cn =
  let extract_class_fields classname =
    let {Typ.Struct.fields; statics} = get_class_struct_typ program tenv classname in
    (statics, fields)
  in
  let trans_fields classname =
    match JClasspath.lookup_node classname program with
    | Some (Javalib.JClass jclass) ->
        let superclass_fields =
          match jclass.Javalib.c_super_class with
          | None ->
              ([], [])
          | Some super_classname ->
              extract_class_fields super_classname
        in
        let super_fields =
          let rev_pair (l1, l2) = (List.rev l1, List.rev l2) in
          List.fold
            ~f:(fun (statics, fields) interface_name ->
              let interface_statics, interface_fields = extract_class_fields interface_name in
              (interface_statics @ statics, interface_fields @ fields) )
            ~init:(rev_pair superclass_fields) jclass.Javalib.c_interfaces
          |> rev_pair
        in
        Javalib.cf_fold (collect_class_field classname) (Javalib.JClass jclass) super_fields
    | Some (Javalib.JInterface jinterface) ->
        let interface_fields =
          Javalib.if_fold (collect_interface_field classname) (Javalib.JInterface jinterface) []
        in
        (interface_fields, [])
    | _ ->
        ([], [])
  in
  trans_fields cn


and get_class_struct_typ =
  let seen = ref JBasics.ClassSet.empty in
  fun program tenv cn ->
    let name = typename_of_classname cn in
    match Tenv.lookup tenv name with
    | Some struct_typ ->
        struct_typ
    | None when JBasics.ClassSet.mem cn !seen ->
        Tenv.mk_struct tenv name
    | None -> (
        seen := JBasics.ClassSet.add cn !seen ;
        match JClasspath.lookup_node cn program with
        | None ->
            Tenv.mk_struct tenv name
        | Some node ->
            let create_super_list interface_names =
              List.iter
                ~f:(fun cn -> ignore (get_class_struct_typ program tenv cn))
                interface_names ;
              List.map ~f:typename_of_classname interface_names
            in
            let supers, fields, statics, annots =
              match node with
              | Javalib.JInterface jinterface ->
                  let statics, _ = get_all_fields program tenv cn in
                  let sil_interface_list = create_super_list jinterface.Javalib.i_interfaces in
                  let item_annotation =
                    JAnnotation.translate_item jinterface.Javalib.i_annotations
                  in
                  (sil_interface_list, [], statics, item_annotation)
              | Javalib.JClass jclass ->
                  let statics, nonstatics =
                    let classpath_static, classpath_nonstatic = get_all_fields program tenv cn in
                    add_model_fields program (classpath_static, classpath_nonstatic) cn
                  in
                  let item_annotation = JAnnotation.translate_item jclass.Javalib.c_annotations in
                  let interface_list = create_super_list jclass.Javalib.c_interfaces in
                  let super_classname_list =
                    match jclass.Javalib.c_super_class with
                    | None ->
                        interface_list (* base case of the recursion *)
                    | Some super_cn ->
                        ignore (get_class_struct_typ program tenv super_cn) ;
                        let super_classname = typename_of_classname super_cn in
                        super_classname :: interface_list
                  in
                  (super_classname_list, nonstatics, statics, item_annotation)
            in
            let methods =
              Javalib.m_fold
                (fun m procnames -> translate_method_name program tenv m :: procnames)
                node []
            in
            Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots name )


let get_class_type_no_pointer program tenv cn =
  ignore (get_class_struct_typ program tenv cn) ;
  Typ.mk (Tstruct (typename_of_classname cn))


let get_class_type program tenv cn =
  Typ.mk (Tptr (get_class_type_no_pointer program tenv cn, Pk_pointer))


(** return true if [field_name] is the autogenerated C.$assertionsDisabled field for class C *)
let is_autogenerated_assert_field field_name =
  String.equal (Typ.Fieldname.Java.get_field field_name) "$assertionsDisabled"


(** translate an object type *)
let rec object_type program tenv ot =
  match ot with
  | JBasics.TClass cn ->
      get_class_type program tenv cn
  | JBasics.TArray at ->
      Typ.mk (Tptr (Typ.mk_array (value_type program tenv at), Typ.Pk_pointer))


(** translate a value type *)
and value_type program tenv vt =
  match vt with
  | JBasics.TBasic bt ->
      basic_type bt
  | JBasics.TObject ot ->
      object_type program tenv ot


(**  Translate object types into Exp.Sizeof expressions *)
let sizeof_of_object_type program tenv ot subtype =
  match (object_type program tenv ot).Typ.desc with
  | Typ.Tptr (typ, _) ->
      Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype}
  | _ ->
      raise (Type_tranlsation_error "Pointer or array type expected in tenv")


(** return the name and type of a formal parameter, looking up the class name in case of "this" *)
let param_type program tenv cn name vt =
  if String.equal (JBir.var_name_g name) (Mangled.to_string JConfig.this) then
    get_class_type program tenv cn
  else value_type program tenv vt


let get_var_type_from_sig (context : JContext.t) var =
  let program = context.program in
  let tenv = JContext.get_tenv context in
  List.find_map
    ~f:(fun (vt', var') ->
      if JBir.var_equal var var' then Some (param_type program tenv context.cn var' vt') else None
      )
    (JBir.params context.impl)


let get_var_type context var =
  let typ_opt = JContext.get_var_type context var in
  match typ_opt with Some _ -> typ_opt | None -> get_var_type_from_sig context var


let extract_array_type typ =
  match typ.Typ.desc with Typ.Tptr ({desc= Tarray {elt}}, Typ.Pk_pointer) -> elt | _ -> typ


(** translate the type of an expression, looking in the method signature for formal parameters
    this is because variables in expressions do not have accurate types *)
let rec expr_type (context : JContext.t) expr =
  let program = context.program in
  let tenv = JContext.get_tenv context in
  match expr with
  | JBir.Const const ->
      value_type program tenv (const_type const)
  | JBir.Var (vt, var) -> (
    match get_var_type context var with Some typ -> typ | None -> value_type program tenv vt )
  | JBir.Binop (JBir.ArrayLoad _, e1, _) ->
      let typ = expr_type context e1 in
      extract_array_type typ
  | _ ->
      value_type program tenv (JBir.type_of_expr expr)


(** Returns the return type of the method based on the return type
    specified in ms. *)
let return_type program tenv ms =
  match JBasics.ms_rtype ms with None -> Typ.mk Tvoid | Some vt -> value_type program tenv vt
