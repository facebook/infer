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

(** Type transformations between Javalib datatypes and sil datatypes *)

exception Type_tranlsation_error of string

let basic_type = function
  | `Int -> Typ.Tint Typ.IInt
  | `Bool -> Typ.Tint Typ.IBool
  | `Byte -> Typ.Tint Typ.IChar
  | `Char -> Typ.Tint Typ.IChar
  | `Double -> Typ.Tfloat Typ.FDouble
  | `Float -> Typ.Tfloat Typ.FFloat
  | `Long -> Typ.Tint Typ.ILong
  | `Short -> Typ.Tint Typ.IShort


let cast_type = function
  | JBir.F2I
  | JBir.L2I
  | JBir.D2I -> Typ.Tint Typ.IInt
  | JBir.D2L
  | JBir.F2L
  | JBir.I2L -> Typ.Tint Typ.ILong
  | JBir.I2F
  | JBir.L2F
  | JBir.D2F -> Typ.Tfloat Typ.FFloat
  | JBir.L2D
  | JBir.F2D
  | JBir.I2D -> Typ.Tfloat Typ.FDouble
  | JBir.I2B -> Typ.Tint Typ.IBool
  | JBir.I2C -> Typ.Tint Typ.IChar
  | JBir.I2S -> Typ.Tint Typ.IShort


let const_type const =
  match const with
  | `String _ -> (JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.string_cl)))
  | `Class _ -> (JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.class_cl)))
  | `Double _ -> (JBasics.TBasic `Double)
  | `Int _ -> (JBasics.TBasic`Int)
  | `Float _ -> (JBasics.TBasic`Float)
  | `Long _ -> (JBasics.TBasic`Long)
  | `ANull -> JConfig.obj_type


let typename_of_classname cn =
  Typename.Java.from_string (JBasics.cn_name cn)


let rec get_named_type vt =
  match vt with
  | JBasics.TBasic bt -> basic_type bt
  | JBasics.TObject ot ->
      begin
        match ot with
        | JBasics.TArray vt ->
            let content_type = get_named_type vt in
            Typ.Tptr (Typ.Tarray (content_type, None), Typ.Pk_pointer)
        | JBasics.TClass cn -> Typ.Tptr (Typ.Tstruct (typename_of_classname cn), Typ.Pk_pointer)
      end


let extract_cn_type_np typ =
  match typ with
  | Typ.Tptr(vtyp, Typ.Pk_pointer) ->
      vtyp
  | _ -> typ

let rec create_array_type typ dim =
  if dim > 0 then
    let content_typ = create_array_type typ (dim - 1) in
    Typ.Tptr(Typ.Tarray (content_typ, None), Typ.Pk_pointer)
  else typ

let extract_cn_no_obj typ =
  match typ with
  | Typ.Tptr (Tstruct (TN_csu (Class _, _) as name), Pk_pointer) ->
      let class_name = JBasics.make_cn (Typename.name name) in
      if JBasics.cn_equal class_name JBasics.java_lang_object then None
      else
        let jbir_class_name = class_name in
        Some jbir_class_name
  | _ -> None

(** Printing types *)
let rec array_type_to_string vt =
  let s =
    match vt with
    | JBasics.TBasic bt ->
        (match bt with
         | `Bool -> JConfig.boolean_code
         | `Byte -> JConfig.byte_code
         | `Char -> JConfig.char_code
         | `Double -> JConfig.double_code
         | `Float -> JConfig.float_code
         | `Int -> JConfig.int_code
         | `Long -> JConfig.long_code
         | `Short -> JConfig.short_code)
    | JBasics.TObject ot -> object_type_to_string' ot in
  "["^s
and object_type_to_string' ot =
  match ot with
  | JBasics.TClass class_name -> JConfig.class_code (JBasics.cn_name class_name)
  | JBasics.TArray vt -> (array_type_to_string vt)

let object_type_to_string ot =
  match ot with
  | JBasics.TClass class_name -> (JBasics.cn_name class_name)
  | JBasics.TArray vt -> (array_type_to_string vt)

let string_of_basic_type = function
  | `Bool -> JConfig.boolean_st
  | `Byte -> JConfig.byte_st
  | `Char -> JConfig.char_st
  | `Double -> JConfig.double_st
  | `Float -> JConfig.float_st
  | `Int -> JConfig.int_st
  | `Long -> JConfig.long_st
  | `Short -> JConfig.short_st

let rec string_of_type vt =
  match vt with
  | JBasics.TBasic bt -> string_of_basic_type bt
  | JBasics.TObject ot ->
      begin
        match ot with
        | JBasics.TArray vt -> (string_of_type vt)^"[]"
        | JBasics.TClass cn -> JBasics.cn_name cn
      end

let package_to_string p =
  let rec aux p =
    match p with
    | [] -> ""
    | p::[] -> p
    | p:: rest -> p^"."^(aux rest) in
  match p with
  | [] -> None
  | _ -> Some (aux p)


let cn_to_java_type cn =
  (package_to_string (JBasics.cn_package cn),
   (JBasics.cn_simple_name cn))


let vt_to_java_type vt =
  match vt with
  | JBasics.TBasic bt -> None, string_of_basic_type bt
  | JBasics.TObject ot ->
      begin
        match ot with
        | JBasics.TArray vt -> None, (string_of_type vt)^"[]"
        | JBasics.TClass cn -> cn_to_java_type cn

      end

let method_signature_names ms =
  let return_type_name =
    match JBasics.ms_rtype ms with
    | None ->
        if JBasics.ms_name ms = JConfig.constructor_name then
          None
        else
          Some (None, JConfig.void)
    | Some vt -> Some (vt_to_java_type vt) in
  let rec args_to_signature l =
    match l with
    | [] -> []
    | vt:: tail -> (vt_to_java_type vt) :: (args_to_signature tail) in
  let method_name = JBasics.ms_name ms in
  let args_types = args_to_signature (JBasics.ms_args ms) in
  (return_type_name, method_name, args_types)

let get_method_kind m =
  if Javalib.is_static_method m
  then Procname.Static
  else Procname.Non_Static

(* create a mangled procname from an abstract or concrete method *)
let get_method_procname cn ms kind =
  let return_type_name, method_name, args_type_name = method_signature_names ms in
  let class_name = cn_to_java_type cn in
  Procname.java class_name return_type_name method_name args_type_name kind

let get_class_procnames cn node =
  let collect jmethod procnames =
    let ms = Javalib.get_method_signature jmethod in
    let kind = get_method_kind jmethod in
    (get_method_procname cn ms kind) :: procnames in
  Javalib.m_fold collect node []

let create_fieldname cn fs =
  let fieldname cn fs =
    let fieldname = (JBasics.fs_name fs) in
    let classname = (JBasics.cn_name cn) in
    Mangled.from_string (classname^"."^fieldname) in
  Ident.create_fieldname (fieldname cn fs) 0

let create_sil_class_field cn cf =
  let fs = cf.Javalib.cf_signature in
  let field_name = create_fieldname cn fs
  and field_type = get_named_type (JBasics.fs_type fs)
  and annotation = JAnnotation.translate_item cf.Javalib.cf_annotations in
  (field_name, field_type, annotation)


(** Collect static field if static is true, otherwise non-static ones. *)
let collect_class_field cn cf (statics, nonstatics) =
  let field = create_sil_class_field cn cf in
  if Javalib.is_static_field (Javalib.ClassField cf) then
    (field :: statics, nonstatics)
  else
    (statics, field :: nonstatics)


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
    let classpath_ft = Ident.FieldMap.find field_name classpath_field_map in
    if Typ.equal classpath_ft field_type then fields
    else
      (* TODO (#6711750): fix type equality for arrays before failing here *)
      let () = Logging.stderr "Found inconsistent types for %s\n\tclasspath: %a\n\tmodels: %a\n@."
          (Ident.fieldname_to_string field_name)
          (Typ.pp_full pe_text) classpath_ft
          (Typ.pp_full pe_text) field_type in fields
  with Not_found ->
    if Javalib.is_static_field (Javalib.ClassField cf) then
      ((field_name, field_type, annotation):: static, nonstatic)
    else
      (static, (field_name, field_type, annotation):: nonstatic)


let add_model_fields program classpath_fields cn =
  let statics, nonstatics = classpath_fields in
  let classpath_field_map =
    let collect_fields map =
      IList.fold_left
        (fun map (fn, ft, _) -> Ident.FieldMap.add fn ft map) map in
    collect_fields (collect_fields Ident.FieldMap.empty statics) nonstatics in
  try
    match JBasics.ClassMap.find cn (JClasspath.get_models program) with
    | Javalib.JClass _ as jclass ->
        Javalib.cf_fold
          (collect_models_class_fields classpath_field_map cn)
          jclass
          classpath_fields
    | _ ->
        classpath_fields
  with Not_found -> classpath_fields


let rec get_all_fields program tenv cn =
  let extract_class_fields classname =
    let { StructTyp.fields; statics } = get_class_struct_typ program tenv classname in
    (statics, fields) in
  let trans_fields classname =
    match JClasspath.lookup_node classname program with
    | Some (Javalib.JClass jclass) ->
        let super_fields =
          match jclass.Javalib.c_super_class with
          | None -> ([], [])
          | Some super_classname -> extract_class_fields super_classname in
        Javalib.cf_fold (collect_class_field classname) (Javalib.JClass jclass) super_fields
    | Some (Javalib.JInterface jinterface) ->
        let interface_fields =
          Javalib.if_fold (collect_interface_field classname) (Javalib.JInterface jinterface) [] in
        (interface_fields, [])
    | _ -> ([], []) in
  trans_fields cn


and get_class_struct_typ program tenv cn =
  let name = typename_of_classname cn in
  match Tenv.lookup tenv name with
  | Some struct_typ ->
      struct_typ
  | None ->
      match JClasspath.lookup_node cn program with
      | None ->
          Tenv.mk_struct tenv name
      | Some node ->
          let create_super_list interface_names =
            IList.iter (fun cn -> ignore (get_class_struct_typ program tenv cn)) interface_names;
            IList.map typename_of_classname interface_names in
          let supers, fields, statics, annots =
            match node with
            | Javalib.JInterface jinterface ->
                let statics, _ = get_all_fields program tenv cn in
                let sil_interface_list = create_super_list jinterface.Javalib.i_interfaces in
                let item_annotation = JAnnotation.translate_item jinterface.Javalib.i_annotations in
                (sil_interface_list, [], statics, item_annotation)
            | Javalib.JClass jclass ->
                let statics, nonstatics =
                  let classpath_static, classpath_nonstatic = get_all_fields program tenv cn in
                  add_model_fields program (classpath_static, classpath_nonstatic) cn in
                let item_annotation = JAnnotation.translate_item jclass.Javalib.c_annotations in
                let interface_list = create_super_list jclass.Javalib.c_interfaces in
                let super_classname_list =
                  match jclass.Javalib.c_super_class with
                  | None -> interface_list (* base case of the recursion *)
                  | Some super_cn ->
                      ignore (get_class_struct_typ program tenv super_cn);
                      let super_classname = typename_of_classname super_cn in
                      super_classname :: interface_list in
                (super_classname_list, nonstatics, statics, item_annotation) in
          let methods = IList.map (fun j -> Procname.Java j) (get_class_procnames cn node) in
          Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots name

let get_class_type_no_pointer program tenv cn =
  ignore (get_class_struct_typ program tenv cn);
  Typ.Tstruct (typename_of_classname cn)

let get_class_type program tenv cn =
  Typ.Tptr (get_class_type_no_pointer program tenv cn, Pk_pointer)

(** return true if [field_name] is the autogenerated C.$assertionsDisabled field for class C *)
let is_autogenerated_assert_field field_name =
  string_equal (Ident.java_fieldname_get_field field_name) "$assertionsDisabled"

let is_closeable program tenv typ =
  let closeable_cn = JBasics.make_cn "java.io.Closeable" in
  let closeable_typ = get_class_type program tenv closeable_cn in
  let autocloseable_cn = JBasics.make_cn "java.lang.AutoCloseable" in
  let autocloseable_typ = get_class_type program tenv autocloseable_cn in
  let implements t = Prover.Subtyping_check.check_subtype tenv typ t in
  implements closeable_typ || implements autocloseable_typ


(** translate an object type *)
let rec object_type program tenv ot =
  match ot with
  | JBasics.TClass cn -> get_class_type program tenv cn
  | JBasics.TArray at -> Typ.Tptr (Typ.Tarray (value_type program tenv at, None), Typ.Pk_pointer)

(** translate a value type *)
and value_type program tenv vt =
  match vt with
  | JBasics.TBasic bt -> basic_type bt
  | JBasics.TObject ot -> object_type program tenv ot


(**  Translate object types into Exp.Sizeof expressions *)
let sizeof_of_object_type program tenv ot subtypes =
  match object_type program tenv ot with
  | Typ.Tptr (typ, _) ->
      Exp.Sizeof (typ, None, subtypes)
  | _ ->
      raise (Type_tranlsation_error "Pointer or array type expected in tenv")


(** return the name and type of a formal parameter, looking up the class name in case of "this" *)
let param_type program tenv cn name vt =
  if (JBir.var_name_g name) = Mangled.to_string JConfig.this
  then get_class_type program tenv cn
  else value_type program tenv vt


let get_var_type_from_sig (context : JContext.t) var =
  let program = context.program in
  try
    let tenv = JContext.get_tenv context in
    let vt', var' =
      IList.find
        (fun (_, var') -> JBir.var_equal var var')
        (JBir.params context.impl) in
    Some (param_type program tenv context.cn var' vt')
  with Not_found -> None


let get_var_type context var =
  let typ_opt = JContext.get_var_type context var in
  match typ_opt with
  | Some _ -> typ_opt
  | None -> get_var_type_from_sig context var


let extract_array_type typ =
  match typ with
  | Typ.Tptr(Typ.Tarray (vtyp, _), Typ.Pk_pointer) -> vtyp
  | _ -> typ


(** translate the type of an expression, looking in the method signature for formal parameters
    this is because variables in expressions do not have accurate types *)
let rec expr_type (context : JContext.t) expr =
  let program = context.program in
  let tenv = JContext.get_tenv context in
  match expr with
  | JBir.Const const -> value_type program tenv (const_type const)
  | JBir.Var (vt, var) ->
      (match get_var_type context var with
       | Some typ -> typ
       | None -> (value_type program tenv vt))
  | JBir.Binop ((JBir.ArrayLoad _), e1, _) ->
      let typ = expr_type context e1 in
      (extract_array_type typ)
  | _ -> value_type program tenv (JBir.type_of_expr expr)


(** Returns the return type of the method based on the return type
    specified in ms. *)
let return_type program tenv ms =
  match JBasics.ms_rtype ms with
  | None -> Typ.Tvoid
  | Some vt -> value_type program tenv vt


let add_models_types tenv =
  let add_type t typename struct_typ =
    if not (Tenv.mem t typename) then
      Tenv.add tenv typename struct_typ in
  Tenv.iter (add_type tenv) !JClasspath.models_tenv
