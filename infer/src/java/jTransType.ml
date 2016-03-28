(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Javalib_pack
open Sawja_pack

(** Type transformations between Javalib datatypes and sil datatypes *)

exception Type_tranlsation_error of string

let basic_type = function
  | `Int -> Sil.Tint Sil.IInt
  | `Bool -> Sil.Tint Sil.IBool
  | `Byte -> Sil.Tint Sil.IChar
  | `Char -> Sil.Tint Sil.IChar
  | `Double -> Sil.Tfloat Sil.FDouble
  | `Float -> Sil.Tfloat Sil.FFloat
  | `Long -> Sil.Tint Sil.ILong
  | `Short -> Sil.Tint Sil.IShort


let cast_type = function
  | JBir.F2I
  | JBir.L2I
  | JBir.D2I -> Sil.Tint Sil.IInt
  | JBir.D2L
  | JBir.F2L
  | JBir.I2L -> Sil.Tint Sil.ILong
  | JBir.I2F
  | JBir.L2F
  | JBir.D2F -> Sil.Tfloat Sil.FFloat
  | JBir.L2D
  | JBir.F2D
  | JBir.I2D -> Sil.Tfloat Sil.FDouble
  | JBir.I2B -> Sil.Tint Sil.IBool
  | JBir.I2C -> Sil.Tint Sil.IChar
  | JBir.I2S -> Sil.Tint Sil.IShort


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
  Typename.TN_csu (Csu.Class Csu.Java, (Mangled.from_string (JBasics.cn_name cn)))


let rec get_named_type vt =
  match vt with
  | JBasics.TBasic bt -> basic_type bt
  | JBasics.TObject ot ->
      begin
        match ot with
        | JBasics.TArray vt ->
            let content_type = get_named_type vt in
            let size = Sil.exp_get_undefined false in
            Sil.Tptr (Sil.Tarray (content_type, size), Sil.Pk_pointer) (* unknown size *)
        | JBasics.TClass cn -> Sil.Tptr (Sil.Tvar (typename_of_classname cn), Sil.Pk_pointer)
      end


let extract_cn_type_np typ =
  match typ with
  | Sil.Tptr(vtyp, Sil.Pk_pointer) ->
      vtyp
  | _ -> typ

let rec create_array_type typ dim =
  if dim > 0 then
    let content_typ = create_array_type typ (dim - 1) in
    let size = Sil.exp_get_undefined false in
    Sil.Tptr(Sil.Tarray (content_typ, size), Sil.Pk_pointer)
  else typ

let extract_cn_no_obj typ =
  match typ with
  | Sil.Tptr (Sil.Tstruct { Sil.csu = Csu.Class _; struct_name = Some classname },
              Sil.Pk_pointer) ->
      let class_name = (Mangled.to_string classname) in
      if class_name = JConfig.object_cl then None
      else
        let jbir_class_name = (JBasics.make_cn class_name) in
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

let get_method_kind m = if Javalib.is_static_method m then Procname.Static else Procname.Non_Static

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
let collect_class_field cn cf (static_fields, nonstatic_fields) =
  let field = create_sil_class_field cn cf in
  if Javalib.is_static_field (Javalib.ClassField cf) then
    (field :: static_fields, nonstatic_fields)
  else
    (static_fields, field :: nonstatic_fields)


(** Collect an interface field. *)
let collect_interface_field cn inf l =
  let fs = inf.Javalib.if_signature in
  let field_type = get_named_type (JBasics.fs_type fs) in
  let field_name = create_fieldname cn fs in
  let annotation = JAnnotation.translate_item inf.Javalib.if_annotations in
  (field_name, field_type, annotation) :: l


let dummy_type cn =
  let classname = Mangled.from_string (JBasics.cn_name cn) in
  Sil.Tstruct {
    Sil.instance_fields = [];
    static_fields = [];
    csu = Csu.Class Csu.Java;
    struct_name = Some classname;
    superclasses = [];
    def_methods = [];
    struct_annotations = Sil.item_annotation_empty;
  }


let collect_models_class_fields classpath_field_map cn cf fields =
  let static, nonstatic = fields in
  let field_name, field_type, annotation = create_sil_class_field cn cf in
  try
    let classpath_ft = Ident.FieldMap.find field_name classpath_field_map in
    if Sil.typ_equal classpath_ft field_type then fields
    else
      (* TODO (#6711750): fix type equality for arrays before failing here *)
      let () = Logging.stderr "Found inconsistent types for %s\n\tclasspath: %a\n\tmodels: %a\n@."
          (Ident.fieldname_to_string field_name)
          (Sil.pp_typ_full pe_text) classpath_ft
          (Sil.pp_typ_full pe_text) field_type in fields
  with Not_found ->
    if Javalib.is_static_field (Javalib.ClassField cf) then
      ((field_name, field_type, annotation):: static, nonstatic)
    else
      (static, (field_name, field_type, annotation):: nonstatic)


let add_model_fields program classpath_fields cn =
  let static_fields, nonstatic_fields = classpath_fields in
  let classpath_field_map =
    let collect_fields map =
      IList.fold_left
        (fun map (fn, ft, _) -> Ident.FieldMap.add fn ft map) map in
    collect_fields (collect_fields Ident.FieldMap.empty static_fields) nonstatic_fields in
  try
    match JBasics.ClassMap.find cn (JClasspath.get_models program) with
    | Javalib.JClass _ as jclass ->
        Javalib.cf_fold
          (collect_models_class_fields classpath_field_map cn)
          jclass
          classpath_fields

    | _ -> classpath_fields
  with Not_found -> classpath_fields


let rec get_all_fields program tenv cn =
  let extract_class_fields classname =
    match get_class_type_no_pointer program tenv classname with
    | Sil.Tstruct { Sil.instance_fields; static_fields } -> (static_fields, instance_fields)
    | _ -> assert false in
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


and create_sil_type program tenv cn =
  match JClasspath.lookup_node cn program with
  | None -> dummy_type cn
  | Some node ->
      let create_super_list interface_names =
        IList.iter (fun cn -> ignore (get_class_type_no_pointer program tenv cn)) interface_names;
        IList.map typename_of_classname interface_names in
      let superclasses, instance_fields, static_fields, struct_annotations =
        match node with
        | Javalib.JInterface jinterface ->
            let static_fields, _ = get_all_fields program tenv cn in
            let sil_interface_list = create_super_list jinterface.Javalib.i_interfaces in
            let item_annotation = JAnnotation.translate_item jinterface.Javalib.i_annotations in
            (sil_interface_list, [], static_fields, item_annotation)
        | Javalib.JClass jclass ->
            let static_fields, nonstatic_fields =
              let classpath_static, classpath_nonstatic = get_all_fields program tenv cn in
              add_model_fields program (classpath_static, classpath_nonstatic) cn in
            let item_annotation = JAnnotation.translate_item jclass.Javalib.c_annotations in
            let interface_list = create_super_list jclass.Javalib.c_interfaces in
            let super_classname_list =
              match jclass.Javalib.c_super_class with
              | None -> interface_list (* base case of the recursion *)
              | Some super_cn ->
                  let super_classname =
                    match get_class_type_no_pointer program tenv super_cn with
                    | Sil.Tstruct { Sil.struct_name =  Some classname } ->
                        Typename.TN_csu (Csu.Class Csu.Java, classname)
                    | _ -> assert false in
                  super_classname :: interface_list in
            (super_classname_list, nonstatic_fields, static_fields, item_annotation) in
      let classname = Mangled.from_string (JBasics.cn_name cn) in
      let def_methods = IList.map (fun j -> Procname.Java j) (get_class_procnames cn node) in
      Sil.Tstruct {
        Sil.instance_fields;
        static_fields;
        csu = Csu.Class Csu.Java;
        struct_name = Some classname;
        superclasses;
        def_methods;
        struct_annotations;
      }

and get_class_type_no_pointer program tenv cn =
  let named_type = typename_of_classname cn in
  match Tenv.lookup tenv named_type with
  | None ->
      (match create_sil_type program tenv cn with
       | (Sil.Tstruct struct_typ) as typ->
           Tenv.add tenv named_type struct_typ;
           typ
       | _ -> assert false)
  | Some struct_typ -> Sil.Tstruct struct_typ

let get_class_type program tenv cn =
  let t = get_class_type_no_pointer program tenv cn in
  Sil.Tptr (t, Sil.Pk_pointer)

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
  | JBasics.TArray at ->
      let size = Sil.exp_get_undefined false in
      Sil.Tptr (Sil.Tarray (value_type program tenv at, size), Sil.Pk_pointer)
(** translate a value type *)
and value_type program tenv vt =
  match vt with
  | JBasics.TBasic bt -> basic_type bt
  | JBasics.TObject ot -> object_type program tenv ot


(**  Translate object types into Sil.Sizeof expressions *)
let sizeof_of_object_type program tenv ot subtypes =
  match object_type program tenv ot with
  | Sil.Tptr (Sil.Tarray (vtyp, s), Sil.Pk_pointer) ->
      let typ = (Sil.Tarray (vtyp, s)) in
      Sil.Sizeof (typ, subtypes)
  | Sil.Tptr (typ, _) ->
      Sil.Sizeof (typ, subtypes)
  | _ ->
      raise (Type_tranlsation_error "Pointer or array type expected in tenv")


(** return the name and type of a formal parameter, looking up the class name in case of "this" *)
let param_type program tenv cn name vt =
  if (JBir.var_name_g name) = Mangled.to_string JConfig.this
  then get_class_type program tenv cn
  else value_type program tenv vt


let get_var_type_from_sig context var =
  let program = JContext.get_program context in
  try
    let tenv = JContext.get_tenv context in
    let vt', var' =
      IList.find
        (fun (_, var') -> JBir.var_equal var var')
        (JBir.params (JContext.get_impl context)) in
    Some (param_type program tenv (JContext.get_cn context) var' vt')
  with Not_found -> None


let get_var_type context var =
  let typ_opt = JContext.get_var_type context var in
  match typ_opt with
  | Some _ -> typ_opt
  | None -> get_var_type_from_sig context var


let extract_array_type typ =
  match typ with
  | Sil.Tptr(Sil.Tarray (vtyp, _), Sil.Pk_pointer) -> vtyp
  | _ -> typ


(** translate the type of an expression, looking in the method signature for formal parameters
    this is because variables in expressions do not have accurate types *)
let rec expr_type context expr =
  let program = JContext.get_program context in
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
    specified in ms. If the method is the initialiser, return the type
    Object instead. *)
let return_type program tenv ms meth_kind =
  if meth_kind = JContext.Init then
    get_class_type program tenv (JBasics.make_cn JConfig.object_cl)
  else
    match JBasics.ms_rtype ms with
    | None -> Sil.Tvoid
    | Some vt -> value_type program tenv vt


let add_models_types tenv =
  let add_type t typename struct_typ =
    if not (Tenv.mem t typename) then
      Tenv.add tenv typename struct_typ in
  Tenv.iter (add_type tenv) !JClasspath.models_tenv


(* TODO #6604630: remove *)
let never_returning_null =
  (* class, method name, [arg type], ret_type of methods to model as never returning null *)
  let fragment_type = "android.support.v4.app.Fragment" in
  let never_null_method_sigs =
    [
      (fragment_type, "getContext", [], "android.content.Context", Procname.Non_Static);
      (fragment_type, "getActivity", [], "android.support.v4.app.FragmentActivity", Procname.Non_Static)
    ] in
  let make_procname = function
    | (class_name, method_name, arg_types, ret_type, kind) ->
        let return_cn = JBasics.make_cn ret_type in
        let cn = JBasics.make_cn class_name
        and ms =
          JBasics.make_ms
            method_name arg_types (Some (JBasics.TObject (JBasics.TClass return_cn))) in
        get_method_procname cn ms kind in
  IList.map make_procname never_null_method_sigs
