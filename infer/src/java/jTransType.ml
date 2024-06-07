(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
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

(** https://docs.oracle.com/javase/tutorial/java/nutsandbolts/datatypes.html *)
let translate_basic_type = function
  | `Int ->
      StdTyp.int
  | `Bool ->
      StdTyp.boolean
  | `Byte ->
      StdTyp.Java.byte
  | `Char ->
      StdTyp.Java.char
  | `Double ->
      StdTyp.double
  | `Float ->
      StdTyp.float
  | `Long ->
      StdTyp.long
  | `Short ->
      StdTyp.Java.short


let cast_type = function
  | JBir.F2I | JBir.L2I | JBir.D2I ->
      StdTyp.int
  | JBir.D2L | JBir.F2L | JBir.I2L ->
      StdTyp.long
  | JBir.I2F | JBir.L2F | JBir.D2F ->
      StdTyp.float
  | JBir.L2D | JBir.F2D | JBir.I2D ->
      StdTyp.double
  | JBir.I2B ->
      StdTyp.boolean
  | JBir.I2C ->
      StdTyp.Java.char
  | JBir.I2S ->
      StdTyp.Java.short


let typename_of_classname cn = Typ.Name.Java.from_string (JBasics.cn_name cn)

let rec get_named_type (vt : JBasics.value_type) =
  match vt with
  | TBasic bt ->
      translate_basic_type bt
  | TObject (TArray at) ->
      Typ.(mk_ptr (mk_array (get_named_type at)))
  | TObject (TClass cn) ->
      Typ.(mk_ptr (mk_struct (typename_of_classname cn)))


let rec create_array_type typ dim =
  if dim > 0 then
    let content_typ = create_array_type typ (dim - 1) in
    Typ.(mk_ptr (mk_array content_typ))
  else typ


(** Printing types *)
let object_type_to_string ot =
  let rec array_type_to_string (vt : JBasics.value_type) =
    let s =
      match vt with
      | TObject ot ->
          object_type_to_string' ot
      | TBasic `Bool ->
          JConfig.boolean_code
      | TBasic `Byte ->
          JConfig.byte_code
      | TBasic `Char ->
          JConfig.char_code
      | TBasic `Double ->
          JConfig.double_code
      | TBasic `Float ->
          JConfig.float_code
      | TBasic `Int ->
          JConfig.int_code
      | TBasic `Long ->
          JConfig.long_code
      | TBasic `Short ->
          JConfig.short_code
    in
    "[" ^ s
  and object_type_to_string' ot =
    match ot with
    | JBasics.TClass class_name ->
        JConfig.class_code (JBasics.cn_name class_name)
    | JBasics.TArray vt ->
        array_type_to_string vt
  in
  match ot with
  | JBasics.TClass class_name ->
      JBasics.cn_name class_name
  | JBasics.TArray vt ->
      array_type_to_string vt


let method_signature_names ms =
  let method_name = JBasics.ms_name ms in
  let return_type_name =
    match JBasics.ms_rtype ms with
    | None when String.equal method_name JConfig.constructor_name ->
        None
    | None ->
        Some StdTyp.void
    | Some vt ->
        Some (get_named_type vt)
  in
  let args_types = List.map ~f:get_named_type (JBasics.ms_args ms) in
  (return_type_name, method_name, args_types)


let create_fieldname cn fs =
  let field_name = JBasics.fs_name fs in
  let class_name = JBasics.cn_name cn in
  Fieldname.make (Typ.Name.Java.from_string class_name) field_name


let create_sil_class_field cn {Javalib.cf_signature; cf_annotations; cf_kind} =
  let field_name = create_fieldname cn cf_signature in
  let field_type = get_named_type (JBasics.fs_type cf_signature) in
  let annotation =
    let real_annotations = JAnnotation.translate_item cf_annotations in
    (* translate modifiers like "volatile" as annotations *)
    match cf_kind with
    | Javalib.Volatile ->
        Annot.volatile :: real_annotations
    | Javalib.Final ->
        Annot.final :: real_annotations
    | Javalib.NotFinal ->
        real_annotations
  in
  Struct.mk_field field_name field_type ~annot:annotation


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
  let field = Struct.mk_field field_name field_type ~annot:annotation in
  field :: l


let collect_models_class_fields classpath_field_map cn cf fields =
  let static, nonstatic = fields in
  let {Struct.name= field_name; typ= field_type; annot= annotation} =
    create_sil_class_field cn cf
  in
  match Fieldname.Map.find_opt field_name classpath_field_map with
  | Some classpath_ft when Typ.equal classpath_ft field_type ->
      fields
  | Some classpath_ft ->
      (* TODO (#6711750): fix type equality for arrays before failing here *)
      L.internal_error "Found inconsistent types for %s@\n\tclasspath: %a@\n\tmodels: %a@\n@."
        (Fieldname.to_string field_name) (Typ.pp_full Pp.text) classpath_ft (Typ.pp_full Pp.text)
        field_type ;
      fields
  | None when Javalib.is_static_field (Javalib.ClassField cf) ->
      let field = Struct.mk_field field_name field_type ~annot:annotation in
      (field :: static, nonstatic)
  | None ->
      let field = Struct.mk_field field_name field_type ~annot:annotation in
      (static, field :: nonstatic)


let add_model_fields classpath_fields cn =
  let statics, nonstatics = classpath_fields in
  let classpath_field_map =
    let collect_fields map =
      List.fold ~f:(fun map {Struct.name= fn; typ= ft} -> Fieldname.Map.add fn ft map) ~init:map
    in
    collect_fields (collect_fields Fieldname.Map.empty statics) nonstatics
  in
  try
    match JBasics.ClassMap.find cn (JModels.get_classmap ()) with
    | Javalib.JClass _ as jclass ->
        Javalib.cf_fold (collect_models_class_fields classpath_field_map cn) jclass classpath_fields
    | _ ->
        classpath_fields
  with Caml.Not_found -> classpath_fields


let get_method_kind m =
  if Javalib.is_static_method m then Procname.Java.Static else Procname.Java.Non_Static


let rec get_method_procname program tenv cn ms kind =
  let (_ : Struct.t) = get_class_struct_typ program tenv cn in
  let return_type, method_name, parameters = method_signature_names ms in
  let class_name = Typ.Name.Java.from_string (JBasics.cn_name cn) in
  Procname.make_java ~class_name ~return_type ~method_name ~parameters ~kind


(* create a mangled procname from an abstract or concrete method *)
and translate_method_name program tenv m =
  let cn, ms = JBasics.cms_split (Javalib.get_class_method_signature m) in
  let proc_name = get_method_procname program tenv cn ms (get_method_kind m) in
  JProgramDesc.add_missing_callee program proc_name cn ms ;
  proc_name


and get_all_fields program tenv cn =
  let extract_class_fields classname =
    let {Struct.fields; statics} = get_class_struct_typ program tenv classname in
    (statics, fields)
  in
  let trans_fields classname =
    match JProgramDesc.lookup_node classname program with
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
  let create_super_list program tenv interface_names =
    List.iter ~f:(fun cn -> ignore (get_class_struct_typ program tenv cn)) interface_names ;
    List.map ~f:typename_of_classname interface_names
  in
  let make_struct program tenv node supers ~fields ~statics annots ~java_class_kind name =
    let methods =
      Javalib.m_fold (fun m procnames -> translate_method_name program tenv m :: procnames) node []
    in
    let node_name = Javalib.get_name node in
    let java_location : Location.t option = JProgramDesc.get_java_location program node_name in
    ( match java_location with
    | Some loc ->
        L.debug Capture Verbose "Java location %s -> %a@." (JBasics.cn_name node_name)
          Location.pp_file_pos loc
    | None ->
        () ) ;
    let class_info : Struct.ClassInfo.t =
      JavaClassInfo {kind= java_class_kind; loc= java_location}
    in
    Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots ~class_info name
  in
  fun program tenv cn ->
    let name = typename_of_classname cn in
    match Tenv.lookup tenv name with
    | Some struct_typ ->
        struct_typ
    | None when JBasics.ClassSet.mem cn !seen ->
        Tenv.mk_struct ~dummy:true tenv name
    | None -> (
        seen := JBasics.ClassSet.add cn !seen ;
        match JProgramDesc.lookup_node cn program with
        | None ->
            Tenv.mk_struct ~dummy:true tenv name
        | Some (Javalib.JInterface jinterface as node) ->
            let statics, _ = get_all_fields program tenv cn in
            let supers = create_super_list program tenv jinterface.Javalib.i_interfaces in
            let annots = JAnnotation.translate_item jinterface.Javalib.i_annotations in
            make_struct program tenv node supers ~fields:[] ~statics annots
              ~java_class_kind:Struct.Interface name
        | Some (Javalib.JClass jclass as node) ->
            let statics, fields =
              let classpath_static, classpath_nonstatic = get_all_fields program tenv cn in
              add_model_fields (classpath_static, classpath_nonstatic) cn
            in
            let annots = JAnnotation.translate_item jclass.Javalib.c_annotations in
            let interface_list = create_super_list program tenv jclass.Javalib.c_interfaces in
            let supers =
              match jclass.Javalib.c_super_class with
              | None ->
                  interface_list (* base case of the recursion *)
              | Some super_cn ->
                  ignore (get_class_struct_typ program tenv super_cn) ;
                  let super_classname = typename_of_classname super_cn in
                  super_classname :: interface_list
            in
            let java_class_kind : Struct.java_class_kind =
              if jclass.Javalib.c_abstract then Struct.AbstractClass else Struct.NormalClass
            in
            make_struct program tenv node supers ~fields ~statics annots ~java_class_kind name )


let get_class_type_no_pointer program tenv cn =
  ignore (get_class_struct_typ program tenv cn) ;
  Typ.mk_struct (typename_of_classname cn)


let get_class_type program tenv cn = Typ.mk_ptr (get_class_type_no_pointer program tenv cn)

(** return true if [field_name] is the autogenerated C.$assertionsDisabled field for class C *)
let is_autogenerated_assert_field field_name =
  String.equal (Fieldname.get_field_name field_name) "$assertionsDisabled"


(** translate an object type *)
let rec object_type program tenv ot =
  match ot with
  | JBasics.TClass cn ->
      get_class_type program tenv cn
  | JBasics.TArray at ->
      Typ.mk_ptr (Typ.mk_array (value_type program tenv at))


(** translate a value type *)
and value_type program tenv vt =
  match vt with
  | JBasics.TBasic bt ->
      translate_basic_type bt
  | JBasics.TObject ot ->
      object_type program tenv ot


(** Translate object types into Exp.Sizeof expressions *)
let sizeof_of_object_type program tenv ot subtype =
  match (object_type program tenv ot).Typ.desc with
  | Typ.Tptr (typ, _) ->
      Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype; nullable= false}
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
      if JBir.var_equal var var' then Some (param_type program tenv context.cn var' vt') else None )
    (JBir.params context.impl)


let get_var_type context var =
  let typ_opt = JContext.get_var_type context var in
  match typ_opt with Some _ -> typ_opt | None -> get_var_type_from_sig context var


let extract_array_type typ =
  match typ.Typ.desc with Typ.Tptr ({desc= Tarray {elt}}, Typ.Pk_pointer) -> elt | _ -> typ


(** translate the type of an expression, looking in the method signature for formal parameters this
    is because variables in expressions do not have accurate types *)
let rec expr_type (context : JContext.t) expr =
  let program = context.program in
  let tenv = JContext.get_tenv context in
  match expr with
  | JBir.Var (vt, var) -> (
    match get_var_type context var with Some typ -> typ | None -> value_type program tenv vt )
  | JBir.Binop (JBir.ArrayLoad _, e1, _) ->
      let typ = expr_type context e1 in
      extract_array_type typ
  | _ ->
      value_type program tenv (JBir.type_of_expr expr)


(** Returns the return type of the method based on the return type specified in ms. *)
let return_type program tenv ms =
  match JBasics.ms_rtype ms with None -> StdTyp.void | Some vt -> value_type program tenv vt
