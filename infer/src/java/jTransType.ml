open Javalib_pack
open Sawja_pack


open Utils

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
  | `String str -> (JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.string_cl)))
  | `Class cl -> (JBasics.TObject (JBasics.TClass (JBasics.make_cn JConfig.class_cl)))
  | `Double _ -> (JBasics.TBasic `Double)
  | `Int _ -> (JBasics.TBasic`Int)
  | `Float _ -> (JBasics.TBasic`Float)
  | `Long _ -> (JBasics.TBasic`Long)
  | `ANull -> JConfig.obj_type


let typename_of_classname cn =
  Sil.TN_csu (Sil.Class, (Mangled.from_string (JBasics.cn_name cn)))


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
  | Sil.Tptr (Sil.Tstruct (_, _, Sil.Class, Some classname, _, _, _), Sil.Pk_pointer) ->
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


let rec vt_to_java_type vt =
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
  Procname.mangled_java class_name return_type_name method_name args_type_name kind

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
let collect_class_field static cn cf l =
  if Javalib.is_static_field (Javalib.ClassField cf) <> static then l
  else (create_sil_class_field cn cf) :: l


(** Collect an interface field. *)
let collect_interface_field cn inf l =
  let fs = inf.Javalib.if_signature in
  let field_type = get_named_type (JBasics.fs_type fs) in
  let field_name = create_fieldname cn fs in
  let annotation = JAnnotation.translate_item inf.Javalib.if_annotations in
  (field_name, field_type, annotation) :: l


let get_all_fields program static cn =
  let compare (name1, _, _) (name2, _, _) =
    Ident.fieldname_compare name1 name2 in
  let rec loop classname =
    match JClasspath.lookup_node classname program with
    | Some (Javalib.JClass jclass) ->
        let super_fields =
          match jclass.Javalib.c_super_class with
          | None -> []
          | Some super_classname -> loop super_classname in
        let current_fields =
          Javalib.cf_fold (collect_class_field static classname) (Javalib.JClass jclass) [] in
        (list_sort compare current_fields) @ super_fields
    | Some (Javalib.JInterface jinterface) when static ->
        let current_fields =
          Javalib.if_fold (collect_interface_field classname) (Javalib.JInterface jinterface) [] in
        list_sort compare current_fields
    | _ -> [] in
  loop cn


let dummy_type cn =
  let classname = Mangled.from_string (JBasics.cn_name cn) in
  Sil.Tstruct ([], [], Sil.Class, Some classname, [], [], Sil.item_annotation_empty)


let collect_models_class_fields classpath_field_map static cn cf l =
  if Javalib.is_static_field (Javalib.ClassField cf) <> static then l
  else
    let (field_name, field_type, annotation) = create_sil_class_field cn cf in
    try
      let classpath_ft = Ident.FieldMap.find field_name classpath_field_map in
      if Sil.typ_equal classpath_ft field_type then l
      else
        (* TODO (#6711750): fix type equality for arrays before failing here *)
        let () = Logging.stderr "Found inconsistent types for %s\n\tclasspath: %a\n\tmodels: %a\n@."
            (Ident.fieldname_to_string field_name)
            (Sil.pp_typ_full pe_text) classpath_ft
            (Sil.pp_typ_full pe_text) field_type in l
    with Not_found ->
        (field_name, field_type, annotation):: l


let add_model_fields program (static_fields, nonstatic_fields) cn =
  let collect_fields =
    list_fold_left (fun map (fn, ft, _) -> Ident.FieldMap.add fn ft map) Ident.FieldMap.empty in
  try
    match JBasics.ClassMap.find cn (JClasspath.get_models program) with
    | Javalib.JClass _ as jclass ->
        let updated_static_fields =
          Javalib.cf_fold
            (collect_models_class_fields (collect_fields static_fields) true cn)
            jclass
            static_fields
        and updated_nonstatic_fields =
          Javalib.cf_fold
            (collect_models_class_fields (collect_fields nonstatic_fields) false cn)
            jclass
            nonstatic_fields in
        (updated_static_fields, updated_nonstatic_fields)
    | _ -> (static_fields, nonstatic_fields)
  with Not_found -> (static_fields, nonstatic_fields)


let rec create_sil_type program tenv cn =
  match JClasspath.lookup_node cn program with
  | None -> dummy_type cn
  | Some node ->
      let create_super_list interface_names =
        (list_map (fun i -> Mangled.from_string (JBasics.cn_name i)) interface_names) in
      let (super_list, nonstatic_fields, static_fields, item_annotation) =
        match node with
        | Javalib.JInterface jinterface ->
            let static_fields = get_all_fields program true cn in
            let sil_interface_list = list_map (fun c -> (Sil.Class, c)) (create_super_list jinterface.Javalib.i_interfaces) in
            let item_annotation = JAnnotation.translate_item jinterface.Javalib.i_annotations in
            (sil_interface_list, [], static_fields, item_annotation)
        | Javalib.JClass jclass ->
        (* TODO: create two functions to get static fields and non-static ones *)
            let static_fields, nonstatic_fields =
              let classpath_static_fields = get_all_fields program true cn
              and classpath_nonstatic_fields = get_all_fields program false cn in
              add_model_fields program (classpath_static_fields, classpath_nonstatic_fields) cn in
            let item_annotation = JAnnotation.translate_item jclass.Javalib.c_annotations in
            let interface_list = create_super_list jclass.Javalib.c_interfaces in
            let super_classname_list =
              match jclass.Javalib.c_super_class with
              | None -> interface_list (* base case of the recursion *)
              | Some super_cn ->
                  let super_classname =
                    match get_class_type_no_pointer program tenv super_cn with
                    | Sil.Tstruct (_, _, _, Some classname, _, _, _) -> classname
                    | _ -> assert false in
                  super_classname :: interface_list in
            let super_sil_classname_list =
              list_map (fun c -> (Sil.Class, c)) super_classname_list in
            (super_sil_classname_list, nonstatic_fields, static_fields, item_annotation) in
      let classname = Mangled.from_string (JBasics.cn_name cn) in
      let method_procnames = get_class_procnames cn node in
      Sil.Tstruct (nonstatic_fields, static_fields, Sil.Class, Some classname, super_list, method_procnames, item_annotation)

and get_class_type_no_pointer program tenv cn =
  let named_type = typename_of_classname cn in
  let class_type_np =
    match Sil.tenv_lookup tenv named_type with
    | None -> create_sil_type program tenv cn
    | Some t -> t in
  Sil.tenv_add tenv named_type class_type_np;
  class_type_np


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
  let implements t = Prover.check_subtype tenv typ t in
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
  if (JBir.var_name_g name) = JConfig.this
  then get_class_type program tenv cn
  else value_type program tenv vt


let get_var_type_from_sig context var =
  let program = JContext.get_program context in
  try
    let tenv = JContext.get_tenv context in
    let vt', var' =
      list_find
        (fun (vt', var') -> JBir.var_equal var var')
        (JBir.params (JContext.get_impl context)) in
    Some (param_type program tenv (JContext.get_cn context) var' vt')
  with Not_found -> None


let get_var_type context var =
  let typ_opt = JContext.get_var_type context var in
  match typ_opt with
  | Some atype -> typ_opt
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
  | JBir.Binop ((JBir.ArrayLoad typ), e1, e2) ->
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


let update_tenv tenv program =
  let add cn _ =
    let class_typename = typename_of_classname cn in
    Sil.tenv_add tenv class_typename (create_sil_type program tenv cn) in
  JBasics.ClassMap.iter add (JClasspath.get_classmap program)


(* Update a type environment with the types found in the classpath *)
let saturate_tenv_with_classpath classpath tenv =
  let jar_tenv_filename =
    let root = Filename.concat Config.default_in_zip_results_dir Config.captured_dir_name in
    Filename.concat root Config.global_tenv_filename in
  let temp_tenv_filename =
    DB.filename_from_string (Filename.temp_file "tmp_" Config.global_tenv_filename) in
  let typename_of_classname classname =
    Sil.TN_csu (Sil.Class, classname) in
  let rec is_useful_subtype jar_tenv = function
    | Sil.TN_csu (Sil.Class, classname) when
    Mangled.equal classname JConfig.java_lang_object_classname -> false
    | typename when Sil.tenv_mem tenv typename -> true
    | typename ->
        begin
          match Sil.tenv_lookup jar_tenv typename with
          | None
          | Some (Sil.Tstruct (_, _, _, _, [], _, _)) -> false
          | Some (Sil.Tstruct (_, _, _, _, supers, _, _)) ->
              list_exists
                (is_useful_subtype jar_tenv)
                (list_map (fun (_, c) -> typename_of_classname c) supers)
          | _ -> assert false
        end in
  let transfer_type jar_tenv typename typ =
    if not (Sil.tenv_mem tenv typename) then
      if is_useful_subtype jar_tenv typename then
        Sil.tenv_add tenv typename typ in
  let extract_tenv zip_channel =
    try
      let entry = Zip.find_entry zip_channel jar_tenv_filename in
      let temp_tenv_file = DB.filename_to_string temp_tenv_filename in
      let () = Zip.copy_entry_to_file zip_channel entry temp_tenv_file in
      match Sil.load_tenv_from_file temp_tenv_filename with
      | None -> None
      | Some jar_tenv -> Some jar_tenv
    with Not_found -> None in
  let update path =
    if not (Filename.check_suffix path ".jar") then ()
    else
      let zip_channel = Zip.open_in path in
      match extract_tenv zip_channel with
      | None -> ()
      | Some jar_tenv -> Sil.tenv_iter (transfer_type jar_tenv) jar_tenv;
          Zip.close_in zip_channel in
  let paths =
    let l = JClasspath.split_classpath classpath in
    if !JClasspath.models_jar = "" then l
    else !JClasspath.models_jar :: l in
  list_iter update paths;
  DB.file_remove temp_tenv_filename


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
  list_map make_procname never_null_method_sigs
