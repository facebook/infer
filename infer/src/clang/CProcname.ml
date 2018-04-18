(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let rec get_mangled_method_name function_decl_info method_decl_info =
  (* For virtual methods return mangled name of the method from most base class
     Go recursively until there is no method in any parent class. All names
     of the same method need to be the same, otherwise dynamic dispatch won't
     work. *)
  let open Clang_ast_t in
  match method_decl_info.xmdi_overriden_methods with
  | [] ->
      function_decl_info.fdi_mangled_name
  | base1_dr :: _ ->
      let base1 =
        match CAst_utils.get_decl base1_dr.dr_decl_pointer with Some b -> b | _ -> assert false
      in
      match base1 with
      | CXXMethodDecl (_, _, _, fdi, mdi)
      | CXXConstructorDecl (_, _, _, fdi, mdi)
      | CXXConversionDecl (_, _, _, fdi, mdi)
      | CXXDestructorDecl (_, _, _, fdi, mdi) ->
          get_mangled_method_name fdi mdi
      | _ ->
          assert false


let get_template_info tenv (fdi: Clang_ast_t.function_decl_info) =
  match fdi.fdi_template_specialization with
  | Some spec_info ->
      Typ.Template
        {mangled= fdi.fdi_mangled_name; args= CType_decl.get_template_args tenv spec_info}
  | None ->
      Typ.NoTemplate


let is_decl_info_generic_model {Clang_ast_t.di_attributes} =
  let f = function
    | Clang_ast_t.AnnotateAttr {ai_parameters= [_; name; _]}
      when String.equal name "__infer_generic_model" ->
        true
    | _ ->
        false
  in
  List.exists ~f di_attributes


let mk_c_function translation_unit_context ?tenv name function_decl_info_opt =
  let file =
    match function_decl_info_opt with
    | Some (decl_info, function_decl_info) -> (
      match function_decl_info.Clang_ast_t.fdi_storage_class with
      | Some "static"
      (* when we model static functions, we cannot take the file into account to
       create a mangled name because the file of the model is different to the real file,
       thus the model won't work *)
        when not (CTrans_models.is_modelled_static_function (QualifiedCppName.to_qual_string name)) ->
          let file_opt =
            (fst decl_info.Clang_ast_t.di_source_range).Clang_ast_t.sl_file
            |> Option.map ~f:SourceFile.from_abs_path
          in
          let file_to_hex src = SourceFile.to_string src |> Utils.string_crc_hex32 in
          Option.value_map ~f:file_to_hex ~default:"" file_opt
      | _ ->
          "" )
    | None ->
        ""
  in
  let mangled_opt =
    match function_decl_info_opt with
    | Some (_, function_decl_info) ->
        function_decl_info.Clang_ast_t.fdi_mangled_name
    | _ ->
        None
  in
  let mangled_name =
    match mangled_opt with
    | Some m when CGeneral_utils.is_cpp_translation translation_unit_context ->
        m
    | _ ->
        ""
  in
  let template_info, is_generic_model =
    match (function_decl_info_opt, tenv) with
    | Some (decl_info, function_decl_info), Some t ->
        (get_template_info t function_decl_info, is_decl_info_generic_model decl_info)
    | _ ->
        (Typ.NoTemplate, false)
  in
  let mangled = file ^ mangled_name in
  if String.is_empty mangled then
    Typ.Procname.from_string_c_fun (QualifiedCppName.to_qual_string name)
  else Typ.Procname.C (Typ.Procname.c name mangled template_info ~is_generic_model)


let mk_cpp_method ?tenv class_name method_name ?meth_decl mangled =
  let open Clang_ast_t in
  let method_kind =
    match meth_decl with
    | Some (Clang_ast_t.CXXConstructorDecl (_, _, _, _, {xmdi_is_constexpr})) ->
        Typ.Procname.ObjC_Cpp.CPPConstructor {mangled; is_constexpr= xmdi_is_constexpr}
    | Some (Clang_ast_t.CXXDestructorDecl _) ->
        Typ.Procname.ObjC_Cpp.CPPDestructor {mangled}
    | _ ->
        Typ.Procname.ObjC_Cpp.CPPMethod {mangled}
  in
  let template_info, is_generic_model =
    match meth_decl with
    | Some (CXXMethodDecl (di, _, _, fdi, _))
    | Some (CXXConstructorDecl (di, _, _, fdi, _))
    | Some (CXXConversionDecl (di, _, _, fdi, _))
    | Some (CXXDestructorDecl (di, _, _, fdi, _)) ->
        let templ_info =
          match tenv with Some t -> get_template_info t fdi | None -> Typ.NoTemplate
        in
        let is_gen_model =
          is_decl_info_generic_model di
          || (* read whether parent class is annoatated as generic model *)
             di.di_parent_pointer |> Option.value_map ~f:CAst_utils.get_decl ~default:None
             |> Option.map ~f:Clang_ast_proj.get_decl_tuple
             |> Option.value_map ~f:is_decl_info_generic_model ~default:false
        in
        (templ_info, is_gen_model)
    | _ ->
        (Typ.NoTemplate, false)
  in
  Typ.Procname.ObjC_Cpp
    (Typ.Procname.ObjC_Cpp.make class_name method_name method_kind template_info ~is_generic_model)


let mk_objc_method class_typename method_name method_kind =
  Typ.Procname.ObjC_Cpp
    (Typ.Procname.ObjC_Cpp.make class_typename method_name method_kind Typ.NoTemplate
       ~is_generic_model:false)


let block_procname_with_index defining_proc i =
  Config.anonymous_block_prefix ^ Typ.Procname.to_string defining_proc
  ^ Config.anonymous_block_num_sep ^ string_of_int i


(** Global counter for anonymous block*)
let block_counter = ref 0

let reset_block_counter () = block_counter := 0

let get_fresh_block_index () =
  block_counter := !block_counter + 1 ;
  !block_counter


let mk_fresh_block_procname defining_proc =
  let name = block_procname_with_index defining_proc (get_fresh_block_index ()) in
  Typ.Procname.mangled_objc_block name


let get_class_typename ?tenv method_decl_info =
  let class_ptr = Option.value_exn method_decl_info.Clang_ast_t.di_parent_pointer in
  match CAst_utils.get_decl class_ptr with
  | Some class_decl ->
      CType_decl.get_record_typename ?tenv class_decl
  | None ->
      CFrontend_config.incorrect_assumption __POS__ method_decl_info.Clang_ast_t.di_source_range
        "Expecting class declaration when getting the class typename"


module NoAstDecl = struct
  let c_function_of_string translation_unit_context tenv name =
    let qual_name = QualifiedCppName.of_qual_string name in
    mk_c_function translation_unit_context ~tenv qual_name None


  let cpp_method_of_string tenv class_name method_name =
    mk_cpp_method ~tenv class_name method_name None


  let objc_method_of_string_kind class_name method_name method_kind =
    mk_objc_method class_name method_name method_kind
end

let objc_method_procname ?tenv decl_info method_name mdi =
  let class_typename = get_class_typename ?tenv decl_info in
  let is_instance = mdi.Clang_ast_t.omdi_is_instance_method in
  let method_kind = Typ.Procname.ObjC_Cpp.objc_method_kind_of_bool is_instance in
  mk_objc_method class_typename method_name method_kind


let from_decl translation_unit_context ?tenv meth_decl =
  let open Clang_ast_t in
  match meth_decl with
  | FunctionDecl (decl_info, name_info, _, fdi) ->
      let name = CAst_utils.get_qualified_name name_info in
      let function_info = Some (decl_info, fdi) in
      mk_c_function translation_unit_context ?tenv name function_info
  | CXXMethodDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConstructorDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConversionDecl (decl_info, name_info, _, fdi, mdi)
  | CXXDestructorDecl (decl_info, name_info, _, fdi, mdi) ->
      let mangled = get_mangled_method_name fdi mdi in
      let method_name = CAst_utils.get_unqualified_name name_info in
      let class_typename = get_class_typename ?tenv decl_info in
      mk_cpp_method ?tenv class_typename method_name ~meth_decl mangled
  | ObjCMethodDecl (decl_info, name_info, mdi) ->
      objc_method_procname ?tenv decl_info name_info.Clang_ast_t.ni_name mdi
  | BlockDecl _ ->
      let name =
        Config.anonymous_block_prefix ^ Config.anonymous_block_num_sep
        ^ string_of_int (get_fresh_block_index ())
      in
      Typ.Procname.mangled_objc_block name
  | _ ->
      Logging.die InternalError "Expected method decl, but got %s."
        (Clang_ast_proj.get_decl_kind_string meth_decl)


let from_decl_for_linters translation_unit_context method_decl =
  let open Clang_ast_t in
  match method_decl with
  | ObjCMethodDecl (decl_info, name_info, mdi) ->
      let method_name =
        match String.split ~on:':' name_info.Clang_ast_t.ni_name with
        | hd :: _ ->
            hd
        | _ ->
            name_info.Clang_ast_t.ni_name
      in
      objc_method_procname decl_info method_name mdi
  | _ ->
      from_decl translation_unit_context method_decl
