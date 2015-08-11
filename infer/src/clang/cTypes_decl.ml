(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Processes types and record declarations by adding them to the tenv *)

open Utils
open CFrontend_utils

module L = Logging
exception Typename_not_found

(* Adds the predefined types objc_class which is a struct, and Class, *)
(* which is a pointer to objc_class. *)
let add_predefined_types tenv =
  let objc_class_mangled = Mangled.from_string CFrontend_config.objc_class in
  let objc_class_name = Sil.TN_csu (Sil.Class, objc_class_mangled) in
  let objc_class_type_info =
    Sil.Tstruct ([], [], Sil.Struct,
                 Some (Mangled.from_string CFrontend_config.objc_class), [], [], []) in
  Sil.tenv_add tenv objc_class_name objc_class_type_info;
  let mn = Mangled.from_string CFrontend_config.class_type in
  let class_typename = Sil.TN_typedef(mn) in
  let class_typ = Sil.Tptr ((Sil.Tvar
                               (Sil.TN_csu (Sil.Struct, objc_class_mangled))), Sil.Pk_pointer) in
  Sil.tenv_add tenv class_typename class_typ;
  let typename_objc_object =
    Sil.TN_csu (Sil.Struct, Mangled.from_string CFrontend_config.objc_object) in
  let id_typedef = Sil.Tptr (Sil.Tvar (typename_objc_object), Sil.Pk_pointer) in
  let id_typename = Sil.TN_typedef (Mangled.from_string CFrontend_config.id_cl) in
  Sil.tenv_add tenv id_typename id_typedef

let rec search_for_named_type tenv typ =
  let search typename =
    match typename with
    | Sil.TN_typedef name ->
        (match Sil.tenv_lookup tenv typename with
         | Some _ -> typename
         | None ->
             let pot_class_type = Sil.TN_csu (Sil.Class, name) in
             match Sil.tenv_lookup tenv pot_class_type with
             | Some _ -> pot_class_type
             | None ->
                 let pot_protocol_type = Sil.TN_csu (Sil.Protocol, name) in
                 match Sil.tenv_lookup tenv pot_protocol_type with
                 | Some _ -> pot_protocol_type
                 | None ->
                     let pot_struct_type = Sil.TN_csu (Sil.Struct, name) in
                     match Sil.tenv_lookup tenv pot_struct_type with
                     | Some _ -> pot_struct_type
                     | None ->
                         let pot_union_type = Sil.TN_csu (Sil.Union, name) in
                         match Sil.tenv_lookup tenv pot_union_type with
                         | Some _ -> pot_union_type
                         | None -> raise Typename_not_found)
    | _ -> typename in
  match typ with
  | Sil.Tvar typename -> Sil.Tvar (search typename)
  | Sil.Tptr (typ, p) ->
      Sil.Tptr (search_for_named_type tenv typ, p)
  | _ -> typ

(* Type representation is string in the clang ast. We use a parser for     *)
(* parsing and then translating the type The parser is higher-order and    *)
(* takes a tenv as needs to do look-ups                                    *)
let string_type_to_sil_type tenv s =
  Printing.log_out "    ...Trying parsing TYPE from string: '%s'@." s;
  if s = "" then (
    Printing.log_stats "\n  Empty string parsed as type Void.\n";
    Sil.Tvoid)
  else (
    (* NOTE sometimes we need to remove an extra occurrence of the word*)
    (* "struct" or "union" used in RecordDecl raw type but not in VarDecl or other*)
    (* raw types. This inconsistence gives problems when looking up tenv.*)
    (* To overcome that we remove consistently this extra "union"/"struct" everytyme *)
    (* we translate a type.*)
    (* Example: 'union <anonymous union at union.c:4:1>' will be *)
    (* 'union <anonymous at union.c:4:1>'*)
    let s = (match Str.split (Str.regexp "[ \t]+") s with
        | "struct"::"(anonymous":: "struct":: s' ->
            (*Printing.log_out "    ...Getting rid of the extra 'struct' word@."; *)
            General_utils.string_from_list ("struct"::"(anonymous":: s')
        | "union"::"(anonymous":: "union":: s' ->
            (*Printing.log_out "    ...Getting rid of the extra 'union' word@."; *)
            General_utils.string_from_list ("union"::"(anonymous":: s')
        | _ -> s) in
    let lexbuf = Lexing.from_string s in
    let t =
      try
        let t = CTypes_parser.parse (Ast_lexer.token) lexbuf in
        Printing.log_out
          "    ...Parsed. Translated with sil TYPE '%a'@." (Sil.pp_typ_full pe_text) t;
        t
      with Parsing.Parse_error -> (
          Printing.log_stats
            "\nXXXXXXX PARSE ERROR for string '%s'. RETURNING Void.TODO@.@." s;
          Sil.Tvoid) in
    try
      search_for_named_type tenv t
    with Typename_not_found -> Printing.log_stats
                                 "\nXXXXXX Parsed string '%s' as UNKNOWN type name. RETURNING a type name.TODO@.@." s;
      t)

let qual_type_to_sil_type_no_expansions tenv qt =
  string_type_to_sil_type tenv (CTypes.get_type qt)

let opt_type_to_sil_type tenv opt_type =
  match opt_type with
  | `Type(s) -> qual_type_to_sil_type_no_expansions tenv (Ast_expressions.create_qual_type s)
  | `NoType -> Sil.Tvoid

let parse_func_type name func_type =
  try
    let lexbuf = Lexing.from_string func_type in
    let (return_type, arg_types) = CTypes_parser.clang_func_type (Ast_lexer.token) lexbuf in
    let arg_types =
      match arg_types with
      | [Sil.Tvoid] -> []
      | _ -> arg_types in
    Printing.log_out
      "    ...Parsed. Translated with sil return type '%s' @."
      ((Sil.typ_to_string return_type)^" <- "^(Utils.list_to_string (Sil.typ_to_string) arg_types));
    Some (return_type, arg_types)
  with Parsing.Parse_error -> (
      Printing.log_stats "\nXXXXXXX PARSE ERROR for string '%s'." func_type;
      None)


(* We need to take the name out of the type as the struct can be anonymous*)
let get_record_name opt_type = match opt_type with
  | `Type n' -> CTypes.cut_struct_union n'
  | `NoType -> assert false


let get_method_decls parent decl_list =
  let open Clang_ast_t in
  let rec traverse_decl parent decl = match decl with
    | CXXMethodDecl _ -> [(parent, decl)]
    | CXXRecordDecl (_, _, _, _, decl_list', _, _, _)
    | RecordDecl (_, _, _, _, decl_list', _, _) -> traverse_decl_list decl decl_list'
    | _ -> []
  and traverse_decl_list parent decl_list = list_flatten (list_map (traverse_decl parent) decl_list)  in
  traverse_decl_list parent decl_list

(*In case of typedef like *)
(* typedef struct { f1; f2; ... } s; *)
(* the AST-dump splits the typedef definition from the struct definition. *)
(* The type in the typedef "s" will be "s" and this become detached from the struct definition.*)
(* To avoid circular entry in tenv, we disambiguate this case.*)
(* We check if in tenv there is a "strucs s" defined and we make the type def "s" *)
(* point directly to "struct s" *)
let rec disambiguate_typedef tenv namespace t mn =
  match t with
  | Sil.Tvar(Sil.TN_typedef mn') ->
      if (Mangled.equal mn mn') then
        (* This will give a circularity in the definition of typedef in the tenv. *)
        (* Eg. TN_typdef(mn) --> TN_typedef(mn). We need to break it*)
        let tn = Sil.TN_csu(Sil.Struct, mn) in
        (match Sil.tenv_lookup tenv tn with
         | Some _ ->
             (* There is a struct in tenv, so we make the typedef mn pointing to the struct*)
             Printing.log_out "   ...Found type TN_typdef('%s') " (Mangled.to_string mn);
             Printing.log_out "in typedef of '%s'@." (Mangled.to_string mn);
             Printing.log_out
               "Avoid circular definition in tenv by pointing the typedef to struc TN_csu('%s')@."
               (Mangled.to_string mn);
             Sil.Tvar(tn)
         | None ->
             if add_late_defined_record tenv namespace tn then
               disambiguate_typedef tenv namespace t mn
             else t)
      else t
  | _ -> t

and do_typedef_declaration tenv namespace decl_info name opt_type typedef_decl_info =
  if name = CFrontend_config.class_type || name = CFrontend_config.id_cl then ()
  else
    let ns_suffix = Ast_utils.namespace_to_string namespace in
    let name = ns_suffix^name in
    let mn = Mangled.from_string name in
    let typename = Sil.TN_typedef(mn) in
    let t = opt_type_to_sil_type tenv opt_type in
    (* check for ambiguities in typedef that may create circularities in tenv*)
    let typ = disambiguate_typedef tenv namespace t mn in
    Printing.log_out "ADDING: TypedefDecl for '%s'" name;
    Printing.log_out " with type '%s'\n" (Sil.typ_to_string typ);
    Printing.log_out "  ...Adding entry to tenv with Typename TN_typedef =  '%s'\n"
      (Sil.typename_to_string typename);
    Sil.tenv_add tenv typename typ

and get_struct_fields tenv record_name namespace decl_list =
  let open Clang_ast_t in
  match decl_list with
  | [] -> []
  | FieldDecl(decl_info, name_info, qual_type, field_decl_info):: decl_list' ->
      let field_name = name_info.Clang_ast_t.ni_name in
      Printing.log_out "  ...Defining field '%s'.\n" field_name;
      let id = General_utils.mk_class_field_name record_name field_name in
      let typ = qual_type_to_sil_type tenv qual_type in
      let annotation_items = [] in (* For the moment we don't use them*)
      (id, typ, annotation_items):: get_struct_fields tenv record_name namespace decl_list'
  | CXXRecordDecl (decl_info, name, opt_type, _, decl_list, decl_context_info, record_decl_info, _)
    :: decl_list'
  (* C++/C Records treated in the same way*)
  | RecordDecl (decl_info, name, opt_type, _, decl_list, decl_context_info, record_decl_info)
    :: decl_list'->
      do_record_declaration tenv namespace decl_info name.Clang_ast_t.ni_name opt_type decl_list decl_context_info record_decl_info;
      get_struct_fields tenv record_name namespace decl_list'
  | _ :: decl_list' -> get_struct_fields tenv record_name namespace decl_list'

and get_class_methods tenv class_name namespace decl_list =
  let process_method_decl = function
    | Clang_ast_t.CXXMethodDecl (decl_info, name_info, qual_type, function_decl_info) ->
        let method_name = name_info.Clang_ast_t.ni_name in
        Printing.log_out "  ...Declaring method '%s'.\n" method_name;
        let method_proc = General_utils.mk_procname_from_cpp_method class_name method_name  (CTypes.get_type qual_type) in
        Some method_proc
    | _ -> None in
  (* poor mans list_filter_map *)
  list_flatten_options (list_map process_method_decl decl_list)

and do_record_declaration tenv namespace decl_info name opt_type decl_list decl_context_info record_decl_info =
  Printing.log_out "ADDING: RecordDecl for '%s'" name;
  Printing.log_out " pointer= '%s'\n" decl_info.Clang_ast_t.di_pointer;
  if not record_decl_info.Clang_ast_t.rdi_is_complete_definition then
    Printing.log_err "   ...Warning, definition incomplete. The full definition will probably be later \n@.";
  let typ = get_declaration_type tenv namespace decl_info name opt_type decl_list decl_context_info record_decl_info in
  let typ = expand_structured_type tenv typ in
  add_struct_to_tenv tenv typ

(* For a record declaration it returns/constructs the type *)
and get_declaration_type tenv namespace decl_info n opt_type decl_list decl_context_info record_decl_info =
  let ns_suffix = Ast_utils.namespace_to_string namespace in
  let n = ns_suffix^n in
  let name_str = get_record_name opt_type in
  Printing.log_out "Record Declaration '%s' defined as struct\n" n;
  let non_static_fields = get_struct_fields tenv name_str namespace decl_list in
  let non_static_fields = if CTrans_models.is_objc_memory_model_controlled n then
      General_utils.append_no_duplicates_fields [Sil.objc_ref_counter_field] non_static_fields
    else non_static_fields in
  let non_static_fields = CFrontend_utils.General_utils.sort_fields non_static_fields in
  let static_fields = [] in (* Warning for the moment we do not treat static field. *)
  let typ = (match opt_type with
      | `Type s -> qual_type_to_sil_type_no_expansions tenv (Ast_expressions.create_qual_type s)
      | _ -> assert false) in
  let csu = (match typ with
      | Sil.Tvar (Sil.TN_csu (csu, _)) -> csu
      | _ -> Sil.Struct) in
  let name = Some (Mangled.from_string name_str) in
  let methods_list = get_class_methods tenv name_str namespace decl_list in (* C++ methods only *)
  let superclass_list = [] in (* No super class for structs *)
  let item_annotation = Sil.item_annotation_empty in  (* No annotations for struts *)
  Sil.Tstruct
    (non_static_fields, static_fields, csu, name, superclass_list, methods_list, item_annotation)

(* Look for a record definition that is defined after it is dereferenced. *)
(* It returns true if a new record definition has been added to tenv.*)
and add_late_defined_record tenv namespace typename =
  Printing.log_out "!!!! Calling late-defined record '%s'\n" (Sil.typename_to_string typename) ;
  match typename with
  | Sil.TN_csu(Sil.Struct, name) | Sil.TN_csu(Sil.Union, name) ->
      let open Clang_ast_t in
      let rec scan decls =
        match decls with
        | [] -> false
        | CXXRecordDecl
            (decl_info, record_name, opt_type, _, decl_list, decl_context_info, record_decl_info, _)
          :: decls'
        | RecordDecl
            (decl_info, record_name, opt_type, _, decl_list, decl_context_info, record_decl_info)
          :: decls' ->
            (match opt_type with
             | `Type t ->
                 (* the string t contains the name of the type preceded by the word struct. *)
                 let t_no_struct = CTypes.cut_struct_union t in
                 let pot_struct_type = Sil.TN_csu (Sil.Struct, (Mangled.from_string t_no_struct)) in
                 let pot_union_type = Sil.TN_csu (Sil.Union, (Mangled.from_string t_no_struct)) in
                 if (Sil.typename_equal typename pot_struct_type ||
                     Sil.typename_equal typename pot_union_type) &&
                    record_decl_info.Clang_ast_t.rdi_is_complete_definition then (
                   Printing.log_out "!!!! Adding late-defined record '%s'\n" t;
                   do_record_declaration tenv namespace decl_info record_name.Clang_ast_t.ni_name opt_type decl_list
                     decl_context_info record_decl_info;
                   true)
                 else scan decls'
             | _ -> scan decls')
        | LinkageSpecDecl(_, decl_list', _):: decls' -> scan (decl_list'@decls')
        | _:: decls' -> scan decls' in
      scan !CFrontend_config.global_translation_unit_decls
  | _ -> false

(* Look for a typedef definition that is defined after it is used. *)
(* It returns true if a new typedef definition has been added to tenv.*)
and add_late_defined_typedef tenv namespace typename =
  Printing.log_out "Calling late-defined typedef '%s'\n" (Sil.typename_to_string typename);
  match typename with
  | Sil.TN_typedef name ->
      let rec scan decls =
        let open Clang_ast_t in
        match decls with
        | [] -> false
        | TypedefDecl (decl_info, name_info, opt_type, _, tdi) :: decls' ->
            let name' = name_info.Clang_ast_t.ni_name in
            (match opt_type with
             | `Type t ->
                 if (Mangled.to_string name) = name' then (
                   Printing.log_out "!!!! Adding late-defined typedef '%s'\n" t;
                   do_typedef_declaration tenv namespace decl_info name' opt_type tdi;
                   true)
                 else scan decls'
             | _ -> scan decls')
        | LinkageSpecDecl(_, decl_list', _):: decls' -> scan (decl_list'@decls')
        | _:: decls' -> scan decls' in
      scan !CFrontend_config.global_translation_unit_decls
  | _ -> false

(* Expand a named type Tvar if it has a definition in tenv. This is used for Tenum, Tstruct, etc. *)
and expand_structured_type tenv typ =
  match typ with
  | Sil.Tvar tn ->
      (match Sil.tenv_lookup tenv tn with
       | Some t ->
           Printing.log_out
             "   Type expanded with type '%s' found in tenv@." (Sil.typ_to_string t);
           if Sil.typ_equal t typ then
             typ
           else expand_structured_type tenv t
       | None -> if (add_late_defined_record tenv None tn ||
                     add_late_defined_typedef tenv None tn) then
             expand_structured_type tenv typ
           else typ)
  | Sil.Tptr(t, _) -> typ (*do not expand types under pointers *)
  | _ -> typ

and add_struct_to_tenv tenv typ =
  let typ = expand_structured_type tenv typ in
  let csu = match typ with
    | Sil.Tstruct(_, _, csu, _, _, _, _) -> csu
    | _ -> assert false in
  let mangled = CTypes.get_name_from_struct typ in
  let typename = Sil.TN_csu(csu, mangled) in
  Printing.log_out "  >>>Adding struct to tenv  mangled='%s'\n" (Mangled.to_string mangled);
  Printing.log_out "  >>>Adding struct to tenv typ='%s'\n" (Sil.typ_to_string typ);
  Printing.log_out "  >>>with Key Typename TN_csu('%s')\n" (Sil.typename_to_string typename);
  Printing.log_out "  >>>Adding entry to tenv ('%s'," (Sil.typename_to_string typename);
  Printing.log_out "'%s')\n" (Sil.typ_to_string typ);
  Sil.tenv_add tenv typename typ;
  Printing.log_out "  >>>Verifying that Typename TN_csu('%s') is in tenv\n"
    (Sil.typename_to_string typename);
  (match Sil.tenv_lookup tenv typename with
   | Some t -> Printing.log_out "  >>>OK. Found typ='%s'\n" (Sil.typ_to_string t)
   | None -> Printing.log_out "  >>>NOT Found!!\n")

and qual_type_to_sil_type_general tenv qt no_pointer =
  let typ = string_type_to_sil_type tenv (CTypes.get_type qt) in
  match typ with
  | Sil.Tptr(np_typ, _) when no_pointer ->
      expand_structured_type tenv np_typ
  | _ -> expand_structured_type tenv typ

(* Translate a qual_type from clang to sil type. It uses the raw field     *)
(* (rather than desugared)                                                 *)
and qual_type_to_sil_type tenv qt =
  qual_type_to_sil_type_general tenv qt false

and qual_type_to_sil_type_np tenv qt =
  qual_type_to_sil_type_general tenv qt true

and type_name_to_sil_type tenv name =
  qual_type_to_sil_type_general tenv (Ast_expressions.create_qual_type name) false

let get_type_from_expr_info ei tenv =
  let qt = ei.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt

let class_from_pointer_type tenv qual_type =
  match qual_type_to_sil_type tenv qual_type with
  | Sil.Tptr( Sil.Tvar (Sil.TN_typedef name), _) -> Mangled.to_string name
  | Sil.Tptr( Sil.Tvar (Sil.TN_csu (_, name)), _) -> Mangled.to_string name
  | _ -> assert false

let get_class_type_np tenv expr_info obj_c_message_expr_info =
  let qt =
    match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
    | `Class qt -> qt
    | _ -> expr_info.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt

let extract_sil_type_from_stmt tenv s =
  let qt = CTypes.extract_type_from_stmt s in
  qual_type_to_sil_type tenv qt

let get_type_curr_class tenv curr_class_opt =
  let name = CContext.get_curr_class_name curr_class_opt in
  let typ = Sil.Tvar (Sil.TN_csu (Sil.Class, (Mangled.from_string name))) in
  expand_structured_type tenv typ
