(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Process properties by creating their getters and setters in the case that they need to be syntethized *)
(** or in the case of dynamic. *)
(* How it works: *)
(* - First, the property is defined in the interface. Then, we add the method declarations of the getter *)
(* and setter to the map property_table. *)
(* - Second, in the class implementation, if synthetize is available, create the getters and setters, *)
(* unless some of these methods has already been created before. *)

open Utils
open CFrontend_utils
open CFrontend_config
open Clang_ast_t
module L = Logging

open CContext

type prop_getter_setter = string * (Clang_ast_t.decl * bool) option

(** For each property, we save the getter and the setter method declarations (no implementation). *)
(** A property type is a tuple: *)
(** (qual_type, property attributes, decl_info, (getter_name, getter), (setter_name, setter), ivar name) *)
type property_type = Clang_ast_t.qual_type * Clang_ast_t.property_attribute list *
  Clang_ast_t.decl_info * prop_getter_setter * prop_getter_setter * string option

(** A table that record the property defined in the interface and its getter/setter. *)
(** This info used later on in the implementation if the getter/setter need to automatically *)
(** synthesized*)
module type PropertySig =
sig

  type t

  type property_key = (CContext.curr_class * string)

  val property_key_to_string : property_key -> string

  val reset_property_table: unit -> unit

  val find_property : CContext.curr_class -> string -> property_type

  val find_properties_class : CContext.curr_class -> (string * property_type) list

  val is_mem_property : property_key -> bool

  val replace_property : property_key -> property_type -> unit

  val add_property : property_key -> Clang_ast_t.qual_type ->
  Clang_ast_t.property_attribute list -> Clang_ast_t.decl_info -> unit

  val print_property_table : unit -> unit

  val find_property_name_from_ivar : CContext.curr_class -> string -> string option
end
module Property: PropertySig =
struct

  type property_key = (CContext.curr_class * string)

  let property_key_to_string (curr_class, property_name) =
    ((CContext.curr_class_to_string curr_class)^"-"^property_name)

  (** Hash table to implement error logs *)
  module PropertyTableHash = Hashtbl.Make (struct

      type t = property_key

      let hash (curr_class, pname) = (CContext.curr_class_hash curr_class) + Hashtbl.hash pname

      let equal (curr_class1, property_name1) (curr_class2, property_name2) =
        CContext.curr_class_equal curr_class1 curr_class2 &&
        (String.compare property_name1 property_name2 == 0)

    end)

  type t = property_type PropertyTableHash.t

  let property_table : t = PropertyTableHash.create 100

  let reset_property_table () =
    PropertyTableHash.reset property_table

  let rec find_property curr_class property_name =
    try PropertyTableHash.find property_table (curr_class, property_name)
    with Not_found ->
        match curr_class with
        | ContextCls (name, _, protocols) ->
            let res_opt = list_fold_right
                (fun protocol found_procname_opt ->
                      match found_procname_opt with
                      | Some found_procname -> Some found_procname
                      | None ->
                          Some (find_property (ContextProtocol protocol) property_name)) protocols None in
            (match res_opt with
              | Some res -> res
              | None -> raise Not_found)
        | _ -> raise Not_found

  let find_property_name_from_ivar curr_class ivar =
    let res = ref None in
    PropertyTableHash.iter (fun (cl, pname) (_, _, _, _, _, ivar') ->
            match ivar' with
            | Some s when (CContext.curr_class_equal curr_class  cl) && s = ivar -> res:= Some pname
            | _ -> ()) property_table;
    !res

  let is_mem_property property =
    PropertyTableHash.mem property_table property

  let replace_property property =
    PropertyTableHash.replace property_table property

  let print_property_table () =
    let print_item key (qt, attributes, decl_info, getter, setter, ivar) =
      let getter_str =
        match getter with
        | getter_name, Some (ObjCMethodDecl(_, _, _), defined1) ->
            getter_name
        | _ -> "" in
      let setter_str = match setter with
        | setter_name, Some (ObjCMethodDecl(_, _, _), defined2) ->
            setter_name
        | _ -> "" in
      Logging.out "Property item %s accessors %s and %s \n"
        (property_key_to_string key) getter_str setter_str in
    PropertyTableHash.iter print_item property_table

  let find_properties_class curr_class =
    let find_properties (curr_class', property_name) property_type properties =
      if (CContext.curr_class_equal curr_class' curr_class) then
        (property_name, property_type):: properties
      else properties in
    PropertyTableHash.fold find_properties property_table []

  let get_getter_name prop_name attributes =
    match Ast_utils.getter_attribute_opt attributes with
    | Some name -> name
    | None -> prop_name

  let get_setter_name prop_name attributes =
    match Ast_utils.setter_attribute_opt attributes with
    | Some name -> name
    | None -> "set"^(String.capitalize prop_name)^":"

  let add_property (curr_class, property_name) qt attributes decl_info =
    let key = (curr_class, property_name) in
    let getter_name = get_getter_name property_name attributes in
    let setter_name = get_setter_name property_name attributes in
    Printing.log_out "  ...Using '%s' in property table\n" (property_key_to_string key);
    PropertyTableHash.add property_table key
      (qt, attributes, decl_info, (getter_name, None), (setter_name, None), None)
end

let reset_property_table = Property.reset_property_table

let print_property_table () = Property.print_property_table ()

let find_properties_class = Property.find_properties_class

let get_ivarname_property pidi =
  match pidi.Clang_ast_t.opidi_ivar_decl with
  | Some dr -> (match dr.Clang_ast_t.dr_name with
        | Some n -> n.Clang_ast_t.ni_name
        | _ -> assert false)
  | _ -> (* If ivar is not defined than we need to take the name of the property to define ivar*)
      Ast_utils.property_name pidi

let upgrade_property_accessor property_key property_type meth_decl new_defined is_getter =
  let is_defined meth_decl =
    match meth_decl with
    | Some (method_decl, was_defined) -> new_defined || was_defined
    | None -> new_defined in
  match property_type with
  | qt, attributes, decl_info, (gname, getter), (sname, setter), ivar ->
      if is_getter then
        let defined = is_defined getter in
        Property.replace_property property_key
          (qt, attributes, decl_info, (gname, Some (meth_decl, defined)), (sname, setter), ivar)
      else let defined = is_defined setter in
        Property.replace_property property_key
          (qt, attributes, decl_info, (gname, getter), (sname, Some (meth_decl, defined)), ivar)

let check_for_property curr_class method_name meth_decl body =
  let defined = Option.is_some body in
  let properties_class = find_properties_class curr_class in
  let check_property_accessor curr_class method_name is_getter =
    let method_is_getter (property_name, property_type) =
      match property_type with (_, _, _, (getter_name, _), (setter_name, _), _) ->
          let found =
            if is_getter then (method_name = getter_name)
            else (method_name = setter_name) in
          if found then
            (Printing.log_out "  Found property  '%s'  defined in property table\n"
                (Property.property_key_to_string (curr_class, property_name));
              upgrade_property_accessor
                (curr_class, property_name) property_type meth_decl defined is_getter) in
    list_iter method_is_getter properties_class in
  check_property_accessor curr_class method_name true;
  check_property_accessor curr_class method_name false

let prepare_dynamic_property curr_class decl_info property_impl_decl_info =
  let pname = Ast_utils.property_name property_impl_decl_info in
  let qt = (try
      let qt', atts, di, getter, setter, _ = Property.find_property curr_class pname in
      let ivar = (match property_impl_decl_info.Clang_ast_t.opidi_ivar_decl with
          | Some dr -> Ast_utils.name_opt_of_name_info_opt dr.Clang_ast_t.dr_name
          | None -> None) in
      (* update property info with proper ivar name *)
      Property.replace_property (curr_class, pname) (qt', atts, di, getter, setter, ivar);
      Printing.log_out  "Updated property table by adding ivar name for property pname '%s'\n" pname;
      Some qt'
    with Not_found -> L.err "Property '%s' not found in the table. Ivar not updated and qual_type not found.@." pname;
        None) in
  match property_impl_decl_info.Clang_ast_t.opidi_implementation with
  | `Dynamic ->
  (* For Dynamic property we need to create the ObjCIvarDecl which specifies*)
  (* the field of the property. In case of Dynamic this is not in the AST.*)
  (* Once created the ObjCIvarDecl then we treat the property as synthesized *)
      [Ast_expressions.make_objc_ivar_decl decl_info qt property_impl_decl_info]
  | `Synthesize ->
  (* No names of fields/method to collect from ObjCPropertyImplDecl when Synthesized *)
      []

(*NOTE: Assumption: if there is a getter or a setter defined manually, *)
(* it has been translated already at this point. *)
let method_exists cfg class_name name attributes =
  let procname = CMethod_trans.mk_procname_from_method class_name name in
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some procdesc ->
      Cfg.Procdesc.is_defined procdesc
  | None -> false

let is_property_read_only attributes =
  list_mem (Ast_utils.property_attribute_eq) `Readonly attributes

let should_generate_getter cfg class_name name attributes =
  not (method_exists cfg class_name name attributes)

let should_generate_setter cfg class_name name attributes =
  let setter_exists = not (method_exists cfg class_name name attributes) in
  let is_read_only = is_property_read_only attributes in
  setter_exists && not is_read_only

let get_memory_management_attribute attributes =
  let memory_management_attributes = Ast_utils.get_memory_management_attributes () in
  try Some (list_find (
          fun att -> list_mem (Ast_utils.property_attribute_eq)
                att memory_management_attributes) attributes)
  with Not_found -> None

(*Makes the getters and setters according to the attributes: *)
(* - If readonly is available, only write getter *)
(* - If strong or retain are available then write the following code in the setter: *)
(* [param retain] *)
(* [self->_field release] *)
(* [self->_field = param] *)
(* - If copy is available then write the following code: *)
(* [self->_field = [param copy] *)
let make_getter_setter cfg curr_class decl_info property_impl_decl_info =
  let class_name = CContext.get_curr_class_name curr_class in
  let prop_name = Ast_utils.property_name property_impl_decl_info in
  Printing.log_out "ADDING: ObjCPropertyImplDecl for property '%s' " prop_name;
  Printing.log_out "pointer = '%s'\n" decl_info.Clang_ast_t.di_pointer;
  let qt, attributes, decl_info, (getter_name, getter), (setter_name, setter), _ = (try
      Property.find_property curr_class prop_name
    with _ ->
        Printing.log_out "Property %s not found@." prop_name;
        assert false) in
  let ivar_name = get_ivarname_property property_impl_decl_info in
  let make_getter () =
    (
      match getter with
      | Some (ObjCMethodDecl(di, name_info, mdi), _) ->
          let dummy_info = Ast_expressions.dummy_decl_info di in
          if should_generate_getter cfg class_name name_info.Clang_ast_t.ni_name attributes then
            let deref_self_field = Ast_expressions.make_deref_self_field
                (CContext.get_qt_curr_class curr_class) dummy_info mdi.Clang_ast_t.omdi_result_type ivar_name in
            let body = ReturnStmt(Ast_expressions.make_stmt_info dummy_info, [deref_self_field]) in
            let mdi'= Ast_expressions.make_method_decl_info mdi body in
            let method_decl = ObjCMethodDecl(di, name_info, mdi) in
            Property.replace_property
              (curr_class, prop_name)
              (qt, attributes, decl_info,
                (getter_name, Some (method_decl, true)), (setter_name, setter), Some ivar_name);
            [ObjCMethodDecl(dummy_info, name_info, mdi')]
          else []
      | _ -> []) in
  let make_setter () =
    match setter with
    | Some (ObjCMethodDecl(di, name_info, mdi), _) ->
        if should_generate_setter cfg class_name name_info.Clang_ast_t.ni_name attributes then
          let dummy_info = Ast_expressions.dummy_decl_info di in
          let param_name, qt_param = (match mdi.Clang_ast_t.omdi_parameters with
              | [ParmVarDecl(_, name_info, qt_param, _)] -> name_info.Clang_ast_t.ni_name, qt_param
              | _ -> assert false) in
          let is_hidden = (match property_impl_decl_info.Clang_ast_t.opidi_ivar_decl with
              | Some dr -> dr.Clang_ast_t.dr_is_hidden
              | _ -> false) in
          let decl_ptr = Ast_utils.get_invalid_pointer () in
          let drti_decl_ref' =
            Ast_expressions.make_general_decl_ref (`ParmVar) decl_ptr param_name is_hidden qt_param in
          let decl_ref_expr_info' = Ast_expressions.make_decl_ref_expr_info drti_decl_ref' in
          let expr_info = Ast_expressions.make_expr_info qt_param in
          let stmt_info = Ast_expressions.make_stmt_info dummy_info in
          let rhs_exp = Ast_expressions.make_cast_expr qt_param di decl_ref_expr_info' `ObjCProperty in
          let lhs_exp = Ast_expressions.make_self_field
              (CContext.get_qt_curr_class curr_class) di qt_param ivar_name in
          let boi = { Clang_ast_t.boi_kind = `Assign } in
          let setter = Ast_expressions.make_binary_stmt lhs_exp rhs_exp stmt_info expr_info boi in
          let memory_management_attribute = (get_memory_management_attribute attributes) in
          let code =
            if Ast_utils.is_retain memory_management_attribute then
              let param_decl =
                Ast_expressions.make_decl_ref_exp_var (param_name, qt_param, decl_ptr) `ParmVar stmt_info in
              let retain_call =
                Ast_expressions.make_message_expr qt_param retain param_decl stmt_info true in
              let release_call =
                Ast_expressions.make_message_expr qt_param release lhs_exp stmt_info true in
              [retain_call; release_call; setter]
            else if Ast_utils.is_copy memory_management_attribute then
              let param_decl =
                Ast_expressions.make_decl_ref_exp_var (param_name, qt_param, decl_ptr) `ParmVar stmt_info in
              let copy_call =
                Ast_expressions.make_message_expr qt_param copy param_decl stmt_info true in
              let setter =
                Ast_expressions.make_binary_stmt lhs_exp copy_call stmt_info expr_info boi in
              [setter]
            else [setter] in
          let body = Ast_expressions.make_compound_stmt code stmt_info in
          let mdi'= Ast_expressions.make_method_decl_info mdi body in
          Property.replace_property
            (curr_class, prop_name)
            (qt, attributes, decl_info,
              (getter_name, getter),
              (setter_name, Some (ObjCMethodDecl(di, name_info, mdi), true)), Some ivar_name);
          [ObjCMethodDecl(dummy_info, name_info, mdi')]
        else []
    | _ -> [] in
  match property_impl_decl_info.Clang_ast_t.opidi_implementation with
  | `Dynamic
  (* For the moment we treat Dynamic properties as Synthesize. This imply that setter/getter very simply, getting and setting *)
  (* the value. Therefore we are assuming that the dynamic setter/getter won't have any other side effect.*)
  | `Synthesize ->
      (make_getter ()) @ (make_setter ())

(* Given a list of declarations in an interface returns a triple *)
(* (list of non static fields, list of static fields, list of methods)*)
(* TODO: this part is a bit redundant: we could add the methods to the tenv *)
(* at the same time that we add them to the cfg *)
let rec get_methods curr_class decl_list =
  let class_name = CContext.get_curr_class_name curr_class in
  match decl_list with
  | [] -> []
  | (ObjCMethodDecl(decl_info, name_info, method_decl_info) as d):: decl_list' ->
      let method_name = name_info.Clang_ast_t.ni_name in
      Printing.log_out "  ...Adding Method '%s' \n" (class_name^"_"^method_name);
      let methods = get_methods curr_class decl_list' in
      let _ = check_for_property curr_class method_name d method_decl_info.Clang_ast_t.omdi_body in
      let meth_name = CMethod_trans.mk_procname_from_method class_name method_name in
      meth_name:: methods

  | ObjCPropertyDecl(decl_info, name_info, pdi):: decl_list' ->
  (* Property declaration register the property on the property table to be *)
  (* used later on in case getter and setters need to be synthesized by ObjCPropertyImplDecl *)
      let pname = name_info.Clang_ast_t.ni_name in
      Printing.log_out "  ...Adding Property Declaration '%s' " pname;
      Printing.log_out "  pointer= '%s' \n" decl_info.Clang_ast_t.di_pointer;
      Property.add_property (curr_class, pname) pdi.opdi_qual_type pdi.opdi_property_attributes decl_info;
      get_methods curr_class decl_list' (* TODO maybe add prop_name here *)

  | (d : Clang_ast_t.decl):: decl_list' -> get_methods curr_class decl_list'
