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

module L = Logging

open CContext

type prop_getter_setter = string * (Clang_ast_t.decl * bool) option

(** For each property, we save the getter and the setter method declarations (no implementation). *)
(** A property type is a tuple: *)
(** (type_ptr, property attributes, decl_info, (getter_name, getter), (setter_name, setter), ivar name) *)
type property_type = Clang_ast_t.type_ptr * Clang_ast_t.property_attribute list *
                     Clang_ast_t.decl_info * prop_getter_setter * prop_getter_setter *
                     Clang_ast_t.named_decl_info option

(** A table that record the property defined in the interface and its getter/setter. *)
(** This info used later on in the implementation if the getter/setter need to automatically *)
(** synthesized*)
module type PropertySig =
sig

  type t

  type property_key = (CContext.curr_class * Clang_ast_t.named_decl_info)

  val property_key_to_string : property_key -> string

  val reset_property_table: unit -> unit

  val find_property : CContext.curr_class -> Clang_ast_t.named_decl_info -> property_type

  val find_properties_class : CContext.curr_class ->
    (Clang_ast_t.named_decl_info * property_type) list

  val is_mem_property : property_key -> bool

  val replace_property : property_key -> property_type -> unit

  val add_property : property_key -> Clang_ast_t.type_ptr -> Clang_ast_t.property_attribute list ->
    Clang_ast_t.decl_info -> unit

  val print_property_table : unit -> unit

  val find_property_name_from_ivar : CContext.curr_class -> Clang_ast_t.named_decl_info ->
    Clang_ast_t.named_decl_info option
end
module Property: PropertySig =
struct

  type property_key = (CContext.curr_class * Clang_ast_t.named_decl_info)

  let property_key_to_string (curr_class, property_name) =
    ((CContext.curr_class_to_string curr_class) ^ "-" ^ property_name.Clang_ast_t.ni_name)

  (** Hash table to implement error logs *)
  module PropertyTableHash = Hashtbl.Make (struct

      type t = property_key

      let hash (curr_class, pname) = (CContext.curr_class_hash curr_class) + Hashtbl.hash pname

      let equal (curr_class1, property_name1) (curr_class2, property_name2) =
        CContext.curr_class_equal curr_class1 curr_class2 &&
        (String.compare property_name1.Clang_ast_t.ni_name property_name2.Clang_ast_t.ni_name = 0)

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
        let res_opt = IList.fold_right
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
        | Some s when (CContext.curr_class_equal curr_class cl) && s = ivar -> res:= Some pname
        | _ -> ()) property_table;
    !res

  let is_mem_property property =
    PropertyTableHash.mem property_table property

  let replace_property property =
    PropertyTableHash.replace property_table property

  let print_property_table () =
    let print_item key (tp, attributes, decl_info, getter, setter, ivar) =
      let getter_str =
        match getter with
        | getter_name, Some (Clang_ast_t.ObjCMethodDecl _, defined1) ->
            getter_name
        | _ -> "" in
      let setter_str = match setter with
        | setter_name, Some (Clang_ast_t.ObjCMethodDecl _, defined2) ->
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
    | Some name -> name.Clang_ast_t.ni_name
    | None -> prop_name.Clang_ast_t.ni_name

  let get_setter_name prop_name attributes =
    let open Clang_ast_t in
    match Ast_utils.setter_attribute_opt attributes with
    | Some name -> name.Clang_ast_t.ni_name
    | None -> "set" ^ (String.capitalize prop_name.ni_name) ^ ":"

  let add_property (curr_class, property_name) tp attributes decl_info =
    let key = (curr_class, property_name) in
    let getter_name = get_getter_name property_name attributes in
    let setter_name = get_setter_name property_name attributes in
    PropertyTableHash.add property_table key
      (tp, attributes, decl_info, (getter_name, None), (setter_name, None), None)
end

let reset_property_table = Property.reset_property_table

let print_property_table () = Property.print_property_table ()

let find_properties_class = Property.find_properties_class

let get_ivarname_property pidi =
  match pidi.Clang_ast_t.opidi_ivar_decl with
  | Some dr -> (match dr.Clang_ast_t.dr_name with
      | Some n -> n
      | _ -> assert false)
  | _ -> (* If ivar is not defined than we need to take the name of the property to define ivar*)
      Ast_utils.property_name pidi

let upgrade_property_accessor property_key property_type meth_decl new_defined is_getter =
  let is_defined meth_decl =
    match meth_decl with
    | Some (method_decl, was_defined) -> new_defined || was_defined
    | None -> new_defined in
  match property_type with
  | tp, attributes, decl_info, (gname, getter), (sname, setter), ivar ->
      if is_getter then
        let defined = is_defined getter in
        Property.replace_property property_key
          (tp, attributes, decl_info, (gname, Some (meth_decl, defined)), (sname, setter), ivar)
      else let defined = is_defined setter in
        Property.replace_property property_key
          (tp, attributes, decl_info, (gname, getter), (sname, Some (meth_decl, defined)), ivar)

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
    IList.iter method_is_getter properties_class in
  check_property_accessor curr_class method_name true;
  check_property_accessor curr_class method_name false

let method_is_property_accesor cls method_name =
  let properties_class = find_properties_class cls in
  let method_is_getter (property_name, property_type) res_opt =
    match res_opt with
    | Some res -> res_opt
    | None ->
        match property_type with (_, _, _, (getter_name, _), (setter_name, _), _) ->
          if method_name = getter_name then Some (property_name, property_type, true)
          else if method_name = setter_name then Some (property_name, property_type, false)
          else None in
  IList.fold_right method_is_getter properties_class None

let is_strong_property property_type =
  let _, attrs, _, _, _, _ = property_type in
  IList.exists (fun a -> match a with
      | `Strong -> true
      | _ -> false) attrs

let property_line property_type =
  let _, attrs, decl_info, _, _, _ = property_type in
  let src_range = fst decl_info.Clang_ast_t.di_source_range in
  match src_range.Clang_ast_t.sl_line with
  | Some l -> l
  | _ -> -1

let prepare_dynamic_property curr_class decl_info property_impl_decl_info =
  let pname = Ast_utils.property_name property_impl_decl_info in
  let prop_name = pname.Clang_ast_t.ni_name in
  let res = (try
               let tp', atts, di, getter, setter, _ = Property.find_property curr_class pname in
               let ivar = (match property_impl_decl_info.Clang_ast_t.opidi_ivar_decl with
                   | Some dr -> (match dr.Clang_ast_t.dr_name with
                       | Some name_info -> name_info
                       | None -> assert false)
                   | None -> Ast_utils.generated_ivar_name pname) in
               (* update property info with proper ivar name *)
               Property.replace_property (curr_class, pname) (tp', atts, di, getter, setter, Some ivar);
               Printing.log_out "Updated property table by adding ivar name for property '%s'\n"
                 prop_name;
               Some (tp', ivar)
             with Not_found ->
               L.err "Property '%s' not found in the table. Ivar not updated.@." prop_name;
               None) in
  match property_impl_decl_info.Clang_ast_t.opidi_implementation, res with
  | `Dynamic, Some (tp, ivar) ->
      (* For Dynamic property we need to create the ObjCIvarDecl which specifies*)
      (* the field of the property. In case of Dynamic this is not in the AST.*)
      (* Once created the ObjCIvarDecl then we treat the property as synthesized *)
      [Ast_expressions.make_objc_ivar_decl decl_info tp property_impl_decl_info ivar]
  | _ ->
      (* No names of fields/method to collect from ObjCPropertyImplDecl when Synthesized *)
      []

let is_property_read_only attributes =
  IList.mem (Ast_utils.property_attribute_eq) `Readonly attributes

let get_memory_management_attribute attributes =
  let memory_management_attributes = Ast_utils.get_memory_management_attributes () in
  try Some (IList.find (
      fun att -> IList.mem (Ast_utils.property_attribute_eq)
          att memory_management_attributes) attributes)
  with Not_found -> None

let create_generated_method_name name_info =
  let qual_name = match name_info.Clang_ast_t.ni_qual_name with
    | [] -> []
    | name :: quals -> (name ^ CFrontend_config.generated_suffix) :: quals in
  { Clang_ast_t.ni_name = name_info.Clang_ast_t.ni_name;
    ni_qual_name = qual_name;
  }

let get_ivar_name prop_name ivar_opt =
  match ivar_opt with
  | Some ivar_name -> ivar_name
  | None -> Ast_utils.generated_ivar_name prop_name

let make_setter curr_class prop_name prop_type =
  match prop_type with
  | tp, attributes, decl_info, (getter_name, getter), (setter_name, setter), ivar_opt ->
      let ivar_name = get_ivar_name prop_name ivar_opt in
      let open Clang_ast_t in
      match setter with
      | Some (ObjCMethodDecl(di, name, mdi), _) when not (is_property_read_only attributes) ->
          let dummy_info = Ast_expressions.dummy_decl_info_in_curr_file di in
          let param_name, tp_param = (match mdi.Clang_ast_t.omdi_parameters with
              | [ParmVarDecl(_, name_info, tp_param, _)] -> name_info, tp_param
              | _ -> assert false) in
          let is_hidden = false in
          let decl_ptr = Ast_utils.get_invalid_pointer () in
          let drti_decl_ref' =
            Ast_expressions.make_decl_ref_tp (`ParmVar) decl_ptr param_name is_hidden tp_param in
          let decl_ref_expr_info' = Ast_expressions.make_decl_ref_expr_info drti_decl_ref' in
          let expr_info = Ast_expressions.make_expr_info tp_param in
          let stmt_info = Ast_expressions.make_stmt_info dummy_info in
          let rhs_exp = Ast_expressions.make_cast_expr tp_param di decl_ref_expr_info' `ObjCProperty in
          let lhs_exp = Ast_expressions.make_self_field
              (CContext.get_curr_class_name curr_class) di tp_param ivar_name in
          let boi = { Clang_ast_t.boi_kind = `Assign } in
          let setter = Ast_expressions.make_binary_stmt lhs_exp rhs_exp stmt_info expr_info boi in
          let memory_management_attribute = (get_memory_management_attribute attributes) in
          let code =
            if Ast_utils.is_retain memory_management_attribute then
              let param_decl = Ast_expressions.make_decl_ref_exp_var (param_name, tp_param, decl_ptr) `ParmVar stmt_info in
              let retain_call = Ast_expressions.make_message_expr tp_param CFrontend_config.retain param_decl stmt_info true in
              let release_call = Ast_expressions.make_message_expr tp_param CFrontend_config.release lhs_exp stmt_info true in
              [retain_call; release_call; setter]
            else if Ast_utils.is_copy memory_management_attribute then
              let param_decl = Ast_expressions.make_decl_ref_exp_var (param_name, tp_param, decl_ptr) `ParmVar stmt_info in
              let copy_call = Ast_expressions.make_message_expr tp_param CFrontend_config.copy param_decl stmt_info true in
              let setter = Ast_expressions.make_binary_stmt lhs_exp copy_call stmt_info expr_info boi in
              [setter]
            else [setter] in
          let body = Ast_expressions.make_compound_stmt code stmt_info in
          let mdi'= Ast_expressions.make_method_decl_info mdi body in
          let name_generated = create_generated_method_name name in
          Property.replace_property
            (curr_class, prop_name)
            (tp, attributes, decl_info,
             (getter_name, getter),
             (setter_name, Some (ObjCMethodDecl(di, name, mdi), true)), Some ivar_name);
          [ObjCMethodDecl(dummy_info, name_generated, mdi')]
      | _ -> []

(*Makes the getters and setters according to the attributes: *)
(* - If readonly is available, only write getter *)
(* - If strong or retain are available then write the following code in the setter: *)
(* [param retain] *)
(* [self->_field release] *)
(* [self->_field = param] *)
(* - If copy is available then write the following code: *)
(* [self->_field = [param copy] *)
let make_setter' curr_class decl_info prop_name =
  Printing.log_out "pointer = '%s'\n" decl_info.Clang_ast_t.di_pointer;
  try
    let prop_type = Property.find_property curr_class prop_name in
    (make_setter curr_class prop_name prop_type)
  with _ ->
    Printing.log_out "Property %s not found@." prop_name.Clang_ast_t.ni_name;
    []

let add_properties_to_table curr_class decl_list =
  let add_property_to_table dec =
    match dec with
    | Clang_ast_t.ObjCPropertyDecl(decl_info, name_info, pdi) ->
        (* Property declaration register the property on the property table to be *)
        let pname = name_info.Clang_ast_t.ni_name in
        Printing.log_out "ADDING: ObjCPropertyDecl for property '%s' " pname;
        Printing.log_out "  pointer= '%s' \n" decl_info.Clang_ast_t.di_pointer;
        Property.add_property (curr_class, name_info) pdi.Clang_ast_t.opdi_type_ptr
          pdi.Clang_ast_t.opdi_property_attributes decl_info;
    | _ -> () in
  IList.iter add_property_to_table decl_list

(* Given a list of declarations in an interface returns list of methods *)
let get_methods curr_class decl_list =
  let class_name = CContext.get_curr_class_name curr_class in
  add_properties_to_table curr_class decl_list;
  let get_method decl list_methods =
    match decl with
    | Clang_ast_t.ObjCMethodDecl (decl_info, name_info, method_decl_info) ->
        let is_instance = method_decl_info.Clang_ast_t.omdi_is_instance_method in
        let method_kind = Procname.objc_method_kind_of_bool is_instance in
        let method_name = name_info.Clang_ast_t.ni_name in
        Printing.log_out "  ...Adding Method '%s' \n" (class_name^"_"^method_name);
        let _ = check_for_property curr_class method_name decl method_decl_info.Clang_ast_t.omdi_body in
        let meth_name = General_utils.mk_procname_from_objc_method class_name method_name method_kind in
        meth_name:: list_methods
    | _ -> list_methods in
  IList.fold_right get_method decl_list []
