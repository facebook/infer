(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Process properties by creating their getters and setters in the case that they need to be syntethized *)
(** or in the case of dynamic. *)

type prop_getter_setter = string * (Clang_ast_t.decl * bool) option

(** For each property, we save the getter and the setter method declarations (no implementation). *)
(** A property type is a tuple: *)
(** (qual_type, property attributes, decl_info, (getter_name, getter), (setter_name, setter), ivar name ) *)
type property_type = Clang_ast_t.qual_type * Clang_ast_t.property_attribute list *
  Clang_ast_t.decl_info * prop_getter_setter * prop_getter_setter * string option

val prepare_dynamic_property : CContext.curr_class -> Clang_ast_t.decl_info ->
Clang_ast_t.obj_c_property_impl_decl_info -> Clang_ast_t.decl list

val get_methods : CContext.curr_class -> Clang_ast_t.decl list -> Procname.t list

val make_getter_setter : Cfg.cfg -> CContext.curr_class -> Clang_ast_t.decl_info -> Clang_ast_t.obj_c_property_impl_decl_info ->
Clang_ast_t.decl list

val reset_property_table : unit -> unit

val print_property_table : unit -> unit

val is_property_read_only : Clang_ast_t.property_attribute list -> bool

val find_properties_class : CContext.curr_class -> (string * property_type) list


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

module Property: PropertySig
