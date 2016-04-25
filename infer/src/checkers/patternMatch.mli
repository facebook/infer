(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for Pattern matching. *)

type method_str = {
  classname : string;
  method_name : string;
  ret_type : string;
  params : string list;
  is_static : bool;
  language : Config.language
}

(** Returns the signature of a field access (class name, field name, field type name) *)
val get_java_field_access_signature : Sil.instr -> (string * string * string) option

(** Returns the formal signature (class name, method name,
    argument type names and return type name) *)
val get_java_method_call_formal_signature :
  Sil.instr -> (string * string * string list * string) option

(** Get the this type of a procedure *)
val get_this_type : ProcAttributes.t -> Sil.typ option

(** Get the name of a type *)
val get_type_name : Sil.typ -> string

(** Get the type names of a variable argument *)
val get_vararg_type_names : Cfg.Node.t -> Pvar.t -> string list

val has_formal_method_argument_type_names :
  Cfg.Procdesc.t -> Procname.java -> string list -> bool

(** Check if the method is one of the known initializer methods. *)
val method_is_initializer : Tenv.t -> ProcAttributes.t -> bool

(** Is this a getter proc name? *)
val is_getter : Procname.java -> bool

(** Is this a setter proc name? *)
val is_setter : Procname.java -> bool

(** Is the type a direct subtype of the typename? *)
val is_immediate_subtype : Sil.struct_typ -> Typename.t -> bool

(** Is the type a transitive subtype of the typename? *)
val is_subtype : Tenv.t -> Sil.struct_typ -> Sil.struct_typ -> bool

(** Resolve [typ_str] in [tenv], then check [typ] <: [typ_str] *)
val is_subtype_of_str : Tenv.t -> Typename.t -> string -> bool

(** get the superclasses of [typ]. does not include [typ] itself *)
val strict_supertype_iter : Tenv.t -> (Sil.struct_typ -> unit) -> Sil.struct_typ -> unit

(** Return [true] if [f_typ] evaluates to true on a strict supertype of [orig_struct_typ] *)
val strict_supertype_exists : Tenv.t -> (Sil.struct_typ -> bool) -> Sil.struct_typ -> bool

(** Get the name of the type of a constant *)
val java_get_const_type_name : Sil.const -> string

(** Get the values of a vararg parameter given the pvar used to assign the elements. *)
val java_get_vararg_values : Cfg.Node.t -> Pvar.t -> Idenv.t -> Sil.exp list

val java_proc_name_with_class_method : Procname.java -> string -> string -> bool

(** Return the callees that satisfy [filter]. *)
val proc_calls :
  (Procname.t -> ProcAttributes.t option) ->
  Cfg.Procdesc.t ->
  (Procname.t -> ProcAttributes.t -> bool) ->
  (Procname.t * ProcAttributes.t) list

(** Iterate over all the methods overridden by the procedure.
    Only Java supported at the moment. *)
val proc_iter_overridden_methods : (Procname.t -> unit) -> Tenv.t -> Procname.t -> unit

val type_get_annotation : Sil.typ -> Sil.item_annotation option

(** Get the class name of the type *)
val type_get_class_name : Sil.typ -> Mangled.t option

val type_get_direct_supertypes : Sil.typ -> Typename.t list

(** Is the type a class with the given name *)
val type_has_class_name : Sil.typ -> Mangled.t -> bool

val type_has_direct_supertype : Sil.typ -> Typename.t -> bool

(** Is the type a class type *)
val type_is_class : Sil.typ -> bool

val type_is_nested_in_direct_supertype : Sil.typ -> Typename.t -> bool

val type_is_nested_in_type : Sil.typ -> Mangled.t -> bool

(** Is the type java.lang.Object *)
val type_is_object : Sil.typ -> bool

(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
val get_fields_nullified : Cfg.Procdesc.t -> Ident.FieldSet.t

(** [is_exception tenv class_name] checks if class_name is of type java.lang.Exception *)
val is_exception : Tenv.t -> Typename.t -> bool

(** [is_throwable tenv class_name] checks if class_name is of type java.lang.Throwable *)
val is_throwable : Tenv.t -> Typename.t -> bool

(** [is_runtime_exception tenv class_name] checks if classname is
    of type java.lang.RuntimeException *)
val is_runtime_exception : Tenv.t -> Typename.t -> bool
