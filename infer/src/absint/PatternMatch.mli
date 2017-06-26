(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for Pattern matching. *)

type taint_spec = {
  classname : string;
  method_name : string;
  ret_type : string;
  params : string list;
  is_static : bool;
  taint_kind : PredSymb.taint_kind;
  language : Config.language
}

(** Returns the signature of a field access (class name, field name, field type name) *)
val get_java_field_access_signature : Sil.instr -> (string * string * string) option

(** Returns the formal signature (class name, method name,
    argument type names and return type name) *)
val get_java_method_call_formal_signature :
  Sil.instr -> (string * string * string list * string) option

(** Get the this type of a procedure *)
val get_this_type : ProcAttributes.t -> Typ.t option

(** Get the name of a type *)
val get_type_name : Typ.t -> string

(** Get the type names of a variable argument *)
val get_vararg_type_names : Tenv.t -> Procdesc.Node.t -> Pvar.t -> string list

val has_formal_method_argument_type_names :
  Procdesc.t -> Typ.Procname.java -> string list -> bool

(** Check if the method is one of the known initializer methods. *)
val method_is_initializer : Tenv.t -> ProcAttributes.t -> bool

(** Is this a getter proc name? *)
val is_getter : Typ.Procname.java -> bool

(** Is this a setter proc name? *)
val is_setter : Typ.Procname.java -> bool

(** Is the type a direct subtype of the typename? *)
val is_immediate_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool

(** Is the type a transitive subtype of the typename? *)
val is_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool

(** Resolve [typ_str] in [tenv], then check [typ] <: [typ_str] *)
val is_subtype_of_str : Tenv.t -> Typ.Name.t -> string -> bool

(** Holds iff the predicate holds on a supertype of the named type, including the type itself *)
val supertype_exists : Tenv.t -> (Typ.Name.t -> Typ.Struct.t -> bool) -> Typ.Name.t -> bool

(** Return the first non-None result found when applying the given function to supertypes of the
    named type, including the type itself *)
val supertype_find_map_opt :
  Tenv.t -> (Typ.Name.t -> Typ.Struct.t -> 'a option) -> Typ.Name.t -> 'a option

(** Get the name of the type of a constant *)
val java_get_const_type_name : Const.t -> string

(** Get the values of a vararg parameter given the pvar used to assign the elements. *)
val java_get_vararg_values : Procdesc.Node.t -> Pvar.t -> Idenv.t -> Exp.t list

val java_proc_name_with_class_method : Typ.Procname.java -> string -> string -> bool

(** Return the callees that satisfy [filter]. *)
val proc_calls :
  (Typ.Procname.t -> ProcAttributes.t option) ->
  Procdesc.t ->
  (Typ.Procname.t -> ProcAttributes.t -> bool) ->
  (Typ.Procname.t * ProcAttributes.t) list

(** Return true if applying the given predicate to an override of [procname] or [procname] itself
    returns true. For the moment, this only works for Java *)
val override_exists : (Typ.Procname.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool

(** Apply the given predicate to procname and each override of [procname]. For the moment, this only
    works for Java *)
val override_iter : (Typ.Procname.t -> unit) -> Tenv.t -> Typ.Procname.t -> unit

val type_get_annotation : Tenv.t -> Typ.t -> Annot.Item.t option

(** Get the class name of the type *)
val type_get_class_name : Typ.t -> Typ.Name.t option

(** Is the type a class type *)
val type_is_class : Typ.t -> bool

(** Is the type java.lang.Object *)
val type_is_object : Typ.t -> bool

(** return the set of instance fields that are assigned to a null literal in [procdesc] *)
val get_fields_nullified : Procdesc.t -> Fieldname.Set.t

(** [is_exception tenv class_name] checks if class_name is of type java.lang.Exception *)
val is_exception : Tenv.t -> Typ.Name.t -> bool

(** [is_throwable tenv class_name] checks if class_name is of type java.lang.Throwable *)
val is_throwable : Tenv.t -> Typ.Name.t -> bool

(** [is_runtime_exception tenv class_name] checks if classname is
    of type java.lang.RuntimeException *)
val is_runtime_exception : Tenv.t -> Typ.Name.t -> bool

(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
     including supertypes*)
val check_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool

(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
    for current class only*)
val check_current_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool

(** find superclasss with attributes (e.g., @ThreadSafe), including current class*)
val find_superclasses_with_attributes : (Annot.Item.t -> bool) -> Tenv.t
  -> Typ.Name.t -> Typ.Name.t list
