(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Pattern matching. *)

val get_this_type : ProcAttributes.t -> Typ.t option
(** Get the this type of a procedure *)

val get_type_name : Typ.t -> string
(** Get the name of a type *)

val get_vararg_type_names : Tenv.t -> Procdesc.Node.t -> Pvar.t -> string list
(** Get the type names of a variable argument *)

val method_is_initializer : Tenv.t -> ProcAttributes.t -> bool
(** Check if the method is one of the known initializer methods. *)

val is_getter : Typ.Procname.Java.t -> bool
(** Is this a getter proc name? *)

val is_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool
(** Is the type a transitive subtype of the typename? *)

val is_subtype_of_str : Tenv.t -> Typ.Name.t -> string -> bool
(** Resolve [typ_str] in [tenv], then check [typ] <: [typ_str] *)

val implements_iterator : Tenv.t -> string -> bool
(** Check whether class implements Java's Iterator *)

val implements_collection : Tenv.t -> string -> bool
(** Check whether class implements a Java's Collection *)

val implements_pseudo_collection : Tenv.t -> string -> bool
(** Check whether class implements a pseudo Collection with support
   for get() and size() methods *)

val implements_enumeration : Tenv.t -> string -> bool
(** Check whether class implements a Java's Enumeration *)

val implements_jackson : string -> Tenv.t -> string -> bool
(** Check whether class implements a class from Jackson *)

val implements_inject : string -> Tenv.t -> string -> bool
(** Check whether class implements a Javax Inject *)

val implements_io : string -> Tenv.t -> string -> bool
(** Check whether class implements a Java IO *)

val implements_map : Tenv.t -> string -> bool
(** Check whether class implements a Java's Map *)

val implements_map_entry : Tenv.t -> string -> bool
(** Check whether class implements a Java's Map$Entry *)

val implements_queue : Tenv.t -> string -> bool
(** Check whether class implements a Java's Queue *)

val implements_lang : string -> Tenv.t -> string -> bool
(** Check whether class implements a Java's lang *)

val implements_list : Tenv.t -> string -> bool
(** Check whether class implements a Java's list *)

val implements_google : string -> Tenv.t -> string -> bool
(** Check whether class implements a class of Google  *)

val implements_android : string -> Tenv.t -> string -> bool
(** Check whether class implements a class of Android  *)

val supertype_exists : Tenv.t -> (Typ.Name.t -> Typ.Struct.t -> bool) -> Typ.Name.t -> bool
(** Holds iff the predicate holds on a supertype of the named type, including the type itself *)

val supertype_find_map_opt : Tenv.t -> (Typ.Name.t -> 'a option) -> Typ.Name.t -> 'a option
(** Return the first non-None result found when applying the given function to supertypes of the
    named type, including the type itself *)

val java_get_vararg_values : Procdesc.Node.t -> Pvar.t -> Idenv.t -> Exp.t list
(** Get the values of a vararg parameter given the pvar used to assign the elements. *)

val proc_calls :
     (Typ.Procname.t -> ProcAttributes.t option)
  -> Procdesc.t
  -> (Typ.Procname.t -> ProcAttributes.t -> bool)
  -> (Typ.Procname.t * ProcAttributes.t) list
(** Return the callees that satisfy [filter]. *)

val override_find :
     ?check_current_type:bool
  -> (Typ.Procname.t -> bool)
  -> Tenv.t
  -> Typ.Procname.t
  -> Typ.Procname.t option
(** Return a method which overrides [procname] and satisfies [f] (including [procname] itself when [check_current_type] is true, which it is by default). *)

val override_exists :
  ?check_current_type:bool -> (Typ.Procname.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool
(** Return true if applying the given predicate to an override of [procname] (including [procname] itself when [check_current_type] is true, which it is by default) returns true. *)

val override_iter : (Typ.Procname.t -> unit) -> Tenv.t -> Typ.Procname.t -> unit
(** Apply the given predicate to procname and each override of [procname]. For the moment, this only
    works for Java *)

val lookup_attributes : Tenv.t -> Typ.Procname.t -> ProcAttributes.t option

val type_get_annotation : Tenv.t -> Typ.t -> Annot.Item.t option

val type_get_class_name : Typ.t -> Typ.Name.t option
(** Get the class name of the type *)

val type_is_class : Typ.t -> bool
(** Is the type a class type *)

val type_is_object : Typ.t -> bool
(** Is the type java.lang.Object *)

val get_fields_nullified : Procdesc.t -> Typ.Fieldname.Set.t
(** return the set of instance fields that are assigned to a null literal in [procdesc] *)

val is_throwable : Tenv.t -> Typ.Name.t -> bool
(** [is_throwable tenv class_name] checks if class_name is of type java.lang.Throwable *)

val is_runtime_exception : Tenv.t -> Typ.Name.t -> bool
(** [is_runtime_exception tenv class_name] checks if classname is
    of type java.lang.RuntimeException *)

val check_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool
(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
     including supertypes*)

val check_current_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Typ.Procname.t -> bool
(** tests whether any class attributes (e.g., @ThreadSafe) pass check of first argument,
    for current class only*)

val find_superclasses_with_attributes :
  (Annot.Item.t -> bool) -> Tenv.t -> Typ.Name.t -> Typ.Name.t list
(** find superclasss with attributes (e.g., @ThreadSafe), including current class*)
