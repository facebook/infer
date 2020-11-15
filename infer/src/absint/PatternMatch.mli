(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Pattern matching. *)

val get_this_type_nonstatic_methods_only : ProcAttributes.t -> Typ.t option
(** Get the `this` type of a procedure. Should not be called on non-static methods, otherwise it can
    return a wrong type *)

val get_type_name : Typ.t -> string
(** Get the name of a type *)

val get_vararg_type_names : Tenv.t -> Procdesc.Node.t -> Pvar.t -> string list
(** Get the type names of a variable argument *)

val is_subtype : Tenv.t -> Typ.Name.t -> Typ.Name.t -> bool
(** Is the type a transitive subtype of the typename? *)

val is_subtype_of_str : Tenv.t -> Typ.Name.t -> string -> bool
(** Resolve [typ_str] in [tenv], then check [typ] <: [typ_str] *)

module Java : sig
  val implements : string -> Tenv.t -> string -> bool
  (** Check whether class implements a given Java class *)

  val implements_arrays : Tenv.t -> string -> bool
  (** Check whether class implements Java's Arrays *)

  val implements_iterable : Tenv.t -> string -> bool
  (** Check whether class implements Java's Iterable *)

  val implements_iterator : Tenv.t -> string -> bool
  (** Check whether class implements Java's Iterator *)

  val implements_collection : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Collection *)

  val implements_collections : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Collections *)

  val implements_pseudo_collection : Tenv.t -> string -> bool
  (** Check whether class implements a pseudo Collection with support for get() and size() methods *)

  val implements_enumeration : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Enumeration *)

  val implements_jackson : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class from Jackson *)

  val implements_org_json : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class from Json *)

  val implements_inject : string -> Tenv.t -> string -> bool
  (** Check whether class implements a Javax Inject *)

  val implements_io : string -> Tenv.t -> string -> bool
  (** Check whether class implements a Java IO *)

  val implements_nio : string -> Tenv.t -> string -> bool
  (** Check whether class implements a Java nio *)

  val implements_map : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Map *)

  val implements_androidx_map : Tenv.t -> string -> bool
  (** Check whether class implements a AndroidX's Map *)

  val implements_set : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Set *)

  val implements_map_entry : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Map$Entry *)

  val implements_queue : Tenv.t -> string -> bool
  (** Check whether class implements a Java's Queue *)

  val implements_lang : string -> Tenv.t -> string -> bool
  (** Check whether class implements a Java's lang *)

  val implements_list : Tenv.t -> string -> bool
  (** Check whether class implements a Java's list *)

  val implements_google : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class of Google *)

  val implements_android : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class of Android *)

  val implements_infer_annotation : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class of Infer annotation *)

  val implements_app_activity : Tenv.t -> string -> bool
  (** Check whether class implements a class of [android.app.Activity] *)

  val implements_app_fragment : Tenv.t -> string -> bool
  (** Check whether class implements a class of [androidx.fragment.app.Fragment] *)

  val implements_graphql_story : Tenv.t -> string -> bool
  (** Check whether class implements a class of [com.facebook.graphql.model.GraphQLStory] *)

  val implements_psi_element : Tenv.t -> string -> bool
  (** Check whether class implements a class of [com.intellij.psi.PsiElement] *)

  val implements_sparse_float_array : Tenv.t -> string -> bool
  (** Check whether class implements a class of [com.facebook.litho.internal.SparseFloatArray] *)

  val implements_view_group : Tenv.t -> string -> bool
  (** Check whether class implements a class of [android.view.ViewGroup] *)

  val implements_view_parent : Tenv.t -> string -> bool
  (** Check whether class implements a class of [android.view.ViewParent] *)

  val implements_xmob_utils : string -> Tenv.t -> string -> bool
  (** Check whether class implements a class of xmod.utils *)

  val is_throwable : Tenv.t -> Typ.Name.t -> bool
  (** [is_throwable tenv class_name] checks if class_name is of type java.lang.Throwable *)

  val is_enum : Tenv.t -> Typ.Name.t -> bool
  (** Checks if the type is Java enum (extends java.lang.Enum) *)

  val check_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Procname.t -> bool
  (** tests whether any class attributes (e.g., [@ThreadSafe]) pass check of first argument,
      including supertypes*)

  val check_current_class_attributes : (Annot.Item.t -> bool) -> Tenv.t -> Procname.t -> bool
  (** tests whether any class attributes (e.g., [@ThreadSafe]) pass check of first argument, for
      current class only*)

  val find_superclasses_with_attributes :
    (Annot.Item.t -> bool) -> Tenv.t -> Typ.Name.t -> Typ.Name.t list
  (** find superclasss with attributes (e.g., [@ThreadSafe]), including current class*)

  val is_override_of_lang_object_equals : Procname.t -> bool
  (** Whether the method is an override of `java.lang.Object.equals(Object)` or
      `java.lang.Object.equals(Object)` itself *)

  val method_is_initializer : Tenv.t -> ProcAttributes.t -> bool
  (** Check if the method is one of the known initializer methods. *)
end

val supertype_exists : Tenv.t -> (Typ.Name.t -> Struct.t -> bool) -> Typ.Name.t -> bool
(** Holds iff the predicate holds on a supertype of the named type, including the type itself *)

val supertype_find_map_opt : Tenv.t -> (Typ.Name.t -> 'a option) -> Typ.Name.t -> 'a option
(** Return the first non-None result found when applying the given function to supertypes of the
    named type, including the type itself *)

val proc_calls :
     (Procname.t -> ProcAttributes.t option)
  -> Procdesc.t
  -> (Procname.t -> ProcAttributes.t -> bool)
  -> (Procname.t * ProcAttributes.t) list
(** Return the callees that satisfy [filter]. *)

val override_exists :
  ?check_current_type:bool -> (Procname.t -> bool) -> Tenv.t -> Procname.t -> bool
(** Return true if applying the given predicate to an override of [procname] (including [procname]
    itself when [check_current_type] is true, which it is by default) returns true. *)

val override_iter : (Procname.t -> unit) -> Tenv.t -> Procname.t -> unit
(** Apply the given predicate to procname and each override of [procname]. For the moment, this only
    works for Java *)

val lookup_attributes : Tenv.t -> Procname.t -> ProcAttributes.t option

val lookup_attributes_exn : Tenv.t -> Procname.t -> ProcAttributes.t

val type_name_get_annotation : Tenv.t -> Typ.name -> Annot.Item.t option

val type_get_annotation : Tenv.t -> Typ.t -> Annot.Item.t option

val type_get_class_name : Typ.t -> Typ.Name.t option
(** Get the class name of the type *)

val type_is_class : Typ.t -> bool
(** Is the type a class type *)

val get_fields_nullified : Procdesc.t -> Fieldname.Set.t
(** return the set of instance fields that are assigned to a null literal in [procdesc] *)

val has_same_signature : Procname.t -> (Procname.t -> bool) Staged.t
(** For a given [procname] checks if the method has the same method name, number, order and types of
    parameters.) *)

module ObjectiveC : sig
  val implements : string -> Tenv.t -> string -> bool
  (** Check whether class implements a given ObjC class *)

  val conforms_to : protocol:string -> Tenv.t -> string -> bool
  (** Check whether class conforms to a given ObjC protocol *)

  val implements_collection : Tenv.t -> string -> bool

  val is_core_graphics_create_or_copy : Tenv.t -> string -> bool

  val is_core_foundation_create_or_copy : Tenv.t -> string -> bool

  val is_core_graphics_release : Tenv.t -> string -> bool

  val is_modelled_as_alloc : Tenv.t -> string -> bool

  val is_modelled_as_release : Tenv.t -> string -> bool
end
