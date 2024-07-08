(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Type Environments. *)

(** Type for type environment. *)
type t

val create : unit -> t
(** Create a new type environment. *)

val length : t -> int

val load : SourceFile.t -> t option
[@@alert tenv "Analysis code should use [Exe_env.get_source_tenv] instead."]
(** Load a type environment for a source file *)

val store_debug_file_for_source : SourceFile.t -> t -> unit

val read : DB.filename -> t option
(** Read and return a type environment from the given file *)

val write : t -> DB.filename -> unit
(** Write the type environment into the given file *)

val read_global : unit -> t option
(** Load (without caching) the global type environment *)

val force_load_global : unit -> t option
(** Load and cache the global type environment *)

val load_global : unit -> t option
(** Load and cache the global type environment if not already loaded *)

val store_global : normalize:bool -> t -> unit
(** Save and cache the global type environment *)

val lookup : t -> Typ.Name.t -> Struct.t option
(** Look up a name in the given type environment. *)

val mk_struct :
     t
  -> ?default:Struct.t
  -> ?fields:Struct.field list
  -> ?statics:Struct.field list
  -> ?methods:Procname.t list
  -> ?exported_objc_methods:Procname.t list
  -> ?supers:Typ.Name.t list
  -> ?objc_protocols:Typ.Name.t list
  -> ?annots:Annot.Item.t
  -> ?class_info:Struct.ClassInfo.t
  -> ?dummy:bool
  -> ?source_file:SourceFile.t
  -> Typ.Name.t
  -> Struct.t
(** Construct a struct_typ, normalizing field types *)

val add_field : t -> Typ.Name.t -> Struct.field -> unit
(** Add a field to a given struct in the global type environment. *)

val pp : Format.formatter -> t -> unit
(** print a type environment *)

val fold : t -> init:'acc -> f:(Typ.Name.t -> Struct.t -> 'acc -> 'acc) -> 'acc

val fold_supers :
     ?ignore_require_extends:bool
  -> t
  -> Typ.Name.t
  -> init:'a
  -> f:(Typ.Name.t -> Struct.t option -> 'a -> 'a)
  -> 'a

val mem_supers : t -> Typ.Name.t -> f:(Typ.Name.t -> Struct.t option -> bool) -> bool

val get_parent : t -> Typ.Name.t -> Typ.Name.t option

val find_map_supers :
     ?ignore_require_extends:bool
  -> t
  -> Typ.Name.t
  -> f:(Typ.Name.t -> Struct.t option -> 'a option)
  -> 'a option

val get_fields_trans : t -> Typ.Name.t -> Struct.field list
(** Get all fields from the super classes transitively *)

type per_file = Global | FileLocal of t

val pp_per_file : Format.formatter -> per_file -> unit
(** print per file type environment *)

val merge : src:t -> dst:t -> unit
(** Merge [src] into [dst] *)

val merge_per_file : src:per_file -> dst:per_file -> per_file
(** Best-effort merge of [src] into [dst]. If a procedure is both in [dst] and [src], the one in
    [dst] will get overwritten. *)

module MethodInfo : sig
  module Hack : sig
    type kind = private
      | IsClass  (** Normal method call *)
      | IsTrait of {used: Typ.Name.t; is_direct: bool}
          (** Trait method call: [used] is the name of the class uses the trait. If it is a direct
              trait method call, e.g. [Trait::foo], [used] is the name of the trait. *)
  end

  type t [@@deriving show]

  val mk_class : Procname.t -> t

  val get_procname : t -> Procname.t

  val get_hack_kind : t -> Hack.kind option
end

val resolve_method :
     method_exists:(Procname.t -> Procname.t list -> bool)
  -> t
  -> Typ.Name.t
  -> Procname.t
  -> MethodInfo.t option * Typ.Name.Set.t
(** [resolve_method ~method_exists tenv class_name procname] return a pair
    [(info_opt, missed_captures)] where [info_opt] tries to resolve [procname] to a method in
    [class_name] or its super-classes, that is non-virtual (non-Java-interface method).
    [missed_captures] is the set of classnames for which the hierarchy traversal would have need to
    examine its members but the class was not captured. [method_exists adapted_procname methods]
    should check if [adapted_procname] ([procname] but with its class potentially changed to some
    [other_class]) is among the [methods] of [other_class]. *)

val resolve_field_info : t -> Typ.Name.t -> Fieldname.t -> Struct.field_info option
(** [resolve_field_info tenv class_name field] tries to find the first field declaration that
    matches [field] name (ignoring its enclosing declared type), starting from class [class_name]. *)

val resolve_fieldname : t -> Typ.Name.t -> string -> Fieldname.t option
(** Similar to [resolve_field_info], but returns the resolved field name. *)

val find_cpp_destructor : t -> Typ.Name.t -> Procname.t option

val find_cpp_constructor : t -> Typ.Name.t -> Procname.t list

val is_trivially_copyable : t -> Typ.t -> bool

val get_hack_direct_used_traits : t -> Typ.Name.t -> HackClassName.t list

val expand_hack_alias : t -> Typ.name -> Typ.name option [@@warning "-unused-value-declaration"]

val expand_hack_alias_in_typ : t -> Typ.t -> Typ.t

module SQLite : SqliteUtils.Data with type t = per_file

val normalize : per_file -> per_file
(** Produce an equivalent type environment that has maximal sharing between its structures. *)
