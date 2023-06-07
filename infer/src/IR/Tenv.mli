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

val load : SourceFile.t -> t option
  [@@alert tenv "Analysis code should use [Exe_env.get_source_tenv] instead."]
(** Load a type environment for a source file *)

val store_debug_file_for_source : SourceFile.t -> t -> unit

val read : DB.filename -> t option
(** Read and return a type environment from the given file *)

val write : t -> DB.filename -> unit
(** Write the type environment into the given file *)

val load_global : unit -> t option
(** Load the global type environment (Java) *)

val store_global : normalize:bool -> t -> unit
(** Save a global type environment (Java/Hack) *)

val lookup : t -> Typ.Name.t -> Struct.t option
(** Look up a name in the given type environment. *)

val mk_struct :
     t
  -> ?default:Struct.t
  -> ?fields:Struct.fields
  -> ?statics:Struct.fields
  -> ?methods:Procname.t list
  -> ?exported_objc_methods:Procname.t list
  -> ?supers:Typ.Name.t list
  -> ?objc_protocols:Typ.Name.t list
  -> ?annots:Annot.Item.t
  -> ?java_class_info:Struct.java_class_info
  -> ?hack_class_info:Struct.Hack.t
  -> ?dummy:bool
  -> ?source_file:SourceFile.t
  -> Typ.Name.t
  -> Struct.t
(** Construct a struct_typ, normalizing field types *)

val add_field : t -> Typ.Name.t -> Struct.field -> unit
(** Add a field to a given struct in the global type environment. *)

val pp : Format.formatter -> t -> unit
(** print a type environment *)

val fold_supers : t -> Typ.Name.t -> init:'a -> f:(Typ.Name.t -> Struct.t option -> 'a -> 'a) -> 'a

val mem_supers : t -> Typ.Name.t -> f:(Typ.Name.t -> Struct.t option -> bool) -> bool

val find_map_supers : t -> Typ.Name.t -> f:(Typ.Name.t -> Struct.t option -> 'a option) -> 'a option

val implements_remodel_class : t -> Typ.Name.t -> bool
(** Check if a class implements the Remodel class *)

type per_file = Global | FileLocal of t

val pp_per_file : Format.formatter -> per_file -> unit
(** print per file type environment *)

val merge : src:t -> dst:t -> unit
(** Merge [src] into [dst] *)

val merge_per_file : src:per_file -> dst:per_file -> per_file
(** Best-effort merge of [src] into [dst]. If a procedure is both in [dst] and [src], the one in
    [dst] will get overwritten. *)

val resolve_method :
     method_exists:(Procname.t -> Procname.t list -> bool)
  -> t
  -> Typ.Name.t
  -> Procname.t
  -> Procname.t option
(** [resolve_method ~method_exists tenv class_name procname] tries to resolve [procname] to a method
    in [class_name] or its super-classes, that is non-virtual (non-Java-interface method).
    [method_exists adapted_procname methods] should check if [adapted_procname] ([procname] but with
    its class potentially changed to some [other_class]) is among the [methods] of [other_class]. *)

val find_cpp_destructor : t -> Typ.Name.t -> Procname.t option

val find_cpp_constructor : t -> Typ.Name.t -> Procname.t list

module SQLite : SqliteUtils.Data with type t = per_file

val normalize : per_file -> per_file
(** Produce an equivalent type environment that has maximal sharing between its structures. *)
