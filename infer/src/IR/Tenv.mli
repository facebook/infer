(*
 * Copyright (c) 2016-present, Facebook, Inc.
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
(** Load a type environment for a source file *)

val store_debug_file_for_source : SourceFile.t -> t -> unit

val load_global : unit -> t option
(** load the global type environment (Java) *)

val store_global : t -> unit
(** save a global type environment (Java) *)

val lookup : t -> Typ.Name.t -> Typ.Struct.t option
(** Look up a name in the global type environment. *)

val mk_struct :
     t
  -> ?default:Typ.Struct.t
  -> ?fields:Typ.Struct.fields
  -> ?statics:Typ.Struct.fields
  -> ?methods:Typ.Procname.t list
  -> ?exported_objc_methods:Typ.Procname.t list
  -> ?supers:Typ.Name.t list
  -> ?annots:Annot.Item.t
  -> Typ.Name.t
  -> Typ.Struct.t
(** Construct a struct_typ, normalizing field types *)

val add_field : t -> Typ.Name.t -> Typ.Struct.field -> unit
(** Add a field to a given struct in the global type environment. *)

val pp : Format.formatter -> t -> unit [@@warning "-32"]
(** print a type environment *)

type per_file = Global | FileLocal of t

val pp_per_file : Format.formatter -> per_file -> unit
  [@@warning "-32"]
(** print per file type environment *)

val merge : src:per_file -> dst:per_file -> per_file
(** Best-effort merge of [src] into [dst]. If a procedure is both in [dst] and [src], the one in
   [src] will get overwritten. *)

module SQLite : SqliteUtils.Data with type t = per_file
