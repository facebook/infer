(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** List of annotations that should be considered aliases of @ThreadSafe *)
module AnnotationAliases : sig
  val of_json : Yojson.Basic.json -> string list
end

module Models : sig
  type lock = Lock | Unlock | LockedIfTrue | NoEffect

  type thread = BackgroundThread | MainThread | MainThreadIfTrue | UnknownThread

  type container_access = ContainerRead | ContainerWrite

  (* TODO: clean this up so it takes only a procname *)

  val is_thread_utils_method : string -> Typ.Procname.t -> bool
  (** return true if the given method name is a utility class for checking what thread we're on *)

  val get_lock : Typ.Procname.t -> HilExp.t list -> lock
  (** describe how this procedure behaves with respect to locking *)

  val get_thread : Typ.Procname.t -> thread
  (** describe how this procedure behaves with respect to thread access *)

  val get_container_access : Typ.Procname.t -> Tenv.t -> container_access option
  (** return Some (access) if this procedure accesses the contents of a container (e.g., Map.get) *)

  val should_skip : Typ.Procname.t -> bool
  (** holds of procedure names which should not be analyzed in order to avoid known sources of
      inaccuracy *)
end
