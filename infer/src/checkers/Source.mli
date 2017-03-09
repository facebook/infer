(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** specify that all the formals of the procdesc are not tainted *)
val all_formals_untainted : Procdesc.t -> (Mangled.t * Typ.t * 'a option) list

module type Kind = sig
  include TraceElem.Kind

  (** kind of an unknown source *)
  val unknown : t

  (** return Some (kind) if the procedure is a taint source, None otherwise *)
  val get : Typ.Procname.t -> Tenv.t -> t option

  (** return each formal of the function paired with either Some(kind) if the formal is a taint
      source, or None if the formal is not a taint source *)
  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module type S = sig
  include TraceElem.S

  (** return true if the current source is a footprint source *)
  val is_footprint : t -> bool

  (** create a footprint source for the value read from the given access path. *)
  val make_footprint : AccessPath.t -> CallSite.t -> t

  (** return Some(access path) if the current source is a footprint source, None otherwise *)
  val get_footprint_access_path: t -> AccessPath.t option

  (** return Some (source) if the call site is a taint source, None otherwise *)
  val get : CallSite.t -> Tenv.t -> t option

  (** return each formal of the function paired with either Some(source) if the formal is a taint
      source, or None if the formal is not a taint source *)
  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module Make (Kind : Kind) : S with module Kind = Kind

module Dummy : S with type t = unit
