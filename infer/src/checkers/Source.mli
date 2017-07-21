(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val all_formals_untainted : Procdesc.t -> (Mangled.t * Typ.t * 'a option) list
(** specify that all the formals of the procdesc are not tainted *)

module type Kind = sig
  include TraceElem.Kind

  val unknown : t
  (** kind of an unknown source *)

  val get : Typ.Procname.t -> Tenv.t -> (t * int option) option
  (** return Some (kind) if the procedure is a taint source, None otherwise *)

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
  (** return each formal of the function paired with either Some(kind) if the formal is a taint
      source, or None if the formal is not a taint source *)
end

module type S = sig
  include TraceElem.S

  type spec =
    { source: t  (** type of the returned source *)
    ; index: int option  (** index of the returned source if Some; return value if None *) }

  val is_footprint : t -> bool
  (** return true if the current source is a footprint source *)

  val make_footprint : AccessPath.t -> Procdesc.t -> t
  (** create a footprint source for the value read from the given access path. *)

  val get_footprint_access_path : t -> AccessPath.t option
  (** return Some(access path) if the current source is a footprint source, None otherwise *)

  val get : CallSite.t -> Tenv.t -> spec option
  (** return Some (taint spec) if the call site is a taint source, None otherwise *)

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
  (** return each formal of the function paired with either Some(source) if the formal is a taint
      source, or None if the formal is not a taint source *)
end

module Make (Kind : Kind) : S with module Kind = Kind

module Dummy : S with type t = unit
