(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Kind = sig
  include TaintTraceElem.Kind

  val get :
    caller_pname:Procname.t -> Procname.t -> HilExp.t list -> Tenv.t -> (t * int option) list
  (** return Some (kind) if the procedure with the given actuals is a taint source, None otherwise *)

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
  (** return each formal of the function paired with either Some(kind) if the formal is a taint
      source, or None if the formal is not a taint source *)
end

module type S = sig
  include TaintTraceElem.S

  type spec =
    { source: t  (** type of the returned source *)
    ; index: int option  (** index of the returned source if Some; return value if None *) }

  val get : caller_pname:Procname.t -> CallSite.t -> HilExp.t list -> Tenv.t -> spec list
  (** return Some (taint spec) if the call site with the given actuals is a taint source, None
      otherwise *)

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
  (** return each formal of the function paired with either Some(source) if the formal is a taint
      source, or None if the formal is not a taint source *)
end

module Dummy : S with type t = unit
