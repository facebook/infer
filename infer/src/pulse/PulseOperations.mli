(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbstractAddress = PulseDomain.AbstractAddress

type t = PulseAbductiveDomain.t

type 'a access_result = ('a, PulseDiagnostic.t) result

module Closures : sig
  val check_captured_addresses :
       Location.t
    -> HilExp.AccessExpression.t
    -> PulseDomain.AbstractAddress.t
    -> t
    -> t access_result
  (** assert the validity of the addresses captured by the lambda *)

  val record :
       Location.t
    -> HilExp.AccessExpression.t
    -> Typ.Procname.t
    -> (AccessPath.base * HilExp.t) list
    -> t
    -> t access_result
  (** record that the access expression points to a lambda with its captured addresses *)
end

module StdVector : sig
  val is_reserved : Location.t -> HilExp.AccessExpression.t -> t -> (t * bool) access_result

  val mark_reserved : Location.t -> HilExp.AccessExpression.t -> t -> t access_result
end

val read :
     Location.t
  -> HilExp.AccessExpression.t
  -> t
  -> (t * (AbstractAddress.t * PulseTrace.t)) access_result

val read_all : Location.t -> HilExp.AccessExpression.t list -> t -> t access_result

val havoc_var : PulseTrace.t -> Var.t -> t -> t

val havoc : PulseTrace.t -> Location.t -> HilExp.AccessExpression.t -> t -> t access_result

val realloc_var : Var.t -> Location.t -> t -> t

val write_var : Var.t -> AbstractAddress.t * PulseTrace.t -> t -> t

val write :
     Location.t
  -> HilExp.AccessExpression.t
  -> AbstractAddress.t * PulseTrace.t
  -> t
  -> t access_result

val invalidate :
     PulseInvalidation.t PulseTrace.action
  -> Location.t
  -> HilExp.AccessExpression.t
  -> t
  -> t access_result

val invalidate_array_elements :
     PulseInvalidation.t PulseTrace.action
  -> Location.t
  -> HilExp.AccessExpression.t
  -> t
  -> t access_result

val remove_vars : Var.t list -> t -> t

val check_address_escape :
  Location.t -> Procdesc.t -> AbstractAddress.t -> PulseTrace.t -> t -> t access_result

module Interproc : sig
  val call :
       Typ.Procname.t
    -> formals:Var.t list
    -> ret:AccessPath.base
    -> actuals:HilExp.t list
    -> CallFlags.t
    -> Location.t
    -> t
    -> PulseSummary.t
    -> PulseAbductiveDomain.t list access_result
end
