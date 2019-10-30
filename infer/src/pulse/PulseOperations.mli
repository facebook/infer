(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type t = PulseAbductiveDomain.t

type 'a access_result = ('a, Diagnostic.t) result

module Closures : sig
  val check_captured_addresses : Location.t -> AbstractValue.t -> t -> (t, Diagnostic.t) result
  (** assert the validity of the addresses captured by the lambda *)
end

val eval : Location.t -> Exp.t -> t -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Use the stack and heap to evaluate the given expression down to an abstract address representing
    its value.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)

val prune :
     is_then_branch:bool
  -> Sil.if_kind
  -> Location.t
  -> condition:Exp.t
  -> t
  -> (t * bool) access_result

val eval_deref : Location.t -> Exp.t -> t -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Like [eval] but evaluates [*exp]. *)

val eval_access :
     Location.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Like [eval] but starts from an address instead of an expression, checks that it is valid, and if
    so dereferences it according to the access. *)

val havoc_id : Ident.t -> ValueHistory.t -> t -> t

val havoc_deref :
  Location.t -> AbstractValue.t * ValueHistory.t -> ValueHistory.t -> t -> t access_result

val havoc_field :
     Location.t
  -> AbstractValue.t * ValueHistory.t
  -> Typ.Fieldname.t
  -> ValueHistory.t
  -> t
  -> t access_result

val realloc_pvar : Pvar.t -> Location.t -> t -> t

val write_id : Ident.t -> AbstractValue.t * ValueHistory.t -> t -> t

val write_deref :
     Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t access_result
(** write the edge [ref --*--> obj] *)

val invalidate :
  Location.t -> Invalidation.t -> AbstractValue.t * ValueHistory.t -> t -> t access_result
(** record that the address is invalid *)

val invalidate_deref :
  Location.t -> Invalidation.t -> AbstractValue.t * ValueHistory.t -> t -> t access_result
(** record that what the address points to is invalid *)

val invalidate_array_elements :
  Location.t -> Invalidation.t -> AbstractValue.t * ValueHistory.t -> t -> t access_result
(** record that all the array elements that address points to is invalid *)

val shallow_copy :
     Location.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** returns the address of a new cell with the same edges as the original *)

val remove_vars : Var.t list -> Location.t -> t -> t

val check_address_escape :
  Location.t -> Procdesc.t -> AbstractValue.t -> ValueHistory.t -> t -> t access_result

val call :
     caller_summary:Summary.t
  -> Location.t
  -> Typ.Procname.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> t
  -> t list access_result
(** perform an interprocedural call: apply the summary for the call proc name passed as argument if
    it exists *)
