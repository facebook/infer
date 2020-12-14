(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type t = AbductiveDomain.t

type 'a access_result = 'a PulseReport.access_result

type access_mode =
  | Read
  | Write
  | NoAccess
      (** The initialized-ness of the address is not checked when it evaluates a heap address
          without actual memory access, for example, when evaluating [&x.f] we need to check
          initialized-ness of [x], not that of [x.f]. *)

val check_addr_access :
  access_mode -> Location.t -> AbstractValue.t * ValueHistory.t -> t -> t access_result
(** Check that the [address] is not known to be invalid *)

module Closures : sig
  val check_captured_addresses : Location.t -> AbstractValue.t -> t -> (t, Diagnostic.t * t) result
  (** assert the validity of the addresses captured by the lambda *)
end

val eval :
  access_mode -> Location.t -> Exp.t -> t -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Use the stack and heap to evaluate the given expression down to an abstract address representing
    its value.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)

val prune : Location.t -> condition:Exp.t -> t -> t access_result

val eval_deref : Location.t -> Exp.t -> t -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Like [eval] but evaluates [*exp]. *)

val eval_access :
     access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** Like [eval] but starts from an address instead of an expression, checks that it is valid, and if
    so dereferences it according to the access. *)

val havoc_id : Ident.t -> ValueHistory.t -> t -> t

val havoc_field :
     Location.t
  -> AbstractValue.t * ValueHistory.t
  -> Fieldname.t
  -> ValueHistory.t
  -> t
  -> t access_result

val realloc_pvar : Pvar.t -> Typ.t -> Location.t -> t -> t

val write_id : Ident.t -> AbstractValue.t * ValueHistory.t -> t -> t

val write_field :
     Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> Fieldname.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t access_result
(** write the edge [ref --.field--> obj] *)

val write_arr_index :
     Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> index:AbstractValue.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t access_result
(** write the edge [ref\[index\]--> obj] *)

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

val allocate : Procname.t -> Location.t -> AbstractValue.t * ValueHistory.t -> t -> t

val add_dynamic_type : Typ.Name.t -> AbstractValue.t -> t -> t

val remove_allocation_attr : AbstractValue.t -> t -> t

val invalidate_access :
     Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> t access_result
(** record that what the address points via the access to is invalid *)

val invalidate_array_elements :
  Location.t -> Invalidation.t -> AbstractValue.t * ValueHistory.t -> t -> t access_result
(** record that all the array elements that address points to is invalid *)

val shallow_copy :
     Location.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) access_result
(** returns the address of a new cell with the same edges as the original *)

val get_dynamic_type_unreachable_values : Var.t list -> t -> (Var.t * Typ.Name.t) list
(** Given a list of variables, computes the unreachable values if the variables were removed from
    the stack, then return the dynamic types of those values if they are available *)

val remove_vars : Var.t list -> Location.t -> t -> t access_result

val check_address_escape :
  Location.t -> Procdesc.t -> AbstractValue.t -> ValueHistory.t -> t -> t access_result

val call :
     caller_proc_desc:Procdesc.t
  -> Errlog.t
  -> callee_data:(Procdesc.t * PulseSummary.t) option
  -> Location.t
  -> Procname.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> t
  -> ExecutionDomain.t list access_result
(** perform an interprocedural call: apply the summary for the call proc name passed as argument if
    it exists *)

val unknown_call :
     Location.t
  -> CallEvent.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> t
  -> t
(** performs a call to a function with no summary by optimistically havoc'ing the by-ref actuals and
    the return value as appropriate *)

val conservatively_initialize_args : AbstractValue.t list -> t -> t
