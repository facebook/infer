(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type t = AbductiveDomain.t

val check_addr_access :
     PathContext.t
  -> ?must_be_valid_reason:Invalidation.must_be_valid_reason
  -> access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** Check that the [address] is not known to be invalid *)

module Closures : sig
  val check_captured_addresses :
    PathContext.t -> Location.t -> AbstractValue.t -> t -> t AccessResult.t
  (** assert the validity of the addresses captured by the lambda *)
end

val pulse_model_type : Typ.name
(** Struct type name of "__infer_pulse_model" *)

module ModeledField : sig
  val string_length : Fieldname.t
  (** Modeled field for string length *)

  val internal_string : Fieldname.t
  (** Modeled field for internal string *)

  val internal_ref_count : Fieldname.t
  (** Modeled field for reference_counting *)

  val delegated_release : Fieldname.t
  (** Modeled field for resource release delegation *)
end

val conservatively_initialize_args : AbstractValue.t list -> t -> t
(** Set all reachable values from the given list as initialized conservatively. *)

val eval :
     PathContext.t
  -> access_mode
  -> Location.t
  -> Exp.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t SatUnsat.t
(** Use the stack and heap to evaluate the given expression down to an abstract address representing
    its value.

    Return an error state if it traverses some known invalid address or if the end destination is
    known to be invalid. *)

val eval_var : PathContext.t -> Location.t -> Pvar.t -> t -> t * (AbstractValue.t * ValueHistory.t)
(** Similar to eval but for pvar only. Always succeeds. *)

val eval_structure_isl :
     PathContext.t
  -> access_mode
  -> Location.t
  -> Exp.t
  -> t
  -> (bool * (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t list) AccessResult.t SatUnsat.t
(** Similar to eval but apply to data structures and ISL abduction. Return a list of abduced states
    (ISLOk and ISLErs); The boolean indicates whether it is data structures or not. *)

val prune :
     PathContext.t
  -> Location.t
  -> condition:Exp.t
  -> t
  -> (t * ValueHistory.t) AccessResult.t SatUnsat.t

val eval_deref :
     PathContext.t
  -> ?must_be_valid_reason:Invalidation.must_be_valid_reason
  -> Location.t
  -> Exp.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t SatUnsat.t
(** Like [eval] but evaluates [*exp]. *)

val eval_deref_isl :
     PathContext.t
  -> Location.t
  -> Exp.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t list

val eval_access :
     PathContext.t
  -> ?must_be_valid_reason:Invalidation.must_be_valid_reason
  -> access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t
(** Like [eval] but starts from an address instead of an expression, checks that it is valid, and if
    so dereferences it according to the access. *)

val eval_deref_access :
     PathContext.t
  -> ?must_be_valid_reason:Invalidation.must_be_valid_reason
  -> access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t
(** Like [eval_access] but does additional dereference. *)

val eval_proc_name :
  PathContext.t -> Location.t -> Exp.t -> t -> (t * Procname.t option) AccessResult.t SatUnsat.t

val havoc_id : Ident.t -> ValueHistory.t -> t -> t

val havoc_deref_field :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> Fieldname.t
  -> ValueHistory.t
  -> t
  -> t AccessResult.t
(** Havoc dereferenced field address. *)

val realloc_pvar : Tenv.t -> PathContext.t -> Pvar.t -> Typ.t -> Location.t -> t -> t

val write_id : Ident.t -> AbstractValue.t * ValueHistory.t -> t -> t

val read_id : Ident.t -> t -> (AbstractValue.t * ValueHistory.t) option

val write_field :
     PathContext.t
  -> Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> Fieldname.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** write the edge [ref --.field--> obj] *)

val write_deref_field :
     PathContext.t
  -> Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> Fieldname.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** write the edge [ref --.field--> _ --*--> obj] *)

val write_arr_index :
     PathContext.t
  -> Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> index:AbstractValue.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** write the edge [ref\[index\]--> obj] *)

val write_deref :
     PathContext.t
  -> Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** write the edge [ref --*--> obj] *)

val write_deref_biad_isl :
     PathContext.t
  -> Location.t
  -> ref:AbstractValue.t * ValueHistory.t
  -> AbstractValue.t HilExp.Access.t
  -> obj:AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t list

(** the way that was used to get to the invalidated address in the state; this is used to record the
    invalidation point in its history in addition to inside the [Invalid] attribute *)
type invalidation_access =
  | MemoryAccess of
      { pointer: AbstractValue.t * ValueHistory.t
      ; access: BaseMemory.Access.t
      ; hist_obj_default: ValueHistory.t }
      (** the value was read from the heap following the [access] edge at address [pointer] *)
  | StackAddress of Var.t * ValueHistory.t  (** the value was read from the stack *)
  | UntraceableAccess  (** we don't know where the value came from; avoid using if possible *)

val invalidate :
     PathContext.t
  -> invalidation_access
  -> Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** record that the address is invalid *)

val invalidate_biad_isl :
     PathContext.t
  -> Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t list
(** record that the address is invalid. If the address has not been allocated, abduce ISL specs for
    both invalid (null, free, unint) and allocated heap. *)

val always_reachable : AbstractValue.t -> t -> t

val allocate : Attribute.allocator -> Location.t -> AbstractValue.t -> t -> t

val java_resource_release : recursive:bool -> AbstractValue.t -> t -> t
(** releases the resource of the argument, and recursively calls itself on the delegated resource if
    [recursive==true] *)

val csharp_resource_release : recursive:bool -> AbstractValue.t -> t -> t
(** releases the resource of the argument, and recursively calls itself on the delegated resource if
    [recursive==true] *)

val add_dynamic_type : Typ.t -> AbstractValue.t -> t -> t

val add_ref_counted : AbstractValue.t -> t -> t

val is_ref_counted : AbstractValue.t -> t -> bool

val remove_allocation_attr : AbstractValue.t -> t -> t

val invalidate_access :
     PathContext.t
  -> Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> t AccessResult.t
(** record that what the address points via the access to is invalid *)

val invalidate_deref_access :
     PathContext.t
  -> Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> BaseMemory.Access.t
  -> t
  -> t AccessResult.t
(** Like [invalidate_access] but invalidates dereferenced address. *)

val invalidate_array_elements :
     PathContext.t
  -> Location.t
  -> Invalidation.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> t AccessResult.t
(** record that all the array elements that address points to is invalid *)

val shallow_copy :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> t
  -> (t * (AbstractValue.t * ValueHistory.t)) AccessResult.t
(** returns the address of a new cell with the same edges as the original *)

val get_dynamic_type_unreachable_values : Var.t list -> t -> (Var.t * Typ.t) list
(** Given a list of variables, computes the unreachable values if the variables were removed from
    the stack, then return the dynamic types of those values if they are available *)

val remove_vars : Var.t list -> Location.t -> t -> t

val check_address_escape :
  Location.t -> Procdesc.t -> AbstractValue.t -> ValueHistory.t -> t -> t AccessResult.t

type call_kind =
  [ `Closure of (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
  | `Var of Ident.t
  | `ResolvedProcname ]

val get_captured_actuals :
     Procname.t
  -> PathContext.t
  -> Location.t
  -> captured_formals:(Var.t * CapturedVar.capture_mode * Typ.t) list
  -> call_kind:call_kind
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> t
  -> (t * ((AbstractValue.t * ValueHistory.t) * Typ.t) list) AccessResult.t SatUnsat.t
