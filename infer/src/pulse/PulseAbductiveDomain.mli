(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module BaseAddressAttributes = PulseBaseAddressAttributes
module BaseDomain = PulseBaseDomain
module BaseMemory = PulseBaseMemory
module BaseStack = PulseBaseStack

(** Layer on top of {!BaseDomain} to propagate operations on the current state to the pre-condition
    when necessary

    The abstract type [t] is a pre/post pair in the style of biabduction. *)

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (** filter both heap and attrs *)

  val filter_addr_with_discarded_attrs : f:(AbstractValue.t -> bool) -> t -> t * Attributes.t list
  (** filter both heap and attrs with returning discarded attrs together *)

  val pp : F.formatter -> t -> unit
end

(** The post abstract state at each program point, or current state. *)
module PostDomain : BaseDomainSig

(** The inferred pre-condition at each program point, biabduction style.

    NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted
    lattice of [Domain], but since we never actually join states or check implication the two
    collapse into one. * *)
module PreDomain : BaseDomainSig

(** biabduction-style pre/post state + skipped calls *)
type t = private
  { post: PostDomain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred pre at the current program point *)
  ; skipped_calls: SkippedCalls.t  (** set of skipped calls *)
  ; path_condition: PathCondition.t  (** arithmetic facts *) }

val leq : lhs:t -> rhs:t -> bool

val pp : Format.formatter -> t -> unit

val mk_initial : Procdesc.t -> t

val get_pre : t -> BaseDomain.t

val get_post : t -> BaseDomain.t

(** stack operations like {!BaseStack} but that also take care of propagating facts to the
    precondition *)
module Stack : sig
  val add : Var.t -> BaseStack.value -> t -> t

  val remove_vars : Var.t list -> t -> t

  val fold : (Var.t -> BaseStack.value -> 'a -> 'a) -> t -> 'a -> 'a

  val find_opt : Var.t -> t -> BaseStack.value option

  val eval : ValueHistory.t -> Var.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** return the value of the variable in the stack or create a fresh one if needed *)

  val mem : Var.t -> t -> bool

  val exists : (Var.t -> BaseStack.value -> bool) -> t -> bool
end

(** memory operations like {!BaseMemory} but that also take care of propagating facts to the
    precondition *)
module Memory : sig
  module Access = BaseMemory.Access
  module Edges = BaseMemory.Edges

  val add_edge :
       AbstractValue.t * ValueHistory.t
    -> Access.t
    -> AbstractValue.t * ValueHistory.t
    -> Location.t
    -> t
    -> t

  val eval_edge :
    AbstractValue.t * ValueHistory.t -> Access.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** [eval_edge (addr,hist) access astate] follows the edge [addr --access--> .] in memory and
      returns what it points to or creates a fresh value if that edge didn't exist. *)

  val find_opt : AbstractValue.t -> t -> BaseMemory.Edges.t option

  val find_edge_opt : AbstractValue.t -> Access.t -> t -> (AbstractValue.t * ValueHistory.t) option
end

(** attribute operations like {!BaseAddressAttributes} but that also take care of propagating facts
    to the precondition *)
module AddressAttributes : sig
  val abduce_and_add : AbstractValue.t -> Attributes.t -> t -> t
  (** add the attributes to both the current state and, if meaningful, the pre *)

  val add_one : AbstractValue.t -> Attribute.t -> t -> t
  (** add the attribute only to the post *)

  val check_valid : Trace.t -> AbstractValue.t -> t -> (t, Invalidation.t * Trace.t) result

  val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val allocate : Procname.t -> AbstractValue.t * ValueHistory.t -> Location.t -> t -> t

  val add_dynamic_type : Typ.Name.t -> AbstractValue.t -> t -> t

  val remove_allocation_attr : AbstractValue.t -> t -> t

  val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

  val is_end_iterator : AbstractValue.t -> t -> bool

  val is_std_vector_reserved : AbstractValue.t -> t -> bool

  val std_vector_reserve : AbstractValue.t -> t -> t

  val find_opt : AbstractValue.t -> t -> Attributes.t option
end

val is_local : Var.t -> t -> bool

val find_post_cell_opt : AbstractValue.t -> t -> BaseDomain.cell option

val discard_unreachable : t -> t * AbstractValue.Set.t * Attributes.t list
(** [discard_unreachable astate] garbage collects unreachable addresses in the state to make it
    smaller, and retuns the new state, the live addresses, and the attributes of discarded addresses *)

val add_skipped_call : Procname.t -> Trace.t -> t -> t

val add_skipped_calls : SkippedCalls.t -> t -> t

val set_path_condition : PathCondition.t -> t -> t

val of_post : Procdesc.t -> t -> t

val set_post_edges : AbstractValue.t -> BaseMemory.Edges.t -> t -> t
(** directly set the edges for the given address, bypassing abduction altogether *)

val set_post_cell : AbstractValue.t * ValueHistory.t -> BaseDomain.cell -> Location.t -> t -> t
(** directly set the edges and attributes for the given address, bypassing abduction altogether *)
