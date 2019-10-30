(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PulseBasicInterface
module BaseDomain = PulseBaseDomain
module BaseMemory = PulseBaseMemory
module BaseStack = PulseBaseStack

(* layer on top of {!BaseDomain} to propagate operations on the current state to the pre-condition
   when necessary

   The abstract type [t] is a pre/post pair in the style of biabduction.
*)

include AbstractDomain.NoJoin

val mk_initial : Procdesc.t -> t

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

  val abduce_attribute : AbstractValue.t -> Attribute.t -> t -> t
  (** add the attribute to the pre, if meaningful (does not modify the post) *)

  val add_attribute : AbstractValue.t -> Attribute.t -> t -> t
  (** add the attribute only to the post *)

  val add_edge :
       AbstractValue.t * ValueHistory.t
    -> Access.t
    -> AbstractValue.t * ValueHistory.t
    -> Location.t
    -> t
    -> t

  val check_valid : Trace.t -> AbstractValue.t -> t -> (t, Invalidation.t * Trace.t) result

  val find_opt : AbstractValue.t -> t -> BaseMemory.cell option

  val find_edge_opt : AbstractValue.t -> Access.t -> t -> (AbstractValue.t * ValueHistory.t) option

  val set_cell : AbstractValue.t * ValueHistory.t -> BaseMemory.cell -> Location.t -> t -> t

  val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val get_closure_proc_name : AbstractValue.t -> t -> Typ.Procname.t option

  val is_std_vector_reserved : AbstractValue.t -> t -> bool

  val std_vector_reserve : AbstractValue.t -> t -> t

  val eval_edge :
    AbstractValue.t * ValueHistory.t -> Access.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** [eval_edge (addr,hist) access astate] follows the edge [addr --access--> .] in memory and
      returns what it points to or creates a fresh value if that edge didn't exist.  *)

  val get_arithmetic : AbstractValue.t -> t -> (Arithmetic.t * Trace.t) option
end

val is_local : Var.t -> t -> bool

module PrePost : sig
  type domain_t = t

  type t = private domain_t

  val pp : Format.formatter -> t -> unit

  val of_post : Procdesc.t -> domain_t -> t

  val apply :
       Typ.Procname.t
    -> Location.t
    -> t
    -> formals:Var.t list
    -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
    -> domain_t
    -> ((domain_t * (AbstractValue.t * ValueHistory.t) option) option, Diagnostic.t) result
  (** return the abstract state after the call along with an optional return value, or [None] if
      the precondition could not be satisfied (e.g. some aliasing constraints were not satisfied) *)
end

val discard_unreachable : t -> t
(** garbage collect unreachable addresses in the state to make it smaller, just for convenience and
    keep its size down *)

val extract_pre : PrePost.t -> BaseDomain.t

val extract_post : PrePost.t -> BaseDomain.t
