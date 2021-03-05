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
  type t = private BaseDomain.t [@@deriving compare, equal, yojson_of]

  val yojson_of_t : t -> Yojson.Safe.t

  val empty : t

  val update : ?stack:BaseStack.t -> ?heap:BaseMemory.t -> ?attrs:BaseAddressAttributes.t -> t -> t

  val filter_addr : f:(AbstractValue.t -> bool) -> t -> t
  (** filter both heap and attrs *)

  val filter_addr_with_discarded_addrs :
    f:(AbstractValue.t -> bool) -> t -> t * AbstractValue.t list
  (** compute new state containing only reachable addresses in its heap and attributes, as well as
      the list of discarded unreachable addresses *)

  val subst_var : AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t

  val pp : F.formatter -> t -> unit
end

(** The post abstract state at each program point, or current state. *)
module PostDomain : BaseDomainSig

(** The inferred pre-condition at each program point, biabduction style.

    NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted
    lattice of [Domain], but since we never actually join states or check implication the two
    collapse into one. * *)
module PreDomain : BaseDomainSig

(** Execution status, similar to {!PulseExecutionDomain} but for ISL (Incorrectness Separation
    Logic) mode, where {!PulseExecutionDomain.ContinueProgram} can also contain "error specs" that
    describe what happens when some addresses are invalid explicitly instead of relying on
    [MustBeValid] attributes. *)
type isl_status =
  | ISLOk  (** ok triple: the program executes without error *)
  | ISLError
      (** Error specification: an invalid address recorded in the precondition will cause an error *)
[@@deriving equal]

(** pre/post on a single program path *)
type t = private
  { post: PostDomain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred procedure pre-condition leading to the current program point *)
  ; path_condition: PathCondition.t
        (** arithmetic facts true along the path (holding for both [pre] and [post] since abstract
            values are immutable) *)
  ; topl: PulseTopl.state
        (** state at of the Topl monitor at the current program point, when Topl is enabled *)
  ; skipped_calls: SkippedCalls.t  (** metadata: procedure calls for which no summary was found *)
  ; isl_status: isl_status }
[@@deriving equal]

val leq : lhs:t -> rhs:t -> bool

val pp : Format.formatter -> t -> unit

val set_isl_status : isl_status -> t -> t

val mk_initial : Tenv.t -> Procdesc.t -> t

val get_pre : t -> BaseDomain.t

val get_post : t -> BaseDomain.t

val simplify_instanceof : Tenv.t -> t -> t

(** stack operations like {!BaseStack} but that also take care of propagating facts to the
    precondition *)
module Stack : sig
  val add : Var.t -> BaseStack.value -> t -> t

  val remove_vars : Var.t list -> t -> t

  val fold : (Var.t -> BaseStack.value -> 'a -> 'a) -> t -> 'a -> 'a

  val find_opt : Var.t -> t -> BaseStack.value option

  val eval : Location.t -> ValueHistory.t -> Var.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** return the value of the variable in the stack or create a fresh one if needed *)

  val mem : Var.t -> t -> bool

  val exists : (Var.t -> BaseStack.value -> bool) -> t -> bool

  val keys : t -> Var.t list
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

  val add_attrs : AbstractValue.t -> Attributes.t -> t -> t

  val check_valid : Trace.t -> AbstractValue.t -> t -> (t, Invalidation.t * Trace.t) result

  val check_initialized : Trace.t -> AbstractValue.t -> t -> (t, unit) result

  val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val allocate : Procname.t -> AbstractValue.t * ValueHistory.t -> Location.t -> t -> t

  val add_dynamic_type : Typ.t -> AbstractValue.t -> t -> t

  val remove_allocation_attr : AbstractValue.t -> t -> t

  val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

  val is_end_of_collection : AbstractValue.t -> t -> bool

  val mark_as_end_of_collection : AbstractValue.t -> t -> t

  val is_std_vector_reserved : AbstractValue.t -> t -> bool

  val std_vector_reserve : AbstractValue.t -> t -> t

  val find_opt : AbstractValue.t -> t -> Attributes.t option

  val check_valid_isl :
       Trace.t
    -> AbstractValue.t
    -> ?null_noop:bool
    -> t
    -> (t list, Invalidation.t * Trace.t * t) result
end

val is_local : Var.t -> t -> bool

val find_post_cell_opt : AbstractValue.t -> t -> BaseDomain.cell option

val discard_unreachable : t -> t * AbstractValue.Set.t * AbstractValue.t list
(** garbage collect unreachable addresses in the state to make it smaller and return the new state,
    the live addresses, and the discarded addresses that used to have attributes attached *)

val add_skipped_call : Procname.t -> Trace.t -> t -> t

val add_skipped_calls : SkippedCalls.t -> t -> t

val set_path_condition : PathCondition.t -> t -> t

(** private type to make sure {!summary_of_post} is always called when creating summaries *)
type summary = private t [@@deriving compare, equal, yojson_of]

val summary_of_post : Tenv.t -> Procdesc.t -> t -> summary SatUnsat.t
(** trim the state down to just the procedure's interface (formals and globals), and simplify and
    normalize the state *)

val set_post_edges : AbstractValue.t -> BaseMemory.Edges.t -> t -> t
(** directly set the edges for the given address, bypassing abduction altogether *)

val set_post_cell : AbstractValue.t * ValueHistory.t -> BaseDomain.cell -> Location.t -> t -> t
(** directly set the edges and attributes for the given address, bypassing abduction altogether *)

val incorporate_new_eqs : PathCondition.new_eqs -> t -> t
(** Check that the new equalities discovered are compatible with the current pre and post heaps,
    e.g. [x = 0] is not compatible with [x] being allocated, and [x = y] is not compatible with [x]
    and [y] being allocated separately. In those cases, the resulting path condition is
    {!PathCondition.false_}. *)

val initialize : AbstractValue.t -> t -> t
(** Remove "Uninitialized" attribute of the given address *)

val set_uninitialized :
     Tenv.t
  -> [ `LocalDecl of Pvar.t * AbstractValue.t option
       (** the second optional parameter is for the address of the variable *)
     | `Malloc of AbstractValue.t  (** the address parameter is a newly allocated address *) ]
  -> Typ.t
  -> Location.t
  -> t
  -> t
(** Add "Uninitialized" attributes when a variable is declared or a memory is allocated by malloc. *)

module Topl : sig
  val small_step : Location.t -> PulseTopl.event -> t -> t

  val large_step :
       call_location:Location.t
    -> callee_proc_name:Procname.t
    -> substitution:(AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
    -> ?condition:PathCondition.t
    -> callee_prepost:PulseTopl.state
    -> t
    -> t

  val get : summary -> PulseTopl.state
end
