(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module BaseDomain = PulseBaseDomain
module Decompiler = PulseDecompiler
module DecompilerExpr = PulseDecompilerExpr
module PathContext = PulsePathContext

(** {2 Abductive, value-normalizing layer on top of [BaseDomain]}

    The operations in this module take care of two things that are important for the safety of
    domain operations:

    1. Propagate operations on the current state to the pre-condition when necessary. This is the
    "abductive" part of the domain and follows the principles of biabduction as laid out in
    *Compositional Shape Analysis by Means of Bi-Abduction*, JACM, 2011,
    https://doi.org/10.1145/2049697.2049700. Simply put, a read from a memory location that is
    reachable from the precondition for the first time is taken to mean that this memory location
    had better been allocated since the beginning of the function for the operation to be safe,
    hence we "abduce" that this was the case and add it to the pre-condition (unless we already know
    that address to be invalid of course).

    2. Normalize values ([AbstractValue.t]) in and out of the state according to their canonical
    representatives as per the current path condition. The idea is that one can ask about any
    abstract value and functions in this API will normalize it as needed first before querying the
    abstract state, and normalize any value found in the abstract state as needed too. For example,
    values in the raw map corresponding to the memory are not necessarily normalized at all times
    but the API allows one to pretend they are by normalizing them on the fly. On the other hand,
    *keys* in the memory map are always normalized so values must be normalized before being looked
    up in the map and this module takes care of that transparently too. See also [PulseCanonValue]. *)

(** signature common to the "normal" [Domain], representing the post at the current program point,
    and the inverted [PreDomain], representing the inferred pre-condition*)
module type BaseDomainSig = sig
  (* private because the lattice is not the same for preconditions and postconditions so we don't
     want to confuse them *)
  type t = private BaseDomain.t [@@deriving compare, equal, yojson_of]
end

(** The post abstract state at each program point, or current state. *)
module PostDomain : BaseDomainSig

(** The inferred pre-condition at each program point, biabduction style.

    NOTE: [PreDomain] and [Domain] theoretically differ in that [PreDomain] should be the inverted
    lattice of [Domain], but since we never actually join states or check implication the two
    collapse into one. * *)
module PreDomain : BaseDomainSig

(** pre/post on a single program path *)
type t = private
  { post: PostDomain.t  (** state at the current program point*)
  ; pre: PreDomain.t  (** inferred procedure pre-condition leading to the current program point *)
  ; path_condition: Formula.t
        (** arithmetic facts true along the path (holding for both [pre] and [post] since abstract
            values are immutable) *)
  ; decompiler: Decompiler.t
  ; topl: PulseTopl.state
        (** state at of the Topl monitor at the current program point, when Topl is enabled *)
  ; need_dynamic_type_specialization: AbstractValue.Set.t
        (** a set of abstract values that are used as receiver of method calls in the instructions
            reached so far *)
  ; transitive_info: TransitiveInfo.t  (** record transitive information inter-procedurally *)
  ; recursive_calls: PulseMutualRecursion.Set.t
  ; skipped_calls: SkippedCalls.t  (** metadata: procedure calls for which no summary was found *)
  }
[@@deriving equal]

val leq : lhs:t -> rhs:t -> bool

val pp : Format.formatter -> t -> unit

val mk_initial : Tenv.t -> ProcAttributes.t -> t

(** Safe version of {!PulseBaseStack} *)
module Stack : sig
  val add : Var.t -> PulseBaseStack.value -> t -> t

  val remove_vars : Var.t list -> t -> t

  val fold :
    ?pre_or_post:[`Pre | `Post] -> (Var.t -> PulseBaseStack.value -> 'a -> 'a) -> t -> 'a -> 'a
  (** [pre_or_post] defaults to [`Post] *)

  val find_opt : Var.t -> t -> PulseBaseStack.value option

  val eval : ValueHistory.t -> Var.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** return the value of the variable in the stack or create a fresh one if needed *)

  val mem : Var.t -> t -> bool

  val exists : (Var.t -> PulseBaseStack.value -> bool) -> t -> bool

  val keys : t -> Var.t list
end

(** Safe version of {!PulseBaseMemory} *)
module Memory : sig
  val add_edge :
       PathContext.t
    -> AbstractValue.t * ValueHistory.t
    -> PulseAccess.t
    -> AbstractValue.t * ValueHistory.t
    -> Location.t
    -> t
    -> t

  val eval_edge :
    AbstractValue.t * ValueHistory.t -> PulseAccess.t -> t -> t * (AbstractValue.t * ValueHistory.t)
  (** [eval_edge (addr,hist) access astate] follows the edge [addr --access--> .] in memory and
      returns what it points to or creates a fresh value if that edge didn't exist. *)

  val fold_edges :
       ?pre_or_post:[`Pre | `Post]
    -> AbstractValue.t
    -> (t, PulseAccess.t * (AbstractValue.t * ValueHistory.t), _) Container.fold
  (** [pre_or_post] defaults to [`Post] *)

  val find_edge_opt :
    AbstractValue.t -> PulseAccess.t -> t -> (AbstractValue.t * ValueHistory.t) option

  val exists_edge :
       ?pre_or_post:[`Pre | `Post]
    -> AbstractValue.t
    -> t
    -> f:(PulseAccess.t * (AbstractValue.t * ValueHistory.t) -> bool)
    -> bool
  (** [pre_or_post] defaults to [`Post] *)
end

(** Safe version of {!PulseBaseAddressAttributes} *)
module AddressAttributes : sig
  val abduce_one : AbstractValue.t -> Attribute.t -> t -> t
  (** add the attribute to the pre, if the address is in pre *)

  val abduce_all : AbstractValue.t -> Attributes.t -> t -> t
  (** [abduce_one] on each attribute in the set *)

  val add_one : AbstractValue.t -> Attribute.t -> t -> t
  (** add the attribute only to the post *)

  val add_all : AbstractValue.t -> Attributes.t -> t -> t
  (** [add_one] on each attribute in the set *)

  val find_opt : AbstractValue.t -> t -> Attributes.t option

  val check_valid :
       PathContext.t
    -> ?must_be_valid_reason:Invalidation.must_be_valid_reason
    -> Trace.t
    -> AbstractValue.t
    -> t
    -> (t, Invalidation.t * Trace.t) result

  val check_initialized :
    PathContext.t -> Trace.t -> AbstractValue.t -> t -> (t, Attribute.UninitializedTyp.t) result

  val add_taint_sink : PathContext.t -> TaintItem.t -> Trace.t -> AbstractValue.t -> t -> t

  val invalidate : AbstractValue.t * ValueHistory.t -> Invalidation.t -> Location.t -> t -> t

  val initialize : AbstractValue.t -> t -> t
  (** Remove "Uninitialized" attribute of the given address *)

  val set_uninitialized :
       Tenv.t
    -> PathContext.t
    -> [ `LocalDecl of Pvar.t * AbstractValue.t option
         (** the second optional parameter is for the address of the variable *)
       | `Malloc of AbstractValue.t  (** the address parameter is a newly allocated address *) ]
    -> Typ.t
    -> Location.t
    -> t
    -> t
  (** Add "Uninitialized" attributes when a variable is declared or a memory is allocated by malloc. *)

  val always_reachable : AbstractValue.t -> t -> t

  val allocate : Attribute.allocator -> AbstractValue.t -> Location.t -> t -> t

  val java_resource_release : AbstractValue.t -> t -> t

  val hack_async_await : AbstractValue.t -> t -> t

  val remove_hack_builder : AbstractValue.t -> t -> t [@@warning "-unused-value-declaration"]

  val set_hack_builder : AbstractValue.t -> Attribute.Builder.t -> t -> t
  [@@warning "-unused-value-declaration"]

  val get_hack_builder : AbstractValue.t -> t -> Attribute.Builder.t option
  [@@warning "-unused-value-declaration"]

  val is_java_resource_released : AbstractValue.t -> t -> bool

  val csharp_resource_release : AbstractValue.t -> t -> t

  val is_csharp_resource_released : AbstractValue.t -> t -> bool

  val in_reported_retain_cycle : AbstractValue.t -> t -> t

  val is_in_reported_retain_cycle : AbstractValue.t -> t -> bool

  val add_dict_contain_const_keys : AbstractValue.t -> t -> t

  val remove_dict_contain_const_keys : AbstractValue.t -> t -> t

  val is_dict_contain_const_keys : AbstractValue.t -> t -> bool

  val add_dict_read_const_key : Timestamp.t -> Trace.t -> AbstractValue.t -> Fieldname.t -> t -> t

  val add_static_type : Tenv.t -> Typ.name -> AbstractValue.t -> Location.t -> t -> t

  val remove_allocation_attr : AbstractValue.t -> t -> t

  val remove_taint_attrs : AbstractValue.t -> t -> t

  val get_allocation_attr : AbstractValue.t -> t -> (Attribute.allocator * Trace.t) option

  val get_static_type : AbstractValue.t -> t -> Typ.Name.t option

  val get_closure_proc_name : AbstractValue.t -> t -> Procname.t option

  val get_copied_into : AbstractValue.t -> t -> Attribute.CopiedInto.t option

  val get_copied_return :
    AbstractValue.t -> t -> (AbstractValue.t * bool * Attribute.CopyOrigin.t * Location.t) option

  val remove_copied_return : AbstractValue.t -> t -> t

  val get_source_origin_of_copy : AbstractValue.t -> t -> AbstractValue.t option

  val get_taint_sources_and_sanitizers :
    AbstractValue.t -> t -> Attribute.TaintedSet.t * Attribute.TaintSanitizedSet.t

  val get_propagate_taint_from :
    AbstractValue.t -> t -> (Attribute.taint_propagation_reason * Attribute.taint_in list) option

  val is_end_of_collection : AbstractValue.t -> t -> bool

  val mark_as_end_of_collection : AbstractValue.t -> t -> t

  val is_std_vector_reserved : AbstractValue.t -> t -> bool

  val get_last_lookup : AbstractValue.t -> t -> AbstractValue.t option

  val std_vector_reserve : AbstractValue.t -> t -> t

  val add_copied_return :
       AbstractValue.t
    -> source:AbstractValue.t
    -> is_const_ref:bool
    -> Attribute.CopyOrigin.t
    -> Location.t
    -> t
    -> t

  val get_config_usage : AbstractValue.t -> t -> Attribute.ConfigUsage.t option

  val get_valid_returned_from_unknown : AbstractValue.t -> t -> AbstractValue.t list option

  val get_written_to : AbstractValue.t -> t -> (Timestamp.t * Trace.t) option

  val is_copied_from_const_ref : AbstractValue.t -> t -> bool

  val is_std_moved : AbstractValue.t -> t -> bool

  val get_address_of_stack_variable :
    AbstractValue.t -> t -> (Var.t * Location.t * ValueHistory.t) option

  val has_unknown_effect : AbstractValue.t -> t -> bool

  val is_hack_sinit_called : AbstractValue.t -> t -> bool
end

val should_havoc_if_unknown : unit -> [> `ShouldHavoc | `ShouldOnlyHavocResources]

val apply_unknown_effect :
     ?havoc_filter:(AbstractValue.t -> PulseAccess.t -> AbstractValue.t * ValueHistory.t -> bool)
  -> ValueHistory.t
  -> AbstractValue.t
  -> t
  -> t
(** do as much as possible to assume "the best" happened at that location: deallocate and initialize
    everything reachable from the address, then havoc all the edges starting from the address
    passing [havoc_filter] (by default everything passes the filter) *)

val is_local : Var.t -> t -> bool

val find_post_cell_opt : AbstractValue.t -> t -> BaseDomain.cell option

val fold_all :
     ?var_filter:(Var.t -> bool)
  -> init:'accum
  -> finish:('accum -> 'final)
  -> f:(Var.t -> 'accum -> AbstractValue.t -> Access.t list -> ('accum, 'final) Continue_or_stop.t)
  -> ?f_revisit:(Var.t -> 'accum -> AbstractValue.t -> Access.t list -> 'accum)
  -> t
  -> [`Post | `Pre]
  -> 'final

val reachable_addresses_from :
     ?edge_filter:(Access.t -> bool)
  -> AbstractValue.t Seq.t
  -> t
  -> [`Pre | `Post]
  -> AbstractValue.Set.t
(** Compute the set of abstract addresses that are reachable from given abstract addresses. *)

val get_unreachable_attributes : t -> AbstractValue.t list
(** collect the addresses that have attributes but are unreachable in the current post-condition *)

val mark_potential_leaks : Location.t -> dead_roots:Var.t list -> t -> t

val add_recursive_call : Location.t -> Procname.t -> t -> t * PulseMutualRecursion.t

val add_recursive_calls : PulseMutualRecursion.Set.t -> t -> t

val add_skipped_call : Procname.t -> Trace.t -> t -> t

val add_skipped_calls : SkippedCalls.t -> t -> t

val add_missed_captures : Typ.Name.Set.t -> t -> t

val set_path_condition : Formula.t -> t -> t

val record_transitive_access : Location.t -> t -> t

val record_call_resolution :
     caller:Procdesc.t
  -> Location.t
  -> TransitiveInfo.Callees.call_kind
  -> TransitiveInfo.Callees.resolution
  -> t
  -> t

val add_need_dynamic_type_specialization : AbstractValue.t -> t -> t

val map_decompiler : t -> f:(Decompiler.t -> Decompiler.t) -> t

val add_block_source : AbstractValue.t -> string -> t -> t

val set_post_edges : AbstractValue.t -> PulseBaseMemory.Edges.t -> t -> t
(** directly set the edges for the given address, bypassing abduction altogether *)

val set_post_cell :
  PathContext.t -> AbstractValue.t * ValueHistory.t -> BaseDomain.cell -> Location.t -> t -> t
(** directly set the edges and attributes for the given address, bypassing abduction altogether *)

val incorporate_new_eqs :
     Formula.new_eqs
  -> t
  -> ( t
     , [> `PotentialInvalidAccess of
          t * AbstractValue.t * (Trace.t * Invalidation.must_be_valid_reason option) ] )
     result
     SatUnsat.t
(** Check that the new equalities discovered are compatible with the current pre and post heaps,
    e.g. [x = 0] is not compatible with [x] being allocated, and [x = y] is not compatible with [x]
    and [y] being allocated separately. In those cases, the result is [Unsat]. *)

val incorporate_new_eqs_on_val : Formula.new_eqs -> AbstractValue.t -> AbstractValue.t
(** Similar to [incorporate_new_eqs], but apply to an abstract value. *)

module Summary : sig
  (** private type to make sure {!of_post} is always called when creating summaries and that it is
      not called twice *)
  type summary = private t [@@deriving compare, equal, yojson_of]

  val add_need_dynamic_type_specialization : AbstractValue.t -> summary -> summary

  val of_post :
       Procname.t
    -> ProcAttributes.t
    -> Location.t
    -> t
    -> ( summary
       , [> `JavaResourceLeak of summary * t * JavaClassName.t * Trace.t * Location.t
         | `HackUnawaitedAwaitable of summary * t * Trace.t * Location.t
         | `HackUnfinishedBuilder of summary * t * Trace.t * Location.t * HackClassName.t
         | `CSharpResourceLeak of summary * t * CSharpClassName.t * Trace.t * Location.t
         | `MemoryLeak of summary * t * Attribute.allocator * Trace.t * Location.t
         | `PotentialInvalidAccessSummary of
           summary * t * DecompilerExpr.t * (Trace.t * Invalidation.must_be_valid_reason option) ]
       )
       result
       SatUnsat.t
  (** Trim the state down to just the procedure's interface (formals and globals), and simplify and
      normalize the state. *)

  val leq : lhs:summary -> rhs:summary -> bool

  val pp : F.formatter -> summary -> unit

  val get_pre : summary -> BaseDomain.t

  val get_post : summary -> BaseDomain.t

  val get_path_condition : summary -> Formula.t

  val get_topl : summary -> PulseTopl.state

  val heap_paths_that_need_dynamic_type_specialization :
    summary -> AbstractValue.t Specialization.HeapPath.Map.t

  val get_recursive_calls : summary -> PulseMutualRecursion.Set.t

  val get_skipped_calls : summary -> SkippedCalls.t

  val is_heap_allocated : summary -> AbstractValue.t -> bool
  (** whether the abstract value provided has edges in the pre or post heap *)

  val get_must_be_valid :
       AbstractValue.t
    -> summary
    -> (Timestamp.t * Trace.t * Invalidation.must_be_valid_reason option) option

  val remove_all_must_not_be_tainted : ?kinds:TaintConfig.Kind.Set.t -> summary -> summary

  val pre_heap_has_assumptions : summary -> bool
  (** whether the pre heap encodes some assumptions about values: either a value is restricted (>=
      0) or there is sharing in the heap. Both represent implicit assumptions that the program must
      have made. *)

  type t = summary [@@deriving compare, equal, yojson_of]

  val get_transitive_info : t -> TransitiveInfo.t
end

val transfer_transitive_info_to_caller : Procname.t -> Location.t -> Summary.t -> t -> t

module Topl : sig
  val small_step : Tenv.t -> Location.t -> PulseTopl.event -> t -> t

  val large_step :
       call_location:Location.t
    -> callee_proc_name:Procname.t
    -> substitution:(AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
    -> callee_summary:PulseTopl.state
    -> callee_is_manifest:bool
    -> t
    -> t

  val report_errors : Procdesc.t -> Errlog.t -> pulse_is_manifest:bool -> Summary.t -> unit
end

(** see {!PulseCanonValue} *)
module CanonValue : sig
  include PulseCanonValue.S with type astate = t

  val downcast : t -> AbstractValue.t [@@inline always]
end
