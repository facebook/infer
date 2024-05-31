(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type call_state

type contradiction = private
  | Aliasing of
      { addr_caller: AbstractValue.t
      ; addr_callee: AbstractValue.t
      ; addr_callee': AbstractValue.t
      ; call_state: call_state }
      (** raised when the precondition and the current state disagree on the aliasing, i.e. some
          addresses [callee_addr] and [callee_addr'] that are distinct in the pre are aliased to a
          single address [caller_addr] in the caller's current state. Typically raised when calling
          [foo(z,z)] where the spec for [foo(x,y)] says that [x] and [y] are disjoint. We only raise
          this information if we have found this alias through a heap path that is not supported by
          our current abstraction (like array accesses). *)
  | AliasingWithAllAliases of Specialization.HeapPath.t list list
      (** similar to [Aliasing] case above but we have collected in a list of alias classes all
          alias information before raising and all of them rely on heap paths that we support. *)
  | DynamicTypeNeeded of AbstractValue.t Specialization.HeapPath.Map.t
      (** A map [path -> value] such that each path leads to a value (in the caller space) that
          requires dynamic type specialization *)
  | CapturedFormalActualLength of
      { captured_formals: (Pvar.t * Typ.t) list
      ; captured_actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list }
  | FormalActualLength of
      {formals: (Pvar.t * Typ.t) list; actuals: ((AbstractValue.t * ValueHistory.t) * Typ.t) list}
  | PathCondition

val is_dynamic_type_needed_contradiction :
  contradiction -> AbstractValue.t Specialization.HeapPath.Map.t option

val merge_contradictions : contradiction option -> contradiction option -> contradiction option
(** applying a summary in the caller context may lead to a contradiction; if the summary is a
    non-trivial list of disjuncts, we will merge all possible contradictions, in each disjunct, into
    a single one, using this [merge_contradictions] function. *)

val apply_summary :
     _ InterproceduralAnalysis.t
  -> PathContext.t
  -> callee_proc_name:Procname.t
  -> Location.t
  -> callee_summary:AbductiveDomain.Summary.t
  -> captured_formals:(Pvar.t * Typ.t) list
  -> captured_actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals:(Pvar.t * Typ.t) list
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> AbductiveDomain.t
  -> ( AbductiveDomain.t
     * (AbstractValue.t * ValueHistory.t) option
     * (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
     * ValueHistory.t CellId.Map.t )
     AccessResult.t
     SatUnsat.t
     * contradiction option
(** result of applying one pre/post pair of a callee's summary:

    - {!PulseSatUnsat.Unsat} if that path in the callee is infeasible
    - otherwise, there can be an error detected
    - otherwise, the result is a new abstract state, an optional return value, and a substitution
      [callee_abstract_value -> caller_abstract_value] mapping callee's abstract values to what they
      became in the new (caller) state *)
