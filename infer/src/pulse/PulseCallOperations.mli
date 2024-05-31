(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val call :
     PulseSummary.t InterproceduralAnalysis.t
  -> PathContext.t
  -> Location.t
  -> Procname.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> call_kind:PulseOperations.call_kind
  -> AbductiveDomain.t
  -> ?call_flags:CallFlags.t
  -> NonDisjDomain.t
  -> ExecutionDomain.t AccessResult.t list
     * NonDisjDomain.t
     * PulseInterproc.contradiction option
     * [`KnownCall | `UnknownCall]
(** perform an interprocedural call: apply the summary for the call proc name passed as argument if
    it exists *)

val unknown_call :
     Tenv.t
  -> PathContext.t
  -> Location.t
  -> CallEvent.t
  -> Procname.t option
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** performs a call to a function with no summary by optimistically havoc'ing the by-ref actuals and
    the return value as appropriate *)

module GlobalForStats : sig
  val init_before_call : unit -> unit

  val is_node_not_stuck : unit -> bool

  val node_is_not_stuck : unit -> unit

  val is_one_call_stuck : unit -> bool

  val one_call_is_stuck : unit -> unit
end
