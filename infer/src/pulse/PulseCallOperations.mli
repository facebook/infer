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

val call :
     Tenv.t
  -> PathContext.t
  -> caller_proc_desc:Procdesc.t
  -> callee_data:PulseSummary.t option
  -> Location.t
  -> Procname.t
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> call_kind:PulseOperations.call_kind
  -> t
  -> ExecutionDomain.t AccessResult.t list * PulseInterproc.contradiction option
(** perform an interprocedural call: apply the summary for the call proc name passed as argument if
    it exists *)

val unknown_call :
     PathContext.t
  -> Location.t
  -> CallEvent.t
  -> Procname.t option
  -> ret:Ident.t * Typ.t
  -> actuals:((AbstractValue.t * ValueHistory.t) * Typ.t) list
  -> formals_opt:(Pvar.t * Typ.t) list option
  -> t
  -> t AccessResult.t SatUnsat.t
(** performs a call to a function with no summary by optimistically havoc'ing the by-ref actuals and
    the return value as appropriate *)
