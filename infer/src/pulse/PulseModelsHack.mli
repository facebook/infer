(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseModelsImport

val matchers : matcher list

val get_static_companion :
     model_desc:string
  -> PathContext.t
  -> Location.t
  -> Typ.name
  -> AbductiveDomain.t
  -> (AbstractValue.t * ValueHistory.t) * AbductiveDomain.t

val get_static_companion_var : Typ.name -> Pvar.t

val make_new_awaitable : PulseModelsDSL.aval -> PulseModelsDSL.aval PulseModelsDSL.model_monad

val build_vec_for_variadic_callee :
     model_data
  -> (AbstractValue.t * ValueHistory.t) list
  -> AbductiveDomain.t
  -> ((AbstractValue.t * ValueHistory.t) * AbductiveDomain.t) sat_unsat_t
