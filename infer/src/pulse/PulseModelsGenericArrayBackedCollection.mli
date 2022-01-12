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
open PulseOperations.Import

val matchers : matcher list

val eval :
     PathContext.t
  -> PulseOperations.access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

val access : (Fieldname.t, 'a) HilExp.Access.t_

val field : Fieldname.t

val element :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> ( AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)
     , AbductiveDomain.t base_error )
     pulse_result

val eval_element :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

val eval_pointer_to_last_element :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> ( AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)
     , AbductiveDomain.t base_error )
     pulse_result

val eval_is_empty :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

module Iterator : sig
  val internal_pointer : Fieldname.t

  val to_internal_pointer :
       PathContext.t
    -> PulseOperations.access_mode
    -> Location.t
    -> AbstractValue.t * ValueHistory.t
    -> AbductiveDomain.t
    -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

  val construct :
       PathContext.t
    -> Location.t
    -> ValueHistory.event
    -> init:AbstractValue.t * ValueHistory.t
    -> ref:AbstractValue.t * ValueHistory.t
    -> AbductiveDomain.t
    -> (AbductiveDomain.t, AbductiveDomain.t base_error) pulse_result
end
