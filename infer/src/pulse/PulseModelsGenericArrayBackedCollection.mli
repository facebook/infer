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
open PulseOperationResult.Import

val matchers : matcher list

val eval :
     PathContext.t
  -> access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

val access : Access.t

val field : Fieldname.t

val size_field : Fieldname.t

val element :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

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
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

val size : AbstractValue.t * ValueHistory.t -> desc:string -> model_no_non_disj

val increase_size :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> desc:string
  -> AbductiveDomain.t
  -> AbductiveDomain.t PulseOperationResult.t

val decrease_size :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> desc:string
  -> AbductiveDomain.t
  -> AbductiveDomain.t PulseOperationResult.t

val empty : AbstractValue.t * ValueHistory.t -> desc:string -> model_no_non_disj

val default_constructor : AbstractValue.t * ValueHistory.t -> desc:string -> model_no_non_disj

val to_internal_size_deref :
     PathContext.t
  -> access_mode
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) AccessResult.t

val assign_size :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbstractValue.t * ValueHistory.t
  -> desc:string
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val assign_size_constant :
     PathContext.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> constant:IntLit.t
  -> desc:string
  -> AbductiveDomain.t
  -> AbductiveDomain.t PulseOperationResult.t

module Iterator : sig
  val internal_pointer : Fieldname.t

  val to_internal_pointer :
       PathContext.t
    -> access_mode
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
    -> AbductiveDomain.t AccessResult.t

  val point_into :
       PathContext.t
    -> Location.t
    -> ValueHistory.event
    -> collection:AbstractValue.t * ValueHistory.t
    -> iter:AbstractValue.t * ValueHistory.t
    -> ?index:AbstractValue.t
    -> AbductiveDomain.t
    -> AbductiveDomain.t AccessResult.t
  (** make [iter] point to the element at [index], or at an unknown position if not given, in the
      backing array of [collection], which is either a collection or an iterator into it *)
end
