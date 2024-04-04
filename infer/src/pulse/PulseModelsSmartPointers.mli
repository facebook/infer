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

module SharedPtr : sig
  val assign_count :
       PathContext.t
    -> Location.t
    -> AbstractValue.t * ValueHistory.t
    -> constant:IntLit.t
    -> desc:string
    -> AbductiveDomain.t
    -> AbductiveDomain.t PulseOperationResult.t
end
