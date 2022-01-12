(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport
open PulseBasicInterface
open PulseOperations.Import

val load_field :
     PulsePathContext.t
  -> Fieldname.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> PulseAbductiveDomain.t
  -> ( PulseAbductiveDomain.t
       * (AbstractValue.t * ValueHistory.t)
       * (AbstractValue.t * ValueHistory.t)
     , PulseAbductiveDomain.t base_error )
     pulse_result

val matchers : matcher list
