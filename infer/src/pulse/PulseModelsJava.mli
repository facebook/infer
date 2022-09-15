(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport
open PulseBasicInterface
open PulseDomainInterface

val load_field :
     PathContext.t
  -> Fieldname.t
  -> Location.t
  -> AbstractValue.t * ValueHistory.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t) * (AbstractValue.t * ValueHistory.t))
     AccessResult.t

val matchers : matcher list
