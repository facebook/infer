(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

(** {1 Models for "Load" instructions, for now just reading from variables with particular names} *)

type model_data = {path: PathContext.t; location: Location.t}

type model =
     model_data
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)) PulseOperationResult.t

val dispatch : load:Exp.t -> model option
