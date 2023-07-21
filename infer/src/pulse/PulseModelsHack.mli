(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* open PulseBasicInterface *)
open PulseModelsImport

val matchers : matcher list

val get_static_companion :
  Typ.name -> PulseAbductiveDomain.t -> PulseAbstractValue.t * PulseAbductiveDomain.t
