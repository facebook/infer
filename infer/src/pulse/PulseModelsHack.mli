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
     model_desc:string
  -> PulsePathContext.t
  -> Location.t
  -> Typ.name
  -> PulseAbductiveDomain.t
  -> PulseBaseStack.value * PulseAbductiveDomain.t

val get_static_companion_var : Typ.name -> Pvar.t
