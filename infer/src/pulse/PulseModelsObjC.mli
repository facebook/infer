(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseModelsImport

val matchers : matcher list

val transfer_ownership_matchers : matcher list

val object_at :
     AbstractValue.t * ValueHistory.t
  -> AbstractValue.t * 'a
  -> ?implement_nil_messaging:bool
  -> desc:string
  -> model_no_non_disj

val insert_object_at :
     AbstractValue.t * ValueHistory.t
  -> AbstractValue.t * ValueHistory.t
  -> AbstractValue.t * 'a
  -> ?disallow_nil_obj:bool
  -> desc:string
  -> model_no_non_disj

val create_array_backed_with_modelled_array :
  AbstractValue.t * ValueHistory.t -> desc:string -> model_no_non_disj

val init_array_backed_with_array :
     AbstractValue.t * ValueHistory.t
  -> AbstractValue.t * ValueHistory.t
  -> desc:string
  -> model_no_non_disj
