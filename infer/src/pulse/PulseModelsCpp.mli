(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport
open PulseBasicInterface

val abort_matchers : matcher list

val matchers : matcher list

module Vector : sig
  val at :
    desc:string -> AbstractValue.t * ValueHistory.t -> AbstractValue.t * 'a -> model_no_non_disj

  val invalidate_references :
    Invalidation.std_vector_function -> AbstractValue.t * ValueHistory.t -> model_no_non_disj

  val push_back : AbstractValue.t * ValueHistory.t -> desc:string -> model_no_non_disj
end

module Function : sig
  val operator_call :
       (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t
    -> (AbstractValue.t * ValueHistory.t) ProcnameDispatcher.Call.FuncArg.t list
    -> model
end
