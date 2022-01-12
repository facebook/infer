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
  val at : desc:string -> AbstractValue.t * ValueHistory.t -> AbstractValue.t * 'a -> model

  val invalidate_references :
    Invalidation.std_vector_function -> AbstractValue.t * ValueHistory.t -> model

  val push_back : AbstractValue.t * ValueHistory.t -> model

  val reserve : AbstractValue.t * ValueHistory.t -> model
end
