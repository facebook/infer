(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open AbstractDomain.Types
module F = Format
module ModifiedParamIndices = AbstractDomain.FiniteSet (Int)
module Domain = AbstractDomain.TopLifted (ModifiedParamIndices)
include Domain

let pure = NonTop ModifiedParamIndices.empty

let impure_global = Top

let is_pure astate =
  match astate with
  | Top ->
      false
  | NonTop modified_params ->
      ModifiedParamIndices.is_empty modified_params


let impure_params modified_params = NonTop modified_params

let all_params_modified args =
  List.foldi ~init:ModifiedParamIndices.empty
    ~f:(fun i acc _ -> ModifiedParamIndices.add i acc)
    args


type summary = Domain.t

let pp_summary fmt astate = F.fprintf fmt "@\n Purity summary: %a @\n" Domain.pp astate
