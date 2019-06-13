(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module ModifiedParamIndices = AbstractDomain.FiniteSet (Int)
module Domain = AbstractDomain.TopLifted (ModifiedParamIndices)
include Domain

let pure = AbstractDomain.Types.NonTop ModifiedParamIndices.empty

let impure_global = AbstractDomain.Types.Top

let is_pure astate =
  match astate with
  | AbstractDomain.Types.Top ->
      false
  | AbstractDomain.Types.NonTop modified_params ->
      ModifiedParamIndices.is_empty modified_params


let impure_params modified_params = AbstractDomain.Types.NonTop modified_params

let all_params_modified args =
  List.foldi ~init:ModifiedParamIndices.empty
    ~f:(fun i acc _ -> ModifiedParamIndices.add i acc)
    args


type summary = Domain.t

let pp_summary fmt astate = F.fprintf fmt "@\n Purity summary: %a @\n" Domain.pp astate
