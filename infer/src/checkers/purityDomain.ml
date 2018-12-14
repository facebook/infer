(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module ModifiedParamIndices = AbstractDomain.FiniteSet (Int)
module Domain = AbstractDomain.BottomLifted (ModifiedParamIndices)
include Domain

let global = -1

let contains_global modified_params = ModifiedParamIndices.mem global modified_params

let pure = AbstractDomain.Types.Bottom

let is_pure = Domain.is_empty

let impure modified_args = AbstractDomain.Types.NonBottom modified_args

let with_purity is_pure modified_args =
  if is_pure then AbstractDomain.Types.Bottom else impure modified_args


let all_params_modified args =
  List.foldi ~init:ModifiedParamIndices.empty
    ~f:(fun i acc _ -> ModifiedParamIndices.add i acc)
    args


let get_modified_params astate =
  match astate with
  | AbstractDomain.Types.NonBottom modified_args ->
      Some modified_args
  | AbstractDomain.Types.Bottom ->
      None


type summary = Domain.t

let pp_summary fmt astate = F.fprintf fmt "@\n Purity summary: %a @\n" Domain.pp astate
