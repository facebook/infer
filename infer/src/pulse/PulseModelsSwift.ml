(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseOperationResult.Import
open PulseModelsImport

let alloc size : model_no_non_disj =
 fun model_data astate ->
  let<++> astate =
    Basic.alloc_not_null ~initialize:false ~desc:"alloc" SwiftAlloc (Some size) model_data astate
  in
  astate


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [+BuiltinDecl.(match_builtin __swift_alloc) <>$ capt_exp $--> alloc]
  |> List.map ~f:(fun matcher ->
         matcher
         |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
         |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
