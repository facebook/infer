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
module DSL = PulseModelsDSL

let alloc size : model_no_non_disj =
 fun model_data astate ->
  let<++> astate =
    Basic.alloc_not_null ~initialize:false ~desc:"alloc" SwiftAlloc (Some size) model_data astate
  in
  astate


let unknown () : unit DSL.model_monad =
  let open DSL.Syntax in
  let* res = fresh () in
  assign_ret res


let builtins_matcher builtin _args : unit -> unit PulseModelsDSL.model_monad =
  let _builtin_s = SwiftProcname.show_builtin builtin in
  match (builtin : SwiftProcname.builtin) with
  | NonDet ->
      unknown
  | InitTuple ->
      unknown
  | DynamicCall ->
      unknown


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [+BuiltinDecl.(match_builtin __swift_alloc) <>$ capt_exp $--> alloc]
  |> List.map ~f:(fun matcher ->
         matcher
         |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
         |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
