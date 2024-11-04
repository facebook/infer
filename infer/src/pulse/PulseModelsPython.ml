(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseModelsImport
module DSL = PulseModelsDSL

let dict_tname = TextualSil.python_dict_type_name

let make_dictionnary _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let open DSL.Syntax in
  let* dict = constructor dict_tname [] in
  assign_ret dict


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [-"$builtins" &:: "py_make_dictionnary" &::.*+++> make_dictionnary]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
