(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let repo = ref None

let initialize storage =
  match !repo with
  | None ->
      repo := Some storage
  | Some _ ->
      Logging.die Logging.InternalError "Attempt to initialize global 3rd party repository twice"


let get_repo () =
  match !repo with
  | None ->
      Logging.die Logging.InternalError "Attempt to access not initialized 3rd party repository"
  | Some repo ->
      repo
