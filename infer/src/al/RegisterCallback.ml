(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let register_frontend_checks () =
  if Config.is_checker_enabled Linters then Capture.al_callback_ref := AL.do_frontend_checks
