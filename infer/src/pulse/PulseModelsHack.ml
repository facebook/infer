(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [-"$builtins" &:: "hack_new_dict" <>$ any_arg $+...$--> Basic.skip]
