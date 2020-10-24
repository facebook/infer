(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

val main :
     command:Report.status Command.basic_command
  -> analyze:(string list -> unit -> Report.status) Command.Param.t
  -> Command.t
