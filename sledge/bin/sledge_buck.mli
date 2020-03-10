(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val main :
     command:unit Command.basic_command
  -> analyze:(string list -> unit -> unit) Command.Param.t
  -> Command.t
