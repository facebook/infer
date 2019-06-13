(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let p = Printf.sprintf

let topl_property = "topl.Property"

let transition = p "transition%d"

let arg = p "arg%d"

let retval = "retval"

let saved_arg = p "savedArg%d"

let reg = p "reg_%s"

let state = "state"

let maybe = "maybe"

let execute = "execute"

let execute_state = p "execute_state_%d"

let save_args = "saveArgs"
