(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module CLOpt = CommandLineOption

type data = {name: string; command_doc: CLOpt.command_doc}

val inferconfig_env_var : string

val inferconfig_file : string

val infer : CLOpt.command_doc

val data_of_command : CLOpt.command -> data
