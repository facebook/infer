(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CLOpt = CommandLineOption

type data = {name: string; command_doc: CLOpt.command_doc}

val inferconfig_env_var : string

val inferconfig_file : string

val infer : CLOpt.command_doc

val data_of_command : InferCommand.t -> data
