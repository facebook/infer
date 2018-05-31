(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run_clang : ClangCommand.t -> (In_channel.t -> 'a) -> 'a

val capture : ClangCommand.t -> unit
