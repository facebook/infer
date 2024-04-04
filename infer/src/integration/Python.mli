(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** [Bytecode] is mostly used for debugging purpose: it takes a single bytecode .pyc file as input
    and captures it. It is used with [--dump-textual] to quickly check the result of the Python ->
    Textual translation.

    [Files] is a list of Python source files, which are then captured and analyzed. This is the main
    way of running infer on Python code. *)
type kind = Bytecode of {files: string list} | Files of {prog: string; args: string list}

val capture : kind -> unit
