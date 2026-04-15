(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val capture : files:string list -> unit
(** Capture C source files using the bundled tree-sitter C grammar. Only C (.c, .h) files are
    currently supported; other extensions will produce an error. *)
