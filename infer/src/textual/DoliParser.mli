(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run : string -> unit
(** [run path] parses the file in [path] accordning to the doli syntax. In the end, it discards the
    generated AST It prints a message indicating whether parsing was succesful, and if not, also
    gives the location of the error. *)

val matcher : Procname.t -> Procname.t option
(** [matcher procname] search for a model that matches [procname]. If it finds one, it returns the
    internal name of the model *)
