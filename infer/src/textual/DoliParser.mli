(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : string -> unit
(** [capture path] parses the file in [path] accordning to the doli syntax. If parsing was
    successful, then it stores the generated cfg into the database (`DB`), and in `Tenv`. If parsing
    was not successful, then it prints an error message, and the location of the error *)

val matcher : Procname.t -> Procname.t option
(** [matcher procname] searches in its internal data structure for a doli-model that matches
    [procname]. If it finds one, then it returns the internal name of that doli-model *)
