(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val extract : files:SourceFile.Set.t -> input_capture_path:string -> unit
(** Using the given capture DB as input, copy the given sources files and their procedures into the
    main capture DB in the results directory, which should be empty. *)

val complete : input_capture_path:string -> bool
(** Using the given capture DB as input, complete the current main capture DB by copying procedures
    and files listed as missing dependencies in the current results directory. See
    [MissingDependencies] module. Returns whether any changes were made. Deletes the missing
    dependency files after completion. *)
