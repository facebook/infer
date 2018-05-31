(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val from_arguments : string -> unit
(** loads the source files from command line arguments and translates them *)

val from_verbose_out : string -> unit
(** loads the source files from javac's verbose output translates them *)
