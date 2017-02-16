(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** loads the source files from command line arguments and translates them *)
val from_arguments : string -> unit

(** loads the source files from javac's verbose output translates them *)
val from_verbose_out : string -> unit
