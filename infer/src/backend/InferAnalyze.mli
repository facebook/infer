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

(** Main module for the analysis after the capture phase *)

val main : changed_files:SourceFile.Set.t option -> unit
(** Given a name of the Makefile to use for multicore analysis, analyze the captured code *)
