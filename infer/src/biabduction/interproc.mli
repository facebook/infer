(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Interprocedural Analysis *)

val analyze_procedure : Callbacks.proc_callback_t
(** Run the biabduction analysis on the given procedure *)
