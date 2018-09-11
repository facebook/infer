(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {2 Global variables} *)

val footprint : bool ref

val run_in_footprint_mode : ('a -> 'b) -> 'a -> 'b
(** Call f x with footprint set to true.
    Restore the initial value of footprint also in case of exception. *)

val run_in_re_execution_mode : ('a -> 'b) -> 'a -> 'b
(** Call f x with footprint set to false.
    Restore the initial value of footprint also in case of exception. *)

(** {2 Global variables with initial values specified by command-line options} *)

val abs_val : int ref

val reset_abs_val : unit -> unit

val run_with_abs_val_equal_zero : ('a -> 'b) -> 'a -> 'b
(** Call f x with abs_val set to zero.
    Restore the initial value also in case of exception. *)

val allow_leak : bool ref
