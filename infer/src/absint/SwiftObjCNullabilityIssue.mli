(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val message : Procname.t -> string
(** Standard report message for MISSING_NULLABILITY_ANNOTATION. The message names the offending
    Objective-C method and explains why the missing annotation matters for Swift interop, so that
    static and Pulse-based reports stay consistent. *)

val should_report_at : Location.t -> bool
(** Whether a MISSING_NULLABILITY_ANNOTATION report at this location should be surfaced to the user.
    Returns [false] for bogus locations (no real line number) that the Swift frontend produces for
    some synthesized calls. *)
