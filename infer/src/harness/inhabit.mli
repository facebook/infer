(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Generate a procedure that calls a given sequence of methods. Useful for harness/test generation. *)

type lifecycle_trace = (Procname.t * Sil.typ option) list

(** create a procedure named harness_name that calls each of the methods in trace add it to the
    cg/cfg *)
val inhabit_trace : lifecycle_trace -> Procname.java -> Cg.t -> Cfg.cfg -> unit

