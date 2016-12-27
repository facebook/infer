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

(** Various preanalysis passes for transforming the IR in useful ways *)

(** perform liveness analysis and insert Nullify/Remove_temps instructions into the IR to make it
    easy for analyses to do abstract garbage collection *)
val do_liveness : Procdesc.t -> Tenv.t -> unit

(** add Abstract instructions into the IR to give hints about when abstraction should be
    performed *)
val do_abstraction : Procdesc.t -> unit

(** add possible dynamic dispatch targets to the call_flags of each call site *)
val do_dynamic_dispatch : Procdesc.t -> Cg.t -> Tenv.t -> unit
