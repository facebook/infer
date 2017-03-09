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

(** Join and Meet Operators *)

(** {2 Join Operators} *)

(** Join two pathsets *)
val pathset_join :
  Typ.Procname.t -> Tenv.t -> Paths.PathSet.t -> Paths.PathSet.t -> Paths.PathSet.t * Paths.PathSet.t

val join_time : float ref

val proplist_collapse_pre : Tenv.t -> Prop.normal Prop.t list -> Prop.normal Specs.Jprop.t list

val pathset_collapse : Tenv.t -> Paths.PathSet.t -> Paths.PathSet.t

(** reduce the pathset only based on implication checking. *)
val pathset_collapse_impl : Typ.Procname.t -> Tenv.t -> Paths.PathSet.t -> Paths.PathSet.t

(** {2 Meet Operators} *)

(** [propset_meet_generate_pre] generates new symbolic heaps (i.e., props)
    by applying the partial meet operator, adds the generated heaps
    to the argument propset, and returns the resulting propset. This function
    is tuned for combining preconditions. *)
val propset_meet_generate_pre : Tenv.t -> Propset.t -> Prop.normal Prop.t list
