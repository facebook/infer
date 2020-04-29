(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Join and Meet Operators *)

(** {2 Join Operators} *)

val pathset_join :
     BiabductionSummary.t InterproceduralAnalysis.t
  -> Paths.PathSet.t
  -> Paths.PathSet.t
  -> Paths.PathSet.t * Paths.PathSet.t
(** Join two pathsets *)

val proplist_collapse_pre :
  Tenv.t -> Prop.normal Prop.t list -> Prop.normal BiabductionSummary.Jprop.t list

(** {2 Meet Operators} *)

val propset_meet_generate_pre : Tenv.t -> Propset.t -> Prop.normal Prop.t list
(** [propset_meet_generate_pre] generates new symbolic heaps (i.e., props) by applying the partial
    meet operator, adds the generated heaps to the argument propset, and returns the resulting
    propset. This function is tuned for combining preconditions. *)
