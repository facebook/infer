(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Implementation of Abstraction Functions *)

val abstract :
  BiabductionSummary.t InterproceduralAnalysis.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Abstract a proposition. *)

val abstract_spec :
     BiabductionSummary.t InterproceduralAnalysis.t
  -> Prop.normal BiabductionSummary.spec
  -> BiabductionSummary.NormSpec.t
(** Normalizes names and applies simplifications, soem of which require looking at both pre and
    post. *)

val abstract_junk :
  BiabductionSummary.t InterproceduralAnalysis.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Check whether the prop contains junk. If it does, and [Config.allowleak] is true, remove the
    junk, otherwise raise a Leak exception. *)

val abstract_no_symop :
  BiabductionSummary.t InterproceduralAnalysis.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Abstract a proposition but don't pay a SymOp *)

val lifted_abstract : BiabductionSummary.t InterproceduralAnalysis.t -> Propset.t -> Propset.t
(** Abstract each proposition in [propset] *)

val remove_redundant_array_elements :
  BiabductionSummary.t InterproceduralAnalysis.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Remove redundant elements in an array, and check for junk afterwards *)
