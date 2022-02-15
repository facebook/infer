(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** {1 Simple devirtualization pre-analysis using a flow-sensitive tracking of dynamic classes} *)

open! IStd

val process : Procdesc.t -> Tenv.t -> unit
(** Run the devirtualization pass by replacing some virtual calls by resolved calls *)
