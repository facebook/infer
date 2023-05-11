(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val do_preanalysis : Tenv.t -> Procdesc.t -> unit
(** Various preanalysis passes for transforming the IR in useful ways *)
