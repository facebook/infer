(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val do_preanalysis : Procdesc.t -> Tenv.t -> unit
(** Various preanalysis passes for transforming the IR in useful ways *)
