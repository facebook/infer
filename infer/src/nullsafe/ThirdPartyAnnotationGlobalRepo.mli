(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A singleton. Should be initialized once prior to start of nullsafe. *)

val initialize : ThirdPartyAnnotationInfo.storage -> unit
(** Should be initialized exactly once before access. *)

val get_repo : unit -> ThirdPartyAnnotationInfo.storage
(** Can be accessed only when initialization was done *)
