(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

type t = ExecutionDomain.summary list [@@deriving yojson_of]

val of_posts : Tenv.t -> Procdesc.t -> Errlog.t -> Location.t -> ExecutionDomain.t list -> t

val force_exit_program :
     Tenv.t
  -> Procdesc.t
  -> Errlog.t
  -> Location.t
  -> ExecutionDomain.t
  -> _ ExecutionDomain.base_t option

val pp : Format.formatter -> t -> unit
