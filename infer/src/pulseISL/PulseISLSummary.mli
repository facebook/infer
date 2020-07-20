(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseISLDomainInterface

type t = ExecutionDomain.t list

val of_posts : Procdesc.t -> ExecutionDomain.t list -> t

val pp : Format.formatter -> t -> unit
