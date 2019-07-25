(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t = PulseAbductiveDomain.PrePost.t list

val of_posts : Procdesc.t -> PulseAbductiveDomain.t list -> t

val pp : Format.formatter -> t -> unit
