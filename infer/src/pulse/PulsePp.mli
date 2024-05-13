(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

val pp : Pp.print_kind -> PulsePathContext.t option -> F.formatter -> PulseAbductiveDomain.t -> unit
