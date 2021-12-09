(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BaseMemory = PulseBaseMemory

type copy_spec_t = Copied of {location: Location.t; heap: BaseMemory.t} | Modified

include AbstractDomain.WithBottom

val add : source_opt:PulseAbstractValue.t option -> Var.t -> copy_spec_t -> t -> t

val mark_copy_as_modified : is_modified:(BaseMemory.t -> bool) -> Var.t -> t -> t

val get_copied : t -> (Var.t * Location.t) list

val find_copy : source:PulseAbstractValue.t -> t -> Var.t option
