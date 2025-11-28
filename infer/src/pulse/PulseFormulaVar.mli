(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include
  module type of PulseAbstractValue
    with type t = PulseAbstractValue.t
     and module Set = PulseAbstractValue.Set
     and module Map = PulseAbstractValue.Map

val is_simpler_than : t -> t -> bool

val is_simpler_or_equal : t -> t -> bool
