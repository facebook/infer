(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* NOTE: using [Var] for [AbstractValue] here since this is how "abstract values" are interpreted,
   in particular as far as arithmetic is concerned *)

include PulseAbstractValue

let is_simpler_than v1 v2 = PulseAbstractValue.compare v1 v2 < 0

let is_simpler_or_equal v1 v2 = PulseAbstractValue.compare v1 v2 <= 0
