(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0
include module type of Map_intf
module Make (Key : OrderedType) : S with type key = Key.t
