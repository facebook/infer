(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of IterLabels

module Import : sig
  type 'a iter = 'a t
end

val pop : 'a iter -> ('a * 'a iter) option
