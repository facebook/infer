(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility functions for sequences *)

val gen_sequence_list :
  f:('a -> (unit, 'b) Sequence.Generator.t) -> 'a list -> (unit, 'b) Sequence.Generator.t
