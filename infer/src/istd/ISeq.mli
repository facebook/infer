(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val hash_fold_t :
     (Base_internalhash_types.state -> 'a -> Base_internalhash_types.state)
  -> Base_internalhash_types.state
  -> 'a Seq.t
  -> Base_internalhash_types.state
