(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let hash_fold_t hash_fold_x hash seq =
  let limit = 10 in
  let exception LimitReached of Base_internalhash_types.state in
  try
    Seq.fold_lefti
      (fun hash i x ->
        if i >= limit then raise (LimitReached hash) ;
        hash_fold_x hash x )
      hash seq
  with LimitReached hash -> hash
