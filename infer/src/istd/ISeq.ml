(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let fold_result seq ~init ~f =
  Seq.fold_left (fun acc_result x -> Result.bind acc_result ~f:(fun acc -> f acc x)) (Ok init) seq
