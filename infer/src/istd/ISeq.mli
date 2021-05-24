(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val fold_result :
  'a Seq.t -> init:'accum -> f:('accum -> 'a -> ('accum, 'e) result) -> ('accum, 'e) result
