(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** this is a naive/temporary implementation in a near diff, we will 1) parse the source file to
    collect location datas for all class names 2) cache the result for later uses but we may have to
    adapt a litle some signatures *)
let class_name_location file _cn : Location.t = {line= 0; col= 0; file}
