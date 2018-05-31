(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let rec gen_sequence_list ~f =
  let open Sequence.Generator in
  function [] -> return () | x :: tl -> f x >>= fun () -> gen_sequence_list ~f tl
