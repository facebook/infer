(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let find_value_exn = function None -> raise Caml.Not_found | Some v -> v

let value_default_f ~f = function None -> f () | Some v -> v

let if_none_evalopt ~f x = match x with None -> f () | Some _ -> x

let if_none_eval = value_default_f
