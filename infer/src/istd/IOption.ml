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

let exists2 x y ~f = match (x, y) with Some x, Some y -> f x y | _, _ -> false

let map_changed opt ~equal ~f =
  match opt with
  | None ->
      opt
  | Some x ->
      let x' = f x in
      if equal x x' then opt else Some x'


module Let_syntax = struct
  include Option.Let_syntax

  let ( let+ ) x f = Option.map ~f x

  let ( and+ ) x y = Option.both x y

  let ( let* ) x f = Option.bind ~f x

  let ( and* ) x y = Option.both x y
end
