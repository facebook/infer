(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let prng = Random.State.make_self_init ~allow_in_tests:true ()

let random_bits () = Random.State.bits prng

let retry ?(in_dir = Filename.temp_dir_name) ~f prefix suffix =
  let escape s = String.map s ~f:(function '/' | '\'' | '\000' | '\n' | '-' -> '_' | c -> c) in
  let prefix = escape prefix in
  let suffix = escape suffix in
  let rec try_name counter =
    let name =
      let rnd = random_bits () land 0xFF_FFFF in
      Printf.sprintf "%s.tmp.%06x%s" prefix rnd suffix
    in
    let name = Filename.concat in_dir name in
    try f name
    with (Sys_error _ | Unix.Unix_error _) as e ->
      if Int.(counter >= 1000) then raise e else try_name (counter + 1)
  in
  try_name 0


let temp_dir ?(perm = 0o700) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix ~f:(fun name ->
      Unix.mkdir name ~perm ;
      name )


let open_temp_file ?(perm = 0o600) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix ~f:(fun name ->
      (name, Out_channel.create ~perm ~fail_if_exists:true name) )


let temp_file ?perm ?in_dir prefix suffix =
  let name, oc = open_temp_file ?perm ?in_dir prefix suffix in
  Out_channel.close oc ;
  name
