(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let dir_names = ref []

let get_registered_dir_names () = !dir_names

let register_dir_name name =
  if not (List.exists !dir_names ~f:(String.equal name)) then dir_names := name :: !dir_names
