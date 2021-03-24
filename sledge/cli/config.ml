(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Configuration options from config file *)

let config_file_env_var = "SLEDGE_CONFIG"
let exe_relative_config_file_path = "config"

let config_file =
  match Sys.getenv config_file_env_var with
  | Some file -> file
  | None ->
      Filename.concat
        (Filename.dirname Sys.executable_name)
        exe_relative_config_file_path

let contents =
  try Yojson.Basic.from_file config_file
  with Sys_error _ ->
    warn
      "could not read config file %s@\n\
       The path to the config file can be overridden by the %s environment \
       variable."
      config_file config_file_env_var () ;
    `Assoc []

module YBU = Yojson.Basic.Util

let find key =
  try YBU.to_string_option (YBU.member key contents)
  with YBU.Type_error _ -> None

let find_list key =
  try YBU.filter_string (YBU.to_list (YBU.member key contents))
  with YBU.Type_error _ -> []

let find_exn key =
  match find key with
  | Some data -> data
  | None -> fail "%s not specified in config file %s" key config_file ()
