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
  match Core.Sys.getenv config_file_env_var with
  | Some file -> file
  | None ->
      Filename.concat
        (Filename.dirname Caml.Sys.executable_name)
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

let find key = Yojson.Basic.Util.(to_string_option (member key contents))

let find_list key =
  Yojson.Basic.Util.(filter_string (to_list (member key contents)))

let find_exn key =
  match find key with
  | Some data -> data
  | None -> fail "%s not specified in config file %s" key config_file ()
